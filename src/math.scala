import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.dom.*
import cats.Show
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import cats.effect.std.Random
import scala.concurrent.duration.{span => _, *}
import fs2.concurrent.Signal

enum Expr:
  case Num(value: Int)
  case Add(x: Expr, y: Expr)
  case Minus(x: Expr, y: Expr)

object Expr:
  given Show[Expr] = Show.show: expr =>
    expr match
      case Num(value)  => value.toString()
      case Add(x, y)   => s"${(x.show)} + ${y.show}"
      case Minus(x, y) => s"${x.show} - ${y.show}"

  extension (expr: Expr)
    def evaluate: Int =
      expr match
        case Num(x)      => x
        case Add(x, y)   => x.evaluate + y.evaluate
        case Minus(x, y) => x.evaluate - y.evaluate

import Expr.*
val ex1 = Minus(Num(2), Num(1))

def showResult(expr: Expr, guess: String): String =
  if guess.toIntOption.fold(false)(_ == expr.evaluate) then "You're awesome"
  else "Please do it again"

def mkExpr: IO[Expr] =
  for
    random <- Random.scalaUtilRandom[IO]
    x      <- random.betweenInt(1, 2026)
    y      <- random.betweenInt(1, 2026)
    b      <- random.nextBoolean
    op = if b then Add.apply else Minus.apply
  yield op(Num(x), Num(y))

def mkProblems = mkExpr >>= SignallingRef[IO].of

val quiz: Resource[IO, HtmlDivElement[IO]] =
  (
    SignallingRef[IO].of("").toResource,
    mkProblems.toResource
  )
    .parFlatMapN { (guess, prolem) =>
      prolem.get.toResource.flatMap { expr =>
        div(
          label(s"Do your math: ", prolem.map(_.show)),
          input.withSelf { self =>
            (
              placeholder := "Enter your result here",
              onInput --> (_.foreach(_ => self.value.get >>= guess.set))
            )
          },
          span(" ", (prolem: Signal[IO, Expr]).map2(guess)(showResult)),
          button("try again", onClick(mkExpr >>= prolem.set))
        )
      }
    }
