import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.dom.*
import fs2.*
import fs2.concurrent.*
import cats.syntax.all.*

def counterSuffix(num: Int): String =
  val lastDigit = num % 10
  lastDigit match
    case 1 => "st"
    case 2 => "nd"
    case 3 => "rd"
    case _ => "th"

def fibonacci = (SignallingRef[IO].of(1), SignallingRef[IO].of(0), SignallingRef[IO].of(1)).tupled.toResource.flatMap {
  (count, f0, f1) =>
    div(
      p(
        "The ",
        count.map(_.toString),
        count.map(counterSuffix),
        " Fibonacci number is: ",
        b(f1.map(_.toString)),
        " ",
        button(
          "Next one!",
          onClick --> {
            _.evalMap(_ => (f0.get, f1.get).mapN((f0v, f1v) => (f1v, f0v + f1v))).foreach((f1v, next) =>
              f0.set(f1v) *> f1.set(next) *> count.update(_ + 1)
            )
          }
        )
      )
    )
}
