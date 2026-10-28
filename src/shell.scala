import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import cats.effect.std.Random

def w2 =
  Random.scalaUtilRandom[IO].toResource.flatMap { random =>
    random.nextIntBounded(3).toResource.flatMap { winner =>
      SignallingRef[IO].of(winner).toResource.flatMap { ball =>
        SignallingRef[IO].of(None: Option[Boolean]).toResource.flatMap { result =>
          def choose(glass: Int) =
            ball.get.flatMap(hidden => result.set(Some(hidden == glass)))

          val colors = List("lightcoral", "lightblue", "lightgreen")

          div(
            p("Pick a glass:"),
            div(
              (0 until 3).toList.map { glass =>
                button(
                  styleAttr := s"background-color: ${colors(glass)}; min-width: 100px",
                  onClick(choose(glass)),
                  s"Glass ${glass + 1}"
                )
              }
            ),
            p(result.map {
              case Some(true)  => "You won!"
              case Some(false) => "You lost!"
              case None        => ""
            }),
            button(
              onClick(random.nextIntBounded(3).flatMap { newWinner =>
                ball.set(newWinner) *> result.set(None)
              }),
              "Reset"
            )
          )
        }
      }
    }
  }
