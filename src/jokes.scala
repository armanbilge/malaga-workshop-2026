import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.dom.*
import io.circe.*
import io.circe.parser.decode
import org.http4s.{EntityDecoder, circe}
import org.http4s.dom.FetchClientBuilder

case class Joke(id: Int, setup: String, punchline: String) derives Decoder
object Joke:
  given EntityDecoder[IO, List[Joke]] = circe.jsonOf
val client = FetchClientBuilder[IO].create

def getJokesNotWorking(n: Int): IO[List[Joke]] =
  client.expect[List[Joke]](s"https://official-joke-api.appspot.com/jokes/random/$n").debug("")

def getJokes(n: Int): IO[List[Joke]] =
  IO.fromPromise(IO(org.scalajs.dom.fetch(s"https://official-joke-api.appspot.com/jokes/random/$n")))
    .flatMap(response => IO.fromPromise(IO(response.text())))
    .flatMap(body => IO.fromEither(decode[List[Joke]](body)))

def Jokes(label: String, initialStep: Int): Resource[IO, HtmlDivElement[IO]] =
  (SignallingRef[IO].of(initialStep), SignallingRef[IO].of(List.empty[Joke]), SignallingRef[IO].of(""))
    .tupled.toResource.flatMap { (step, jokes, status) =>

      val allowedSteps = List(1, 2, 3, 5, 10)

      val fetchJokes: IO[Unit] =
        status.set("loading ...") *>
          IO.println("buggy") *>
          step.get
            .flatMap(getJokes(_).attempt)
            .flatMap {
              case Right(newJokes) => jokes.update(_ ++ newJokes) *> status.set("")
              case Left(_) => status.set("Request failed :(")
            }

      div(
        p(
          "How many jokes to add? ",
          select.withSelf { self =>
            (
              allowedSteps.map(step => option(value := step.toString, step.toString)),
              value <-- step.map(_.toString),
              onChange --> {
                _.evalMap(_ => self.value.get).map(_.toIntOption).unNone.foreach(step.set)
              }
            )
          }
        ),
        p(
          label,
          br(()),
          " ",
          button(
            "-",
            onClick --> (_.evalMap(_ => step.get).foreach(n => jokes.update(_.dropRight(n))))
          ),
          button(
            "+",
            onClick --> (_.foreach(_ => fetchJokes))
          ),
          " ",
          span(status)
        ),
        p("Jokes List:", " (Total: ", b(jokes.map(_.size.toString)), ")"),
        ul(children <-- jokes.map(_.map(joke => li(b(joke.setup), " ", i(joke.punchline)))))
      )
    }

def jokes = Jokes("More or less jokes?", initialStep = 1)
