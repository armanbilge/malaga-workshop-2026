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

case class Joke(id: Int, `type`: String, setup: String, punchline: String) derives Decoder
object Joke:
  given EntityDecoder[IO, List[Joke]] = circe.jsonOf
val client = FetchClientBuilder[IO].create

val jokeTypes = List("general", "knock-knock", "programming", "dad")

def jokeUrl(jokeType: String) = s"https://official-joke-api.appspot.com/jokes/$jokeType/random"

def getJokesNotWorking(jokeType: String): IO[List[Joke]] =
  client.expect[List[Joke]](jokeUrl(jokeType)).debug("")

def getJokes(jokeType: String): IO[List[Joke]] =
  IO.fromPromise(IO(org.scalajs.dom.fetch(jokeUrl(jokeType))))
    .flatMap(response => IO.fromPromise(IO(response.text())))
    .flatMap(body => IO.fromEither(decode[List[Joke]](body)))

def Jokes(intro: String, initialType: String): Resource[IO, HtmlDivElement[IO]] =
  (
    SignallingRef[IO].of(initialType),
    SignallingRef[IO].of(List.empty[Joke]),
    SignallingRef[IO].of("")
  ).tupled.toResource.flatMap { (jokeType, jokes, status) =>
    val fetchJoke: IO[Unit] =
      status.set("Loading...") *>
        jokeType.get
          .flatMap(getJokes(_).attempt)
          .flatMap {
            case Right(newJokes) => jokes.update(_ ++ newJokes) *> status.set("")
            case Left(_)         => status.set("Request failed :(")
          }

    def removeJoke(index: Int): IO[Unit] = jokes.update(_.patch(index, Nil, 1))

    div(
      cls := "jokes",
      styleTag(jokesCss),
      p(intro),
      div(
        cls := "controls",
        label("Joke type "),
        select.withSelf { self =>
          (
            jokeTypes.map(t => option(value := t, t)),
            value <-- jokeType,
            onChange --> {
              _.evalMap(_ => self.value.get).foreach(jokeType.set)
            }
          )
        },
        button("Add joke", onClick(fetchJoke)),
        span(cls := "status", status)
      ),
      h4("Jokes ", span(cls := "count", jokes.map(_.size.toString))),
      ul(
        cls := "list",
        children <-- jokes.map(_.zipWithIndex.map { (joke, index) =>
          li(
            span(cls := "tag", joke.`type`),
            div(cls  := "text", b(joke.setup), br(()), i(joke.punchline)),
            button(
              cls        := "remove",
              title      := "Remove joke",
              aria.label := "Remove joke",
              onClick(removeJoke(index)),
              "x"
            )
          )
        })
      )
    )
  }

def jokes = Jokes("Pick a type and add a random joke of that type.", initialType = "programming")

private val jokesCss = """
  .jokes .controls { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
  .jokes .controls select, .jokes .controls button { margin: 0; }
  .jokes .status { color: #6b7280; font-size: 0.9em; }
  .jokes .count { display: inline-block; min-width: 1.6em; padding: 0 6px; border-radius: 999px; background: #e5e7eb; font-size: 0.8em; text-align: center; }
  .jokes .list { list-style: none; padding: 0; margin: 0; }
  .jokes .list li { display: flex; align-items: flex-start; gap: 12px; padding: 10px 12px; margin-bottom: 8px; border: 1px solid #e5e7eb; border-radius: 8px; }
  .jokes .tag { flex: none; width: 92px; padding: 2px 8px; border-radius: 999px; background: #dbeafe; color: #1e40af; font-size: 0.75em; text-align: center; }
  .jokes .text { flex: 1; white-space: pre-line; }
  .jokes .text i { color: #4b5563; }
  .jokes .remove { flex: none; margin: 0; padding: 0 10px; line-height: 1.8; background: transparent; color: #9ca3af; }
  .jokes .remove:hover { background: #fee2e2; color: #b91c1c; }
"""
