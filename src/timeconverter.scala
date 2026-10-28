import calico.*
import calico.frp.given
import calico.html.io.{*, given}
import calico.syntax.*
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*
import scala.scalajs.js

private val destinationStorageKey = "time-converter.default-destination"

private def availableTimeZones =
  js.Dynamic.global.Intl
    .supportedValuesOf("timeZone")
    .asInstanceOf[js.Array[String]]
    .toList
    .sorted

private def browserTimeZone =
  js.Dynamic.global.Intl
    .DateTimeFormat()
    .resolvedOptions()
    .timeZone
    .asInstanceOf[String]

private def validTimeZone(timeZone: String) =
  try
    js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
      "en-US",
      js.Dynamic.literal(timeZone = timeZone)
    )
    true
  catch
    case _: Throwable => false

private def dateParts(date: js.Date, timeZone: String): Map[String, String] =
  val formatter = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
    "en-US",
    js.Dynamic.literal(
      timeZone = timeZone,
      year = "numeric",
      month = "numeric",
      day = "numeric",
      hour = "numeric",
      minute = "numeric",
      second = "numeric",
      hourCycle = "h23"
    )
  )

  formatter
    .formatToParts(date)
    .asInstanceOf[js.Array[js.Dynamic]]
    .iterator
    .map { part =>
      part.selectDynamic("type").asInstanceOf[String] ->
        part.selectDynamic("value").asInstanceOf[String]
    }
    .toMap

private def utcMillis(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int
) =
  js.Dynamic.global.Date
    .UTC(year, month - 1, day, hour, minute, 0)
    .asInstanceOf[Double]

private def instantForLocalTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    timeZone: String
) =
  val desired = utcMillis(year, month, day, hour, minute)
  var candidate = desired

  // Refine the UTC guess using the source zone's offset. Repeating also handles
  // offset changes around daylight-saving transitions.
  for _ <- 1 to 3 do
    val parts = dateParts(new js.Date(candidate), timeZone)
    val displayedAsUtc = utcMillis(
      parts("year").toInt,
      parts("month").toInt,
      parts("day").toInt,
      parts("hour").toInt,
      parts("minute").toInt
    )
    candidate += desired - displayedAsUtc

  candidate

private def convertedTime(
    sourceTimeZone: String,
    destinationTimeZone: String,
    hour: Int,
    minute: Int,
    period: String
) =
  try
    val today = dateParts(new js.Date(), sourceTimeZone)
    val hour24 =
      if period == "AM" then hour % 12
      else (hour % 12) + 12

    val instant = instantForLocalTime(
      today("year").toInt,
      today("month").toInt,
      today("day").toInt,
      hour24,
      minute,
      sourceTimeZone
    )

    val formatter = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
      "en-US",
      js.Dynamic.literal(
        timeZone = destinationTimeZone,
        weekday = "short",
        month = "short",
        day = "numeric",
        hour = "numeric",
        minute = "2-digit",
        hour12 = true
      )
    )

    formatter.format(new js.Date(instant)).asInstanceOf[String]
  catch
    case _: Throwable => "Choose valid source and destination time zones"

def timeConverter: Resource[IO, HtmlDivElement[IO]] =
  val window = Window[IO]

  Resource.eval(
    window.localStorage
      .getItem(destinationStorageKey)
      .map(_.filter(validTimeZone))
      .handleError(_ => None)
  ).flatMap { savedDestination =>
    (
      SignallingRef[IO].of(browserTimeZone),
      SignallingRef[IO].of(savedDestination.getOrElse("America/New_York")),
      SignallingRef[IO].of(9),
      SignallingRef[IO].of(0),
      SignallingRef[IO].of("AM"),
      SignallingRef[IO].of(savedDestination.isDefined)
    ).tupled.toResource
  }.flatMap {
    case (
          sourceTimeZone,
          destinationTimeZone,
          sourceHour,
          sourceMinute,
          sourcePeriod,
          destinationDefaultSaved
        ) =>
      val destinationTime = (
        sourceTimeZone: Signal[IO, String],
        destinationTimeZone: Signal[IO, String],
        sourceHour: Signal[IO, Int],
        sourceMinute: Signal[IO, Int],
        sourcePeriod: Signal[IO, String]
      ).mapN(convertedTime)

      val timeControlStyle = "padding: 0.5rem;"

      div(
        div(
          styleAttr := "display: flex; gap: 1rem;",
          div(
            styleAttr := "flex: 1; padding: 2rem; background-color: #dbeafe; color: #1e3a8a;",
            label(
              styleAttr := "display: block; margin-bottom: 0.5rem; font-weight: bold;",
              "Source time zone"
            ),
            input.withSelf { self =>
              (
                typ := "text",
                listId := "time-zones",
                placeholder := "Start typing a time zone…",
                styleAttr := "width: 100%; box-sizing: border-box;",
                value <-- sourceTimeZone,
                onInput(self.value.get.flatMap(sourceTimeZone.set))
              )
            },
            button(
              typ := "button",
              styleAttr := "display: block; width: 100%; margin: 1rem 0 1.5rem; padding: 0.6rem 0.8rem;",
              "Use current time and timezone",
              onClick {
                IO {
                  val timeZone = browserTimeZone
                  val parts = dateParts(new js.Date(), timeZone)
                  val hour24 = parts("hour").toInt
                  val hour12 = if hour24 % 12 == 0 then 12 else hour24 % 12
                  val period = if hour24 < 12 then "AM" else "PM"
                  (timeZone, hour12, parts("minute").toInt, period)
                }.flatMap { (timeZone, hour, minute, period) =>
                  sourceTimeZone.set(timeZone) *>
                    sourceHour.set(hour) *>
                    sourceMinute.set(minute) *>
                    sourcePeriod.set(period)
                }.handleErrorWith(_ => IO.unit)
              }
            ),
            label(
              styleAttr := "display: block; margin-bottom: 0.5rem; font-weight: bold;",
              "Source time"
            ),
            div(
              styleAttr := "display: flex; align-items: center; gap: 0.5rem;",
              select.withSelf { self =>
                (
                  styleAttr := timeControlStyle,
                  (1 to 12).toList.map(hour => option(value := hour.toString, hour.toString)),
                  value <-- sourceHour.map(_.toString),
                  onChange(self.value.get.map(_.toInt).flatMap(sourceHour.set))
                )
              },
              span(":"),
              select.withSelf { self =>
                (
                  styleAttr := timeControlStyle,
                  (0 to 59).toList.map { minute =>
                    val padded = f"$minute%02d"
                    option(value := minute.toString, padded)
                  },
                  value <-- sourceMinute.map(_.toString),
                  onChange(self.value.get.map(_.toInt).flatMap(sourceMinute.set))
                )
              },
              select.withSelf { self =>
                (
                  styleAttr := timeControlStyle,
                  List("AM", "PM").map(period => option(value := period, period)),
                  value <-- sourcePeriod,
                  onChange(self.value.get.flatMap(sourcePeriod.set))
                )
              }
            )
          ),
          div(
            styleAttr := "flex: 1; padding: 2rem; background-color: #fef3c7; color: #92400e;",
            div(
              styleAttr := "display: flex; align-items: center; justify-content: space-between; gap: 1rem; margin-bottom: 0.5rem;",
              label(
                styleAttr := "font-weight: bold;",
                "Destination time zone"
              ),
              button(
                typ := "button",
                styleAttr := "margin: 0; padding: 0.4rem 0.7rem;",
                destinationDefaultSaved.map {
                  case true => "Default saved"
                  case false => "Set as default"
                },
                onClick {
                  destinationTimeZone.get.flatMap { timeZone =>
                    IO(validTimeZone(timeZone)).flatMap {
                      case true =>
                        window.localStorage
                          .setItem(destinationStorageKey, timeZone) *>
                          destinationDefaultSaved.set(true)
                      case false => destinationDefaultSaved.set(false)
                    }
                  }.handleErrorWith(_ => destinationDefaultSaved.set(false))
                }
              )
            ),
            input.withSelf { self =>
              (
                typ := "text",
                listId := "time-zones",
                placeholder := "Start typing a time zone…",
                styleAttr := "width: 100%; box-sizing: border-box;",
                value <-- destinationTimeZone,
                onInput {
                  self.value.get.flatMap { timeZone =>
                    destinationTimeZone.set(timeZone) *>
                      destinationDefaultSaved.set(false)
                  }
                }
              )
            },
            label(
              styleAttr := "display: block; margin: 1.5rem 0 0.5rem; font-weight: bold;",
              "Destination time"
            ),
            div(
              styleAttr := "font-size: 1.5rem; font-weight: bold;",
              destinationTime
            )
          )
        ),
        dataList(
          idAttr := "time-zones",
          availableTimeZones.map(timeZone => option(value := timeZone))
        )
      )
  }
