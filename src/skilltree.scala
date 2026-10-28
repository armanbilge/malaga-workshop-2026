import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*

def skillTree: Resource[IO, HtmlElement[IO]] =
  SignallingRef[IO].of(Option.empty[TreeRung]).toResource.flatMap { selected =>
    div(
      cls := "skill-tree",
      styleTag(treeCss),
      div(
        cls       := "canvas",
        styleAttr := s"width: ${TreeLayout.Width}px; height: ${TreeLayout.Height}px",
        TreeData.skills.map(s => div(cls := "header", styleAttr := TreeLayout.headerStyle(s.column), s.name)),
        TreeData.ladders.map((a, b) => treeLine(a, b, "ladder")),
        TreeData.needs.map((from, to) => treeLine(from, to, "needs")),
        TreeData.rungs.map(rungButton(_, selected))
      ),
      div(children <-- selected.map[List[Resource[IO, Node[IO]]]](_.toList.map(rungDetails)))
    )
  }

private def rungButton(r: TreeRung, selected: SignallingRef[IO, Option[TreeRung]]) =
  val needs = TreeData.prerequisites(r)
  button(
    tpe := "button",
    cls <-- selected.map(s => "rung" :: Option.when(s.contains(r))("selected").toList),
    styleAttr := TreeLayout.rungStyle(r),
    title     := (if needs.isEmpty then r.name else s"${r.name}. Needs: ${needs.map(_.label).mkString(", ")}"),
    onClick(selected.set(Some(r))), // a <button> also fires click on Enter and Space
    span(cls := "elo", r.elo.toString),
    r.name
  )

private def rungDetails(r: TreeRung) =
  val needs = TreeData.prerequisites(r)
  div(
    cls := "details",
    h4(s"${r.name} (${r.elo})"),
    small(s"${TreeData.skills(r.column).name} · ${EloBand.of(r.elo).label}"),
    p(r.blurb),
    p(small(if needs.isEmpty then "No prerequisites from other skills."
    else s"Needs: ${needs.map(_.label).mkString(", ")}"))
  )

/** A straight line between two rung centres: a thin div, rotated. Calico 0.2.3 has no SVG DSL. */
private def treeLine(a: TreeRung, b: TreeRung, kind: String) =
  val (x1, y1) = TreeLayout.centre(a)
  val (x2, y2) = TreeLayout.centre(b)
  val length   = math.hypot(x2 - x1, y2 - y1)
  val angle    = math.atan2(y2 - y1, x2 - x1)
  div(
    cls       := s"line $kind",
    styleAttr := f"left: ${x1}%.1fpx; top: ${y1}%.1fpx; width: $length%.1fpx; transform: rotate($angle%.4frad)"
  )

// Model

private case class TreeSkill(name: String, column: Int)

private case class TreeRung(column: Int, elo: Int, name: String, blurb: String):
  def label = s"${TreeData.skills(column).name} – $name ($elo)"

private enum EloBand(val below: Int, val colour: String, val ink: String, val label: String):
  case Novice       extends EloBand(600, "#3b3b4a", "#ffffff", "below 600")
  case Apprentice   extends EloBand(1000, "#5b6bb0", "#ffffff", "600–1000")
  case Practitioner extends EloBand(1400, "#8cc37a", "#1a1a1a", "1000–1400")
  case Expert       extends EloBand(1800, "#e0a030", "#1a1a1a", "1400–1800")
  case Master       extends EloBand(Int.MaxValue, "#ff4060", "#1a1a1a", "1800 and above")

private object EloBand:
  def of(elo: Int): EloBand = values.find(elo < _.below).getOrElse(Master)

private object TreeData:
  val skills = List(
    TreeSkill("Understanding the Typelevel ecosystem", 0),
    TreeSkill("Calico Developer", 1),
    TreeSkill("Understanding the browser tech stack", 2)
  )

  val rungs = List(
    TreeRung(
      0,
      600,
      "Cats are a library?",
      "You know Cats Effect, FS2 and http4s exist and that they somehow fit together."
    ),
    TreeRung(
      0,
      1000,
      "IO is a value",
      "You can explain why IO describes a program instead of running one, and why that matters."
    ),
    TreeRung(
      0,
      1400,
      "Resource, Stream, Signal",
      "You pick the right Typelevel abstraction for lifetimes, sequences and state without looking it up."
    ),
    TreeRung(
      0,
      1800,
      "Reads the ecosystem",
      "You read a library's type signatures and know how it composes with the rest of the stack."
    ),
    TreeRung(
      1,
      600,
      "It rendered something",
      "div(\"hello\") showed up on the page, and you are not yet sure why it is a Resource."
    ),
    TreeRung(
      1,
      1000,
      "Signals in, nodes out",
      "You bind attributes and children with <-- and stop calling DOM setters by hand."
    ),
    TreeRung(
      1,
      1400,
      "Components are Resources",
      "Components take refs as arguments, and their fibers stop when they unmount."
    ),
    TreeRung(
      1,
      1800,
      "Keyed children whisperer",
      "You render keyed lists with children[K] and know exactly which nodes survive an update."
    ),
    TreeRung(2, 600, "The page has a DOM", "You know HTML becomes a tree of nodes and that JavaScript can change it."),
    TreeRung(
      2,
      1000,
      "Events bubble",
      "You know how events propagate, what preventDefault does and why focus matters."
    ),
    TreeRung(
      2,
      1400,
      "Fetch, WebSocket, history",
      "You use the browser's network and navigation APIs and know what each one costs."
    ),
    TreeRung(
      2,
      1800,
      "The browser is a runtime",
      "You reason about the event loop, rendering frames and accessibility like a platform engineer."
    )
  )

  private def rung(column: Int, elo: Int) = rungs.find(r => r.column == column && r.elo == elo).get

  val ladders =
    rungs.groupBy(_.column).values.toList.flatMap(_.sortBy(_.elo).sliding(2).collect { case List(a, b) => (a, b) })

  /** Dependency edges: (prerequisite in a sub-skill, root rung that needs it). */
  val needs = List(
    rung(0, 1000) -> rung(1, 1000),
    rung(2, 1000) -> rung(1, 1000),
    rung(0, 1400) -> rung(1, 1400),
    rung(2, 1400) -> rung(1, 1800)
  )

  def prerequisites(r: TreeRung) = needs.collect { case (from, `r`) => from }

private object TreeLayout:
  val Width     = 720
  val Height    = 470
  val RungWidth = 200
  val Top       = 70 // below the column headers
  val MinElo    = 300
  val MaxElo    = 2000

  def x(column: Int)      = Width / 6 + column * Width / 3
  def y(elo: Int)         = Top + (MaxElo - elo).toDouble / (MaxElo - MinElo) * (Height - Top - 20)
  def centre(r: TreeRung) = (x(r.column).toDouble, y(r.elo))

  def headerStyle(column: Int) = s"left: ${x(column) - RungWidth / 2}px; width: ${RungWidth}px"

  def rungStyle(r: TreeRung) =
    val band = EloBand.of(r.elo)
    f"left: ${x(r.column) - RungWidth / 2}px; top: ${y(r.elo)}%.1fpx; width: ${RungWidth}px; background: ${band.colour}; color: ${band.ink}"

private val treeCss = """
  .skill-tree .canvas { position: relative; margin: 0 auto; }
  .skill-tree .header { position: absolute; top: 0; text-align: center; font-weight: 600; line-height: 1.2; }
  .skill-tree .line { position: absolute; height: 0; transform-origin: 0 0; pointer-events: none; }
  .skill-tree .ladder { border-top: 4px solid #9a9aae; margin-top: -2px; }
  .skill-tree .needs { border-top: 2px dashed #5a5a70; margin-top: -1px; }
  .skill-tree .rung { position: absolute; margin: 0; padding: 4px 8px; transform: translateY(-50%);
    font-size: 0.85em; line-height: 1.2; text-align: left; border: 2px solid transparent; border-radius: 8px; }
  .skill-tree .rung:hover { filter: brightness(1.08); }
  .skill-tree .rung.selected { border-color: #1a1a1a; box-shadow: 0 0 0 2px #fff, 0 0 0 4px #1a1a1a; }
  .skill-tree .elo { font-weight: 700; margin-right: 6px; }
  .skill-tree .details { border: 1px solid #ddd; border-radius: 12px; padding: 8px 16px; margin-top: 12px; }
  .skill-tree .details h4 { margin: 0; }
"""
