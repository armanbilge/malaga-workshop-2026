import calico.*
import calico.html.Modifier
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*
import org.scalajs.dom

/** A static Elo skill tree drawn in SVG. Hover or focus highlights a rung's links; click or Enter opens its details. */
def skillTree: Resource[IO, HtmlElement[IO]] =
  import SkillTree.{*, given}
  (SignallingRef[IO].of(Option.empty[TreeRung]), SignallingRef[IO].of(Option.empty[TreeRung])).tupled.toResource
    .flatMap { (selected, hovered) =>
      // The rung whose links are highlighted: the one under the pointer or focus, else the open one.
      val focus: Signal[IO, Option[TreeRung]] =
        (hovered: Signal[IO, Option[TreeRung]], selected: Signal[IO, Option[TreeRung]]).mapN(_ orElse _)
      val close = selected.set(None)
      div(
        cls := "skill-tree",
        styleTag(treeCss),
        onKeyDown --> (_.filter(_.key == "Escape").foreach(_ => close)),
        div(
          cls := "stage",
          svgTag(
            "svg",
            (
              "viewBox"    := s"0 0 ${TreeLayout.Width} ${TreeLayout.Height}",
              "width"      := TreeLayout.Width.toString,
              "height"     := TreeLayout.Height.toString,
              "role"       := "group",
              "aria-label" := "Skill tree. Tab through the rungs, Enter opens details, Escape closes them.",
              svgTag("rect", ("class" := "backdrop", "width" := "100%", "height" := "100%", onClick(close))),
              TreeLayout.ticks.map(tick),
              TreeData.skills.map(header),
              TreeData.ladders.map((a, b) => edge(a, b, "ladder", focus)),
              TreeData.needs.map((a, b) => edge(a, b, "needs", focus)),
              TreeData.rungs.map(rungNode(_, selected, hovered, focus))
            )
          ),
          div(children <-- selected.map(_.toList.map(r => popover(r, close): Resource[IO, Node[IO]])))
        )
      )
    }

/** Everything the widget needs, kept out of the namespace the other widget files share. */
private object SkillTree:
  def rungNode(
      r: TreeRung,
      selected: SignallingRef[IO, Option[TreeRung]],
      hovered: SignallingRef[IO, Option[TreeRung]],
      focus: Signal[IO, Option[TreeRung]]
  ) =
    val (x, y)  = TreeLayout.at(r)
    val open    = selected.set(Some(r))
    val classes = (selected: Signal[IO, Option[TreeRung]], focus).mapN { (s, f) =>
      List("rung" -> true, "selected" -> s.contains(r), "linked" -> f.exists(TreeData.linked(_, r)))
        .collect { case (c, true) =>
          c
        }
        .mkString(" ")
    }
    svgTag(
      "g",
      (
        "class" <-- classes,
        "transform"  := s"translate($x,$y)",
        "tabindex"   := "0",
        "role"       := "button",
        "aria-label" := s"${TreeData.skills(r.column).name}, ${r.elo} Elo: ${r.name}",
        onClick(open),
        onKeyDown --> (_.filter(e => e.key == "Enter" || e.key == " ").foreach(e => e.preventDefault *> open)),
        onMouseEnter(hovered.set(Some(r))),
        onMouseLeave(hovered.set(None)),
        onFocus(hovered.set(Some(r))),
        onBlur(hovered.set(None)),
        svgTag("circle", ("class" := "ring", "r" := "15")),
        svgTag("circle", ("class" := "dot", "r" := "11")),
        svgTag("text", ("class" := "elo", "x" := "-24", "dy" := "0.35em", r.elo.toString)),
        svgTag("text", ("class" := "name", "x" := "22", "dy" := "0.35em", r.name))
      )
    )

  def edge(a: TreeRung, b: TreeRung, kind: String, focus: Signal[IO, Option[TreeRung]]) =
    val (x1, y1) = TreeLayout.at(a)
    val (x2, y2) = TreeLayout.at(b)
    // Links bow below the row, so a link between rungs of equal Elo does not run through their labels.
    val d =
      if kind == "ladder" then s"M$x1,$y1 L$x2,$y2"
      else s"M$x1,$y1 Q${(x1 + x2) / 2},${y1.max(y2) + 56} $x2,$y2"
    svgTag(
      "path",
      ("d" := d, "class" <-- focus.map(f => s"edge $kind" + (if f.exists(r => r == a || r == b) then " hot" else "")))
    )

  def header(s: TreeSkill) =
    val x = TreeLayout.columnX(s.column)
    svgTag(
      "text",
      (
        "class" := (if s.column == 1 then "header root" else "header"),
        s.lines.zipWithIndex.map((line, i) => svgTag("tspan", ("x" := x.toString, "y" := (22 + i * 16).toString, line)))
      )
    )

  def tick(elo: Int) =
    val y = TreeLayout.y(elo)
    svgTag(
      "line",
      ("class" := "tick", "x1" := "0", "x2" := TreeLayout.Width.toString, "y1" := y.toString, "y2" := y.toString)
    )

  def popover(r: TreeRung, close: IO[Unit]) =
    val (x, y) = TreeLayout.at(r)
    // Below the rung, so its own label stays readable; kept inside the tree horizontally.
    val left    = (x - 24).max(0).min(TreeLayout.Width - TreeLayout.PopoverWidth)
    val top     = y + 24
    val needs   = TreeData.prerequisites(r)
    val unlocks = TreeData.unlocks(r)
    div(
      cls        := "popover",
      role       := List("dialog"),
      aria.label := r.name,
      styleAttr  := s"left: ${left}px; top: ${top}px; width: ${TreeLayout.PopoverWidth}px",
      div(
        cls := "top",
        div(cls    := "meta", s"${TreeData.skills(r.column).name} · ${r.elo} Elo"),
        button(cls := "close", aria.label := "Close", onClick(close), "×")
      ),
      h4(r.name),
      p(r.blurb),
      Option.when(needs.nonEmpty)(div(cls := "links", b("Needs "), needs.map(_.label).mkString(", "))),
      Option.when(unlocks.nonEmpty)(div(cls := "links", b("Unlocks "), unlocks.map(_.label).mkString(", ")))
    )

  // A tiny SVG layer: Calico 0.2.3 builds HTML elements only, so SVG ones are created in the SVG namespace and given
  // plain attributes, constant or driven by a Signal. Events and children use Calico's own modifiers unchanged.

  def svgTag[M](tag: String, mods: M)(using M: Modifier[IO, Element[IO], M]): Resource[IO, Element[IO]] =
    Resource
      .eval(IO(dom.document.createElementNS("http://www.w3.org/2000/svg", tag).asInstanceOf[Element[IO]]))
      .flatTap(M.modify(mods, _))

  final case class SvgAttr(key: String, value: String)
  final case class SvgAttrSignal(key: String, values: Signal[IO, String])

  extension (key: String)
    def :=(value: String): SvgAttr                     = SvgAttr(key, value)
    def <--(values: Signal[IO, String]): SvgAttrSignal = SvgAttrSignal(key, values)

  def setAttr(e: Element[IO], key: String, value: String) =
    IO(e.asInstanceOf[dom.Element].setAttribute(key, value))

  given Modifier[IO, Element[IO], SvgAttr] = (a, e) => Resource.eval(setAttr(e, a.key, a.value))

  given Modifier[IO, Element[IO], SvgAttrSignal] = (a, e) =>
    a.values.getAndDiscreteUpdates.flatMap { (head, tail) =>
      Resource.eval(setAttr(e, a.key, head)) *> tail.foreach(setAttr(e, a.key, _)).compile.drain.background.void
    }

  // Model

  case class TreeSkill(name: String, column: Int, lines: List[String])

  case class TreeRung(column: Int, elo: Int, name: String, blurb: String):
    def label = s"${TreeData.skills(column).name}, $name ($elo)"

  object TreeLayout:
    val Width        = 820
    val Height       = 440
    val PopoverWidth = 280
    val elos         = List(1800, 1400, 1000, 600)
    val ticks        = elos

    def columnX(column: Int): Int   = 70 + column * 260
    def y(elo: Int): Int            = 80 + elos.indexOf(elo) * 105
    def at(r: TreeRung): (Int, Int) = (columnX(r.column), y(r.elo))

  object TreeData:
    val skills = List(
      TreeSkill("Understanding the Typelevel ecosystem", 0, List("Understanding the", "Typelevel ecosystem")),
      TreeSkill("Calico Developer", 1, List("Calico Developer")),
      TreeSkill("Understanding the browser tech stack", 2, List("Understanding the", "browser tech stack"))
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
        "You pick the right abstraction for lifetimes, sequences and state without looking it up."
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
        "You render keyed lists with children[K] and know which nodes survive an update."
      ),
      TreeRung(
        2,
        600,
        "The page has a DOM",
        "You know HTML becomes a tree of nodes and that JavaScript can change it."
      ),
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
        "You use the browser's network and navigation APIs and know what each costs."
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

    /** Dependency edges: (prerequisite in a side skill, Calico rung that needs it). */
    val needs = List(
      rung(0, 1000) -> rung(1, 1000),
      rung(2, 1000) -> rung(1, 1000),
      rung(0, 1400) -> rung(1, 1400),
      rung(2, 1400) -> rung(1, 1800)
    )

    def prerequisites(r: TreeRung)                    = needs.collect { case (from, `r`) => from }
    def unlocks(r: TreeRung)                          = needs.collect { case (`r`, to) => to }
    def linked(focus: TreeRung, r: TreeRung): Boolean =
      focus == r || needs.contains(r -> focus) || needs.contains(focus -> r)

  val treeCss = """
  .skill-tree .stage { position: relative; width: 820px; }
  .skill-tree svg { display: block; font-family: inherit; }
  .skill-tree .backdrop { fill: transparent; }
  .skill-tree .tick { stroke: #eceef2; stroke-width: 1; }
  .skill-tree .header { fill: #6b7280; font-size: 12px; font-weight: 600; text-anchor: middle; letter-spacing: 0.02em; }
  .skill-tree .header.root { fill: #111827; font-size: 14px; }
  .skill-tree .edge { fill: none; stroke: #d1d5db; stroke-width: 1.5; transition: stroke 0.15s; }
  .skill-tree .edge.needs { stroke-dasharray: 3 4; }
  .skill-tree .edge.hot { stroke: #2563eb; stroke-width: 2; }
  .skill-tree .rung { cursor: pointer; outline: none; }
  .skill-tree .rung .ring { fill: #fff; stroke: transparent; stroke-width: 2; transition: stroke 0.15s; }
  .skill-tree .rung .dot { fill: #fff; stroke: #374151; stroke-width: 2; transition: fill 0.15s; }
  .skill-tree .rung:hover .dot, .skill-tree .rung.linked .dot { fill: #dbeafe; stroke: #2563eb; }
  .skill-tree .rung:focus-visible .ring { stroke: #2563eb; }
  .skill-tree .rung.selected .dot { fill: #2563eb; stroke: #2563eb; }
  .skill-tree .elo { fill: #9ca3af; font-size: 11px; text-anchor: end; font-variant-numeric: tabular-nums; }
  .skill-tree .name, .skill-tree .elo { paint-order: stroke; stroke: #fff; stroke-width: 4px; stroke-linejoin: round; }
  .skill-tree .name { fill: #111827; font-size: 13px; }
  .skill-tree .rung.selected .name { font-weight: 600; }
  .skill-tree .popover {
    position: absolute; background: #fff; border: 1px solid #e5e7eb; border-radius: 8px;
    box-shadow: 0 10px 30px rgba(17, 24, 39, 0.12); padding: 12px 14px; font-size: 14px; z-index: 1;
  }
  .skill-tree .popover h4 { margin: 2px 0 6px; }
  .skill-tree .popover p { margin: 0 0 8px; color: #374151; }
  .skill-tree .popover .meta { color: #6b7280; font-size: 12px; }
  .skill-tree .popover .links { color: #374151; font-size: 12px; margin-top: 4px; }
  .skill-tree .popover .top { display: flex; align-items: flex-start; gap: 8px; }
  .skill-tree .popover .meta { flex: 1; }
  .skill-tree .popover .close {
    flex: none; margin: -4px -6px 0 0; border: none; background: none; padding: 0 6px;
    font-size: 18px; line-height: 1; color: #6b7280; cursor: pointer;
  }
  @media (prefers-reduced-motion: reduce) { .skill-tree * { transition: none !important; } }
  """
