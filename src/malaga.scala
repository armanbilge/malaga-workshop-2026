import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.*

def malaga: Resource[IO, HtmlElement[IO]] =
  SignallingRef[IO].of(WineInputs.default).toResource.flatMap { inputs =>
    val wine = inputs.map(DoMalaga.classify)
    div(
      cls := "wine-lab",
      styleTag(css),
      header(inputs),
      inputsSection(inputs),
      outputSection(wine)
    )
  }

private def header(inputs: SignallingRef[IO, WineInputs]) =
  div(
    cls := "row",
    div(small("Experiment with production choices")),
    button(tpe := "button", onClick(inputs.set(WineInputs.default)), "Reset")
  )

private def inputsSection(inputs: SignallingRef[IO, WineInputs]) =
  div(
    grapeChooser(inputs),
    slider(
      title = "2. Dry grapes in the sun",
      max = 100,
      read = inputs.map(_.drying),
      write = v => inputs.update(_.copy(drying = v)),
      display = v => if (v == 0) "Fresh grapes" else if (v == 100) "Raisins" else s"$v% dried"
    ),
    slider(
      title = "3. Fermentation extent",
      max = 100,
      read = inputs.map(_.fermentation),
      write = v => inputs.update(_.copy(fermentation = v)),
      display = v => s"$v%"
    ),
    fortifyToggle(inputs),
    slider(
      title = "4. Age your wine",
      max = 20,
      read = inputs.map(_.ageingYears),
      write = v => inputs.update(_.copy(ageingYears = v)),
      display = v => if (v == 1) "1 year" else s"$v years"
    )
  )

private def grapeChooser(inputs: SignallingRef[IO, WineInputs]) =
  div(
    div(cls := "row", span("1. Choose your grape")),
    div(
      cls := "segmented",
      Grape.values.toList.map { grape =>
        button(
          tpe := "button",
          cls <-- inputs.map(i => Option.when(i.grape == grape)("selected").toList),
          onClick(inputs.update(_.copy(grape = grape))),
          grape.label
        )
      }
    )
  )

private def slider(title: String, max: Int, read: Signal[IO, Int], write: Int => IO[Unit], display: Int => String) =
  div(
    div(cls := "row", span(title), small(read.map(display))),
    input.withSelf { self =>
      (
        tpe     := "range",
        minAttr := "0",
        maxAttr := max.toString,
        value <-- read.map(_.toString),
        onInput(self.value.get.map(_.toIntOption).flatMap(_.traverse_(write)))
      )
    }
  )

private def fortifyToggle(inputs: SignallingRef[IO, WineInputs]) =
  label(
    input.withSelf { self =>
      (
        tpe := "checkbox",
        checked <-- inputs.map(_.fortified),
        onChange(self.checked.get.flatMap(v => inputs.update(_.copy(fortified = v))))
      )
    },
    " Fortify with grape spirit"
  )

private def outputSection(wine: Signal[IO, Wine]) =
  div(
    hr(()),
    h4("Your experimental wine"),
    div(
      cls := "result",
      glass(wine),
      div(
        h3(cls := "headline", wine.map(_.name)),
        small(wine.map(w => s"${w.inputs.grape.label} · ${w.kind}")),
        chips(wine.map(_.terms), "chip"),
        chips(wine.map(_.aromas), "chip aroma")
      )
    ),
    div(
      cls := "metrics",
      metric("Residual sugar", wine.map(w => f"${w.residualSugar}%.0f g/L"), wine.map(_.sugarTerm)),
      metric(
        "Alcohol",
        wine.map(w => f"${w.acquiredAlcohol}%.1f%% vol"),
        wine.map(w => f"${w.totalAlcohol}%.1f%% vol potential, sugar included")
      ),
      metric("Ageing", wine.map(w => s"${w.inputs.ageingYears} yr"), wine.map(_.ageingTerm.getOrElse("no ageing term")))
    ),
    p(wine.map(_.notes.mkString(" "))),
    p(
      small(
        "Rules from the ",
        a(
          href := "https://www.juntadeandalucia.es/export/drupaljda/V_DO_MALAGA.pdf",
          "DO Málaga product specification"
        ),
        " (pliego de condiciones)."
      )
    )
  )

private def glass(wine: Signal[IO, Wine]) =
  div(
    cls := "glass",
    div(
      cls := "bowl",
      // Sun-drying removes water, so the bowl shows the juice left relative to fresh grapes.
      div(styleAttr <-- wine.map { w =>
        s"height: ${(DoMalaga.mustSugar(0) / w.mustSugar * 80).round}%; background: ${w.colour}"
      })
    ),
    div(cls := "stem"),
    div(cls := "foot")
  )

private def chips(labels: Signal[IO, List[String]], chipCls: String) =
  div(children <-- labels.map[List[Resource[IO, fs2.dom.Node[IO]]]](_.map(l => span(cls := chipCls, l))))

private def metric(title: String, big: Signal[IO, String], sub: Signal[IO, String]) =
  div(small(title), div(cls := "big", big), small(sub))

private val css = """
  .wine-lab { border: 1px solid #ddd; border-radius: 12px; padding: 16px 20px; }
  .wine-lab h4 { margin: 0; }
  .wine-lab .row { display: flex; justify-content: space-between; align-items: baseline; }
  .wine-lab .segmented { display: flex; gap: 6px; }
  .wine-lab .segmented button { flex: 1; }
  .wine-lab .segmented .selected { background: #363636; color: #fff; }
  .wine-lab input[type=range] { width: 100%; }
  .wine-lab .headline { margin: 8px 0 0; }
  .wine-lab .chip { display: inline-block; background: #eee; border-radius: 999px; padding: 2px 10px; font-size: 0.8em; margin: 6px 4px 0 0; }
  .wine-lab .aroma { background: none; border: 1px solid #ddd; }
  .wine-lab .metrics { display: flex; gap: 32px; margin: 12px 0 8px; }
  .wine-lab .big { font-size: 1.3em; font-weight: 600; }
  .wine-lab .result { display: flex; gap: 20px; align-items: center; }
  .wine-lab .glass { flex: none; display: flex; flex-direction: column; align-items: center; }
  .wine-lab .bowl { position: relative; overflow: hidden; width: 70px; height: 80px; border: 2px solid #999; border-radius: 0 0 35px 35px / 0 0 60px 60px; }
  .wine-lab .bowl div { position: absolute; bottom: 0; width: 100%; transition: all 0.3s; }
  .wine-lab .stem { width: 4px; height: 40px; background: #999; }
  .wine-lab .foot { width: 50px; height: 5px; background: #999; border-radius: 50%; }
"""

// Model

enum Grape(val label: String) {
  case Moscatel     extends Grape("Moscatel")
  case PedroXimenez extends Grape("Pedro Ximénez")
}

private case class WineInputs(
    grape: Grape,
    drying: Int,       // 0 = fresh grapes, 100 = fully sun-dried raisins
    fermentation: Int, // 0 = none, 100 = fermented as far as the yeast can go
    fortified: Boolean,
    ageingYears: Int // 0..20
)

private object WineInputs {
  val default = WineInputs(Grape.Moscatel, drying = 0, fermentation = 0, fortified = false, ageingYears = 0)
}

// Thresholds come from the DO «Málaga» product specification (pliego de condiciones):
// B.1 analytical limits, B.3 traditional terms, C.2 liqueur wines, C.3 style and colour terms,
// C.5 «Vendimia Asoleada», C.6 «Naturalmente Dulce», C.7 ageing.
// There is no arrope (cooked must) input, so only the arrope-free terms can appear.
private case class Wine(
    inputs: WineInputs,
    name: String, // headline: the most specific DO name, plus the ageing term
    kind: String, // the DO category, or why the wine falls outside the DO
    compliant: Boolean,
    colour: String,          // CSS colour for the glass, illustrative
    mustSugar: Double,       // g/L before fermentation
    residualSugar: Double,   // g/L left in the wine
    naturalAlcohol: Double,  // % vol made by fermentation
    acquiredAlcohol: Double, // % vol in the glass, after fortification if any
    totalAlcohol: Double,    // acquired + what the residual sugar could still give
    sugarTerm: String,
    ageingTerm: Option[String],
    terms: List[String], // every DO term the wine may carry
    aromas: List[String],
    notes: List[String]
)

private object DoMalaga {
  val SugarPerDegree    = 16.83 // g/L of sugar fermented per 1 % vol of alcohol
  val MaxNaturalAlcohol = 16.0  // yeast stops by itself around here (a «Maestro» stalls at 15–16 % vol)

  // Ripe fresh Málaga grapes give roughly 220 g/L; long asoleo pushes the must past the 350 g/L «Tierno» line.
  def mustSugar(drying: Int): Double = 220 + 2.0 * drying

  private case class Verdict(kind: String, primary: Option[String], compliant: Boolean, terms: List[String])

  def classify(in: WineInputs): Wine = {
    val must     = mustSugar(in.drying)
    val natural  = (must * in.fermentation / 100 / SugarPerDegree).min(MaxNaturalAlcohol)
    val residual = must - natural * SugarPerDegree
    val acquired = if (in.fortified) natural.max(15.0) else natural // C.2: encabezado to at least 15 % vol
    val total    = acquired + residual / SugarPerDegree
    val sweet    = residual >= 45                                   // B.1: «Dulce» starts at 45 g/L
    val years    = in.ageingYears

    // B.1 footnotes: «Seco» < 45 g/L (may be called «Semiseco» above 12); «Dulce» ≥ 45 (may be called «Semidulce» up to 140).
    val sugarTerms =
      if (!sweet) "Seco" :: Option.when(residual > 12)("Semiseco").toList
      else "Dulce" :: Option.when(residual <= 140)("Semidulce").toList

    // C.7.4 ageing terms; «Pálido» (B.3.2) is a sweet wine that was never aged.
    // Exactly five years is undefined in the pliego («Añejo» < 5, «Trasañejo» > 5); the slider treats 5 as Trasañejo.
    val ageingTerm = years match {
      case 0     => Option.when(sweet)("Pálido")
      case 1     => None // 6–24 months: plain «Málaga», no traditional term
      case 2     => Some("Noble")
      case 3 | 4 => Some("Añejo")
      case _     => Some("Trasañejo")
    }

    val verdict =
      if (in.fortified) {
        val tierno       = must > 350 && natural < 3 // C.2: long asoleo, at most a slight spontaneous fermentation
        val dulceNatural =
          must >= 212 && natural >= 7 // C.2: ≥ 212 g/L must, ≥ 7 % vol from fermentation, then fortified
        val aged  = years >= 1 || ageingTerm.contains("Pálido") // C.7.3: ≥ 6 months in oak unless «Pálido»
        val style =                                             // C.3.1, the arrope-free terms
          if (!sweet) Some("Dry Pale")
          else if (residual > 140) Option.when(years >= 1)("Sweet") // ageing obligatory
          else if (years >= 2) Some("Pajarete")                     // 45–140 g/L and at least 2 years
          else Option.when(residual <= 100)("Pale Cream")           // 45–100 g/L
        val colourTerm = Option.when(years >= 1)("Dorado") // C.3.2: aged liqueur wine, 0 % arrope
        val base       = "Vino de licor" :: style.toList ++ colourTerm
        if (!sweet)
          Verdict(
            if (aged) "Vino de licor seco"
            else
              "an unaged dry vino de licor: the DO asks for at least six months in oak, only a sweet «Pálido» may skip it",
            style,
            aged,
            base
          )
        else if (tierno) Verdict("Vino de licor dulce, «Tierno»", Some("Tierno"), aged, "Tierno" :: base)
        else if (dulceNatural)
          Verdict(
            "Vino de licor dulce, «Vino Dulce Natural»",
            Some(style.filter(_ == "Pajarete").getOrElse("Vino Dulce Natural")),
            aged,
            "Vino Dulce Natural" :: base
          )
        else
          Verdict(
            "fortified must (a mistela): a DO sweet liqueur wine needs ≥ 7 % vol from fermentation, or long asoleo for a «Tierno»",
            None,
            false,
            base
          )
      } else if (residual < 4) {
        if (acquired >= 15) Verdict("Vino seco (unfortified dry wine)", Some("Vino seco"), true, List("Vino seco"))
        else
          Verdict(
            f"a dry wine at $acquired%.1f %% vol: DO Málaga dry wines need ≥ 15 %% vol, lighter ones belong to DO Sierras de Málaga",
            None,
            false,
            Nil
          )
      } else if (!sweet)
        Verdict("an unfortified wine between 4 and 45 g/L: DO Málaga has no such category", None, false, Nil)
      else if (must > 300 && acquired >= 13 && residual >= 80) // C.6 and B.1
        Verdict(
          "Vino de uvas sobremaduradas, «Naturalmente Dulce»",
          Some("Naturalmente Dulce"),
          true,
          List("Vino de uvas sobremaduradas", "Naturalmente Dulce")
        )
      else if (must >= 252 && acquired >= 12 && total >= 16) // B.1; overripe grapes give > 15 % vol potential
        Verdict(
          "Vino dulce de uvas sobremaduradas",
          Some("Vino dulce de uvas sobremaduradas"),
          true,
          List("Vino de uvas sobremaduradas")
        )
      else if (must > 350 && residual > 140 && acquired >= 10 && total >= 18) // B.1; raisined = long asoleo
        Verdict(
          "Vino dulce de uvas pasificadas",
          Some("Vino dulce de uvas pasificadas"),
          true,
          List("Vino de uvas pasificadas")
        )
      else
        Verdict(
          f"sweet but only $acquired%.1f %% vol: too little fermentation for an unfortified DO wine",
          None,
          false,
          Nil
        )

    val terms = // only a compliant wine may carry DO terms
      if (verdict.compliant)
        (verdict.terms ++ sugarTerms ++ ageingTerm ++ Option.when(in.drying > 0)("Vendimia Asoleada")).distinct // C.5
      else Nil

    val grapeAromas = in.grape match {
      case Grape.Moscatel     => List("Orange blossom", "Fresh grape")
      case Grape.PedroXimenez => List("Fig", "Honey")
    }
    val dryingAromas =
      if (in.drying == 0) List("Fresh fruit") else if (must <= 350) List("Dried apricot") else List("Raisin")
    val aromas =
      grapeAromas ++ dryingAromas ++
        Option.when(natural >= 7)("Fermentative notes") ++
        Option.when(in.fortified)("Spirit warmth") ++
        Option.when(years >= 1)("Walnut") ++ Option.when(years >= 2)("Toffee") ++
        Option.when(years >= 3)("Coffee") ++ Option.when(years >= 5)("Leather")

    val notes = List(
      if (in.drying == 0)
        f"Fresh grapes: the must holds about $must%.0f g/L of sugar and keeps its primary varietal fruit."
      else if (must <= 350)
        f"Short asoleo concentrates the must to about $must%.0f g/L; past 300 g/L it qualifies for a «Naturalmente Dulce»."
      else
        f"Long asoleo concentrates the must to about $must%.0f g/L, beyond the 350 g/L the pliego asks of a «Tierno».",
      if (in.fermentation == 0)
        "Nothing has fermented, so all of that sugar is still there and the yeast has made no alcohol."
      else if (natural >= MaxNaturalAlcohol)
        f"Yeast gives up at about $MaxNaturalAlcohol%.0f %% vol, leaving $residual%.0f g/L unfermented: a very sweet must can never ferment to dryness."
      else
        f"Fermentation turned about ${must - residual}%.0f g/L of sugar into $natural%.1f %% vol of alcohol (about 17 g/L per degree), leaving $residual%.0f g/L.",
      if (in.fortified)
        f"Wine spirit (encabezado) stops fermentation and lifts the wine to $acquired%.0f %% vol, inside the 15–22 %% vol range of a vino de licor."
      else
        "No spirit added: every degree of alcohol comes from fermentation, so sweetness and strength pull against each other.",
      if (years == 0)
        "Unaged. DO liqueur wines must spend at least six months in oak unless they are sold as «Pálido»."
      else
        s"$years ${
            if (years == 1) "year" else "years"
          } in oak casks of at most 1,000 L, by añadas or criaderas y soleras: «Noble» is 2 to 3 years, «Añejo» 3 to 5, «Trasañejo» over 5.",
      if (verdict.compliant) s"Result: ${verdict.kind}." else s"Outside the DO rules: ${verdict.kind}."
    )

    // Colour follows the pliego's descriptions: unaged wines are straw to pale gold, «Dorado» liqueur wines gold to amber,
    // «Pajarete» gold to dark amber, and aged «Sweet» wines amber to black.
    val colour = {
      val shade = years.min(5) + (if (residual > 140 && years >= 1) 1 else 0)
      List("#f1e3a6", "#e9c860", "#d8a830", "#c48a2a", "#a96a1e", "#7d4414", "#4b2609")(shade)
    }

    Wine(
      inputs = in,
      name = if (verdict.compliant) (verdict.primary.toList ++ ageingTerm).mkString(" ") else "Not a DO Málaga wine",
      kind = verdict.kind,
      compliant = verdict.compliant,
      colour = colour,
      mustSugar = must,
      residualSugar = residual,
      naturalAlcohol = natural,
      acquiredAlcohol = acquired,
      totalAlcohol = total,
      sugarTerm = sugarTerms.mkString(" / "),
      ageingTerm = ageingTerm,
      terms = terms,
      aromas = aromas,
      notes = notes
    )
  }
}
