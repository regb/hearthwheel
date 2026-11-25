package hearthwheel
package core

import sgl.Input.Keys
import hearthwheel.model._

/** The rules of a map made by hand: its name, what wins it, and the aims on the way. */
private final class RulesView(map: HandMap, ui: Ui, close: () => Unit) {
  private val goals = Vector[(String, Goal)]("Endless" -> Goal.Endless, "One year" -> Goal.Survive(1), "Two years" -> Goal.Survive(2), "Three years" -> Goal.Survive(3), "Cleanse" -> Goal.Cleanse, "Great Wheel" -> Goal.Wheel)
  private val threats = Vector("Mild" -> 0.6f, "Even" -> 1f, "Harsh" -> 1.5f, "Cruel" -> 2.2f)
  private val starts = Vector("Poor" -> 1, "Fair" -> 3, "Rich" -> 10)
  private val lengths = Vector("Quick" -> 0.5f, "Usual" -> 1f, "Long" -> 1.8f)
  private val usual = Vector(150f, 140f, 140f, 120f)
  private val blocks = Blocks.all.filter(b => b.category.nonEmpty && Pages.byBlock.contains(b))
  private val crew = UnitKinds.all.filter(k => k.friendly && !UnitKinds.isLantern(k))
  private def stock(n: Int) = Vector(Stack(Items.Copper, 220 * n), Stack(Items.Tin, 60 * n), Stack(Items.Flint, 40 * n), Stack(Items.Coal, 40 * n))

  def draw(): Unit = {
    val w = scala.math.min(ui.width - 20f, 1060f)
    val h = scala.math.min(ui.height - 20f, 620f)
    val (x, y) = Sheets.open(ui, "Rules of the map", w, h, close)
    var r = map.rules
    ui.text("Name", x + 26f, y + 66f, 14f, Sheets.Brown, bold = true)
    ui.rect(x + 100f, y + 60f, 300f, 30f, Tint.Ink)
    ui.text(Lang.verbatim(r.name + (if((ui.time * 2f).toInt % 2 == 0) "_" else "")), x + 110f, y + 66f, 15f, Tint.Cream)
    def choice[A](row: Int, label: String, options: Vector[(String, A)], now: A => Boolean)(choose: A => Unit): Unit = {
      val cy = y + 100f + row * 38f
      ui.text(label, x + 26f, cy + 7f, 14f, Sheets.Brown, bold = true)
      options.zipWithIndex.foreach { case ((name, v), i) => if(ui.button(x + 100f + i * 92f, cy, 88f, 31f, name, enabled = !now(v), size = 13f)) choose(v) }
    }
    choice[Goal](0, "Goal", goals, _ == r.goal)(g => r = r.copy(goal = g))
    choice[Float](1, "Threat", threats, t => scala.math.abs(t - r.threat) < 0.01f)(t => r = r.copy(threat = t))
    choice[Float](2, "Seasons", lengths, k => scala.math.abs(r.seasons(0) - usual(0) * k) < 1f)(k => r = r.copy(seasons = usual.map(_ * k)))
    choice[Int](3, "Starts in", Season.values.toVector.map(s => s.label -> s.ordinal), _ == r.startSeason)(s => r = r.copy(startSeason = s))
    choice[Int](4, "Start", starts, n => r.start == stock(n))(n => r = r.copy(start = stock(n)))
    val tx = x + w - 300f
    def toggle(i: Int, label: String, v: Boolean): Boolean = {
      val ty = y + 100f + i * 38f
      ui.catchMouse(tx, ty, 270f, 28f)
      ui.nine(ui.art(if(ui.over(tx, ty, 270f, 28f)) "slot_hot" else "slot"), tx, ty, 28f, 28f, 8f)
      if(v) ui.picture(ui.art.icon("check"), tx + 5f, ty + 5f, 18f, 18f)
      ui.text(label, tx + 38f, ty + 5f, 14f, Tint.Ink)
      if(ui.clicked(tx, ty, 270f, 28f)) !v else v
    }
    r = r.copy(wild = toggle(0, "The Bramble spreads", r.wild), calm = toggle(1, "Calm skies", r.calm), patient = toggle(2, "Waves wait for the last foe", r.patient), timer = !toggle(3, "Waves only on call", !r.timer), free = toggle(4, "All known and free", r.free))
    // Aims.
    val ay = y + 298f
    ui.text("Aims", x + 26f, ay, 16f, Sheets.Brown, bold = true)
    if(ui.button(x + 100f, ay - 6f, 110f, 30f, "Add an aim", r.aims.size < 7, 13f)) r = r.copy(aims = r.aims :+ (Aim.Wave(5), Vector(Stack(Items.Copper, 100))))
    r.aims.zipWithIndex.foreach { case (entry, i) =>
      val by = ay + 32f + i * 40f
      ui.rect(x + 18f, by, w - 36f, 36f, Tint(0xe3cb9c))
      ui.text(s"${i + 1}", x + 26f, by + 9f, 14f, Sheets.Brown, bold = true)
      aim(entry, x + 46f, by + 4f, w) match {
        case None => r = r.copy(aims = r.aims.patch(i, Nil, 1))
        case Some(changed) => if(changed != entry && i < r.aims.size) r = r.copy(aims = r.aims.updated(i, changed))
      }
    }
    map.rules = r
  }

  private def step(x: Float, y: Float, value: Int, by: Int, least: Int, most: Int): Int = {
    val less = ui.button(x, y, 26f, 28f, "-", value > least, 14f)
    val more = ui.button(x + 76f, y, 26f, 28f, "+", value < most, 14f)
    ui.centered(value.toString, x + 51f, y + 6f, 14f, Tint.Ink, bold = true)
    MathUtil.clamp(value + (if(more) by else 0) - (if(less) by else 0), least, most)
  }
  // The place of the next one, for what is clicked on: backwards on its left half.
  private def turn(x: Float, y: Float, w: Float, n: Int, now: Int): Int = {
    ui.catchMouse(x, y, w, 28f)
    ui.nine(ui.art(if(ui.over(x, y, w, 28f)) "slot_hot" else "slot"), x, y, w, 28f, 8f)
    if(ui.clicked(x, y, w / 2f, 28f)) (now + n - 1) % n else if(ui.clicked(x + w / 2f, y, w / 2f, 28f)) (now + 1) % n else now
  }

  private def aim(entry: (Aim, Vector[Stack]), x: Float, y: Float, w: Float): Option[(Aim, Vector[Stack])] = {
    val (a, reward) = entry
    val kinds = Vector("Bring goods", "Build", "Learn", "Hold out", "Slay", "Burn hearts", "Have automata", "See a season", "Send by wagon")
    val kind = a.ordinal
    val next = turn(x, y, 140f, kinds.size, kind)
    ui.centered(kinds(kind), x + 70f, y + 6f, ui.fitting(kinds(kind), 130f, 13f), Tint.Cream, bold = true)
    val ox = x + 148f
    def block(b: Block): Block = {
      val i = turn(ox, y, 190f, blocks.size, scala.math.max(0, blocks.indexOf(b)))
      Portrait.draw(ui.raw, ui.art, blocks(i), (ox + 16f) * ui.scale, (y + 14f) * ui.scale, 22f * ui.scale, 0, sgl.engine.gameobject.Color.White)
      ui.text(blocks(i).label, ox + 32f, y + 6f, ui.fitting(blocks(i).label, 150f, 13f), Tint.Cream, bold = true)
      blocks(i)
    }
    def item(px: Float, i: Item): Item = {
      val k = turn(px, y, 40f, Items.Count, i.id)
      ui.picture(ui.art.items(k), px + 10f, y + 4f, 20f, 20f)
      Items.all(k)
    }
    val nx = ox + 198f
    val changed: Aim =
      if(next != kind) next match {
        case 0 => Aim.Deliver(Items.Copper, 100)
        case 1 => Aim.Build(Blocks.Smelter, 1)
        case 2 => Aim.Learn(Blocks.Smelter)
        case 3 => Aim.Wave(5)
        case 4 => Aim.Slay(50)
        case 5 => Aim.Burn(1)
        case 6 => Aim.Crew(UnitKinds.Beetle, 2)
        case 7 => Aim.Reach(1, Season.Winter)
        case _ => Aim.Send(100)
      } else a match {
        case Aim.Deliver(i, n) => Aim.Deliver(item(ox, i), step(nx, y, n, 10, 10, 5000))
        case Aim.Build(b, n) => Aim.Build(block(b), step(nx, y, n, 1, 1, 50))
        case Aim.Learn(b) => Aim.Learn(block(b))
        case Aim.Wave(n) => Aim.Wave(step(nx, y, n, 1, 1, 100))
        case Aim.Slay(n) => Aim.Slay(step(nx, y, n, 10, 10, 5000))
        case Aim.Burn(n) => Aim.Burn(step(nx, y, n, 1, 1, scala.math.max(1, map.nests.size)))
        case Aim.Crew(k, n) =>
          val i = turn(ox, y, 190f, crew.size, scala.math.max(0, crew.indexOf(k)))
          ui.picture(ui.art.bodies(crew(i).id), ox + 6f, y + 4f, 20f, 20f, sgl.engine.gameobject.Color.White, -MathUtil.Pi / 2f)
          ui.text(crew(i).label, ox + 32f, y + 6f, 13f, Tint.Cream, bold = true)
          Aim.Crew(crew(i), step(nx, y, n, 1, 1, 30))
        case Aim.Reach(year, s) =>
          val i = turn(ox, y, 120f, 4, s.ordinal)
          ui.picture(ui.art.seasons(i), ox + 6f, y + 4f, 20f, 20f)
          ui.text(Season.fromOrdinal(i).label, ox + 32f, y + 6f, 13f, Tint.Cream, bold = true)
          Aim.Reach(step(nx, y, year, 1, 1, 9), Season.fromOrdinal(i))
        case Aim.Send(n) => Aim.Send(step(nx, y, n, 50, 50, 5000))
      }
    // What comes of it: one kind of goods.
    val rx = nx + 130f
    ui.text("gives", rx, y + 6f, 13f, Tint.Ink, bold = true)
    val before = reward.headOption.getOrElse(Stack(Items.Copper, 0))
    val what = item(rx + 54f, before.item)
    val amount = step(rx + 102f, y, before.amount, 20, 0, 2000)
    val dropped = ui.button(x + w - 104f, y, 30f, 28f, "", icon = ui.art.icon("remove"))
    if(dropped) None else Some((changed, if(amount > 0) Vector(Stack(what, amount)) else Vector.empty))
  }

  /** Typed into the name. */
  def typed(text: String): Unit = map.rules = map.rules.copy(name = (map.rules.name + text.filter(c => c >= ' ' && c != '#' && c != '/')).take(28))
  def key(k: Keys.Key): Unit = if(k == Keys.Backspace) map.rules = map.rules.copy(name = map.rules.name.dropRight(1))
}
