package hearthwheel
package core

import sgl.engine.gameobject.Color
import hearthwheel.model._

/** The cards of a loom, to punch. */
private final class LoomView(play: Play, ui: Ui, loom: LoomBuilding) {
  private val world = play.world
  private val step = if(play.controls.shift) 10 else 1

  def draw(): Unit = {
    val w = scala.math.min(ui.width - 20f, 1080f)
    val h = scala.math.min(ui.height - 20f, 600f)
    val (x, y) = Sheets.open(ui, "Cards", w, h, () => play.overlay = Overlay.None)
    val full = loom.cards.size >= Card.Most
    if(ui.button(x + 110f, y + 12f, 130f, 32f, "Add a card", !full, 14f)) loom.cards = loom.cards :+ Card.blank
    if(ui.button(x + 250f, y + 12f, 100f, 32f, "Copy", loom.cards.nonEmpty, 14f)) { play.services.copy(Share.cards(loom)); play.say("Copied", Tint.Good) }
    if(ui.button(x + 360f, y + 12f, 100f, 32f, "Paste", size = 14f)) play.services.paste(t => Share.readCards(t) match {
      case Some(cards) => loom.cards = cards
      case None => play.say("There are no cards on the clipboard", Tint.Bad)
    })
    loom.cards.zipWithIndex.foreach { case (c, i) => card(c, i, x + 20f, y + 58f + i * 54f, w - 40f) }
    if(loom.cards.isEmpty) { val _ = ui.wrapped("No cards yet. Each one says: if what it looks at compares like this, then do that. All of them are read twice a second, from the top.", x + 26f, y + 70f, w - 60f, 15f, Sheets.Dim) }
    val by = y + h - 38f
    ui.text("Drums", x + 26f, by + 4f, 14f, Sheets.Brown, bold = true)
    world.drums.zipWithIndex.foreach { case (v, d) => ui.text(s"${LoomView.drum(d)}  $v", x + 34f + ui.textWidth("Drums", 14f, bold = true) + d * 90f, by + 4f, 14f, Tint.Ink, bold = true) }
    ui.right("Hold Shift to count by ten. Tap buildings with the loom chosen to link them.", x + w - 26f, by + 5f, 13f, Sheets.Dim)
  }

  // Goes on with the right half, back with the left one.
  private def cycle(x: Float, y: Float, w: Float, label: String, n: Int, now: Int): Int = {
    val all = scala.math.max(1, n)
    ui.catchMouse(x, y, w, 32f)
    ui.nine(ui.art(if(ui.over(x, y, w, 32f)) "slot_hot" else "slot"), x, y, w, 32f, 8f)
    ui.clip(x + 4f, y, w - 8f, 32f)(ui.centered(label, x + w / 2f, y + 8f, 12f, Tint.Cream, bold = true))
    if(ui.clicked(x, y, w / 2f, 32f)) { play.sound(SoundKind.Click, 0.3f); (now + all - 1) % all }
    else if(ui.clicked(x + w / 2f, y, w / 2f, 32f)) { play.sound(SoundKind.Click, 0.3f); (now + 1) % all }
    else now
  }
  private def number(x: Float, y: Float, w: Float, value: Int, by: Int): Int = {
    val less = ui.button(x, y, 26f, 32f, "-", size = 14f)
    val more = ui.button(x + w - 26f, y, 26f, 32f, "+", size = 14f)
    ui.centered(value.toString, x + w / 2f, y + 8f, 14f, Tint.Ink, bold = true)
    MathUtil.clamp(value + (if(more) by * step else 0) - (if(less) by * step else 0), -9999, 9999)
  }
  private def item(x: Float, y: Float, now: Int): Int = {
    val v = cycle(x, y, 44f, "", Items.Count, MathUtil.clamp(now, 0, Items.Count - 1))
    ui.picture(ui.art.items(v), x + 11f, y + 5f, 22f, 22f)
    v
  }
  private def link(x: Float, y: Float, w: Float, now: Int): Int = {
    val v = cycle(x, y, w, "", loom.MostTies, MathUtil.clamp(now, 0, loom.MostTies - 1))
    val b = loom.tied(v)
    ui.text((v + 1).toString, x + 9f, y + 8f, 12f, Tint.Gold, bold = true)
    if(b != null) {
      val block = b match { case s: SiteBuilding => s.target; case o => o.block }
      Portrait.draw(ui.raw, ui.art, block, (x + w / 2f + 6f) * ui.scale, (y + 16f) * ui.scale, 24f * ui.scale, 0, Color.White)
    } else ui.text("none", x + 22f, y + 9f, ui.fitting("none", w - 28f, 11f, bold = false), Tint.Soft)
    v
  }

  private def card(c: Card, i: Int, x: Float, y: Float, w: Float): Unit = {
    val lit = loom.acted == i && loom.fired > 0f
    ui.rect(x, y, w, 48f, if(lit) Tint(0xffe2a0) else Tint(0xe3cb9c))
    ui.text(s"${i + 1}", x + 8f, y + 15f, 15f, Sheets.Brown, bold = true)
    var at = x + 28f
    val cy = y + 8f
    ui.text("If", at, y + 15f, 14f, Tint.Ink, bold = true)
    at += 18f
    val sense = Sense.fromOrdinal(cycle(at, cy, 196f, c.sense.label, Sense.values.length, c.sense.ordinal))
    at += 200f
    var of = c.of
    sense.pick match {
      case Pick.Item => of = item(at, cy, of)
      case Pick.Link => of = link(at, cy, 54f, of)
      case Pick.Drum => of = cycle(at, cy, 44f, LoomView.drum(of), 4, of & 3)
      case _ => ()
    }
    at += 58f
    var compare = c.compare
    var value = c.value
    if(sense != Sense.Always) {
      compare = cycle(at, cy, 86f, Card.Compares(c.compare), 3, c.compare)
      value = number(at + 90f, cy, 108f, c.value, sense match { case Sense.Goods | Sense.Held => 10; case Sense.Health | Sense.Power | Sense.Hearth | Sense.TurnIn | Sense.WaveIn => 5; case _ => 1 })
    }
    at += 206f
    ui.text("then", at, y + 15f, 14f, Tint.Ink, bold = true)
    at += 40f
    val doing = Doing.fromOrdinal(cycle(at, cy, 180f, c.doing.label, Doing.values.length, c.doing.ordinal))
    at += 184f
    var on = c.on
    var amount = c.amount
    doing.pick match {
      case Pick.Link => on = link(at, cy, 54f, on)
      case Pick.LinkItem => on = link(at, cy, 54f, on); amount = item(at + 58f, cy, amount)
      case Pick.Drum => on = cycle(at, cy, 44f, LoomView.drum(on), 4, on & 3)
      case Pick.DrumValue => on = cycle(at, cy, 44f, LoomView.drum(on), 4, on & 3); amount = number(at + 50f, cy, 108f, amount, 1)
      case _ => ()
    }
    val now = Card(sense, of, compare, value, doing, on, amount)
    if(ui.button(x + w - 36f, cy + 2f, 28f, 28f, "", icon = ui.art.icon("remove"))) loom.cards = loom.cards.patch(i, Nil, 1)
    else if(now != c && i < loom.cards.size) loom.cards = loom.cards.updated(i, now)
  }
}
private object LoomView {
  def drum(i: Int): String = Vector("A", "B", "C", "D")(i & 3)
}

/** What can be put on the clipboard, and taken from it. */
private object Share {
  private val LayoutMark = "hearthwheel-layout/"
  private val MapMark = "hearthwheel-map/"
  private val GameMark = "hearthwheel-game/"
  private val CardMark = "hearthwheel-cards/"
  private def after(mark: String, text: String): Option[String] = { val t = text.trim; if(t.startsWith(mark)) Some(t.drop(mark.length)) else None }

  def layout(plans: Vector[Plan]): String = LayoutMark + LayoutCodec.save(plans)
  def readLayout(text: String): Option[Vector[Plan]] = after(LayoutMark, text).flatMap(LayoutCodec.load)

  def map(m: HandMap): String = MapMark + m.save
  def readMap(text: String): Option[HandMap] = after(MapMark, text).flatMap(HandMap.load(_).toOption)

  def cards(loom: LoomBuilding): String = CardMark + Card.write(loom.cards)
  def readCards(text: String): Option[Vector[Card]] = after(CardMark, text).map(Card.read)

  def game(rules: Rules, valley: Int, text: String): String = {
    val out = new Writer
    RulesCodec.write(rules, out)
    s"$GameMark$valley#${out.result}#$text"
  }
  /** The valley of the campaign it is of, its rules, and its text, which can be loaded. */
  def readGame(text: String): Option[(Int, Rules, String)] = after(GameMark, text).flatMap { t =>
    scala.util.Try {
      val Array(valley, rules, world) = t.split("#", 3): @unchecked
      val read = RulesCodec.read(new Reader(rules))
      Codec.load(world, read).get
      (if(valley.toInt >= 0 && valley.toInt < Valleys.all.size) valley.toInt else -1, read, world)
    }.toOption
  }
}
