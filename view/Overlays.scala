package hearthwheel
package core

import scala.util.{Failure, Success}

import sgl.engine.gameobject.{Color, GameContext}
import hearthwheel.model._
import hearthwheel.model.MathUtil._

private object Sheets {
  /** A large sheet of paper over everything, with a title and a way out. */
  def open(ui: Ui, title: String, w: Float, h: Float, close: () => Unit): (Float, Float) = {
    ui.beginModal()
    ui.catchMouse(0f, 0f, ui.width, ui.height)
    ui.rect(0f, 0f, ui.width, ui.height, Color(0.08f, 0.05f, 0.09f, 0.74f))
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.paper(x, y, w, h)
    ui.text(title, x + 26f, y + 15f, 24f, Tint(0x8c3a24), bold = true)
    if(ui.button(x + w - 108f, y + 12f, 88f, 32f, "Close", size = 14f)) close()
    (x, y)
  }
  val Brown: Color = Tint(0x8c3a24)
  val Dim: Color = Tint(0x7a5a48)
}

/** Everything there is to know about blocks, goods and creatures. */
private final class CodexView(world: Valley, profile: Profile, ui: Ui, block: Block, choose: Block => Unit, close: () => Unit) {
  private val art = ui.art
  def draw(): Unit = {
    val w = scala.math.min(ui.width - 24f, 1080f)
    val h = scala.math.min(ui.height - 16f, 640f)
    val (x, y) = Sheets.open(ui, "Codex", w, h, close)
    Vector("Buildings", "Goods", "Creatures").zipWithIndex.foreach { case (label, i) =>
      if(ui.button(x + 130f + i * 124f, y + 12f, 116f, 32f, label, enabled = CodexView.tab != i, size = 14f)) CodexView.tab = i
    }
    if(block != CodexView.lastBlock) { CodexView.lastBlock = block; if(block != null) CodexView.tab = 0 }
    val gx = x + 26f
    val gy = y + 60f
    val side = x + w - 330f
    ui.line(side - 14f, gy, side - 14f, y + h - 20f, 1.5f, Tint(0xd9bd8c))
    CodexView.tab match {
      case 0 => buildings(gx, gy, side - 30f - gx, side)
      case 1 => goods(gx, gy, side)
      case _ => creatures(gx, gy, side)
    }
  }

  private def buildings(gx: Float, gy: Float, width: Float, side: Float): Unit = {
    val cell = 42f
    val columns = (width / cell).toInt
    var y = gy
    var shown = block
    Category.values.foreach { c =>
      val list = Blocks.buildable(c)
      val rows = (list.size + columns - 1) / columns
      ui.text(c.label, gx, y, 13f, Sheets.Dim, bold = true)
      list.zipWithIndex.foreach { case (b, i) =>
        val bx = gx + (i % columns) * cell
        val by = y + 18f + (i / columns) * cell
        if(ui.slot(bx, by, cell - 4f, b eq block)) choose(b)
        val known = world.knows(b) || profile.learned.contains(b)
        Portrait.draw(ui.raw, art, b, (bx + (cell - 4f) / 2f) * ui.scale, (by + (cell - 4f) / 2f) * ui.scale, (if(b.size == 1) 24f else 29f) * ui.scale, 0, if(known) Color.White else Color(0.5f, 0.45f, 0.45f, 0.8f))
        if(ui.over(bx, by, cell - 4f, cell - 4f)) shown = b
      }
      y += 20f + rows * cell
    }
    if(shown != null) {
      Portrait.draw(ui.raw, art, shown, (side + 50f) * ui.scale, (gy + 50f) * ui.scale, 78f * ui.scale, 0, Color.White)
      ui.text(shown.label, side + 104f, gy + 14f, 20f, Sheets.Brown, bold = true)
      ui.text(s"${shown.size} by ${shown.size}", side + 104f, gy + 42f, 13f, Sheets.Dim)
      var at = ui.wrapped(shown.about, side, gy + 108f, 300f, 14f, Tint.Ink) + 8f
      Facts.of(shown).foreach { f => at = ui.wrapped(f, side, at, 300f, 13f, Sheets.Dim, bold = true) + 1f }
      at += 8f
      shown.cost.foreach { s =>
        ui.picture(art.items(s.item.id), side, at, 18f, 18f)
        ui.text(s"${s.amount} ${s.item.label}", side + 24f, at + 1f, 13f, Tint.Ink)
        at += 21f
      }
      shown.role match {
        case t: Role.Turret if t.ammo.nonEmpty =>
          at += 6f
          t.ammo.foreach { case (item, shot) =>
            ui.picture(art.items(item.id), side, at, 18f, 18f)
            val parts = Vector(
              if(shot.damage > 0f) s"${shot.damage.toInt} damage" else "",
              if(shot.splash > 0f) s"${shot.splashDamage.toInt} around" else "",
              if(shot.pierce > 0 && shot.pierce < 90) s"through ${shot.pierce}" else if(shot.pierce >= 90) "through all" else "",
              if(shot.burn > 0f) "burns" else "", if(shot.slow > 0f) "slows" else "", if(shot.homing > 0f) "follows" else ""
            ).filter(_.nonEmpty).mkString(", ")
            ui.text(parts, side + 24f, at + 1f, 13f, Tint.Ink)
            at += 21f
          }
        case _ => ()
      }
    } else { val _ = ui.wrapped("Choose a building to read about it. Pressed over one in the valley, the codex opens at its page.", side, gy + 10f, 290f, 14f, Sheets.Dim) }
  }

  private def goods(gx: Float, gy: Float, side: Float): Unit = {
    var shown: Item = null
    Items.all.foreach { i =>
      val bx = gx + (i.id % 2) * 300f
      val by = gy + (i.id / 2) * 72f
      ui.nine(art(if(ui.over(bx, by, 288f, 64f)) "slot_hot" else "slot"), bx, by, 288f, 64f, 10f)
      ui.picture(art.items(i.id), bx + 12f, by + 12f, 40f, 40f)
      ui.text(i.label, bx + 64f, by + 8f, 16f, Tint.Cream, bold = true)
      val _ = ui.wrapped(i.about, bx + 64f, by + 28f, 214f, 12f, Tint.Soft)
      if(ui.over(bx, by, 288f, 64f)) shown = i
    }
    if(shown != null) {
      val item = shown
      ui.picture(art.items(item.id), side, gy, 64f, 64f)
      ui.text(item.label, side + 78f, gy + 12f, 20f, Sheets.Brown, bold = true)
      var at = gy + 78f
      val facts = Vector(
        if(item.hardness > 0) s"Dug out, hardness ${item.hardness}" else "",
        if(item.burn > 0f) s"Burns for ${Layout.oneDecimal(item.burn)} times coal" else "",
        if(Items.explosive(item) > 0f) "Explodes" else ""
      ).filter(_.nonEmpty)
      facts.foreach { f => ui.text(f, side, at, 14f, Sheets.Dim, bold = true); at += 20f }
      def list(title: String, blocks: Vector[Block]): Unit = if(blocks.nonEmpty) {
        at += 8f
        ui.text(title, side, at, 14f, Sheets.Brown, bold = true)
        at += 22f
        blocks.zipWithIndex.foreach { case (b, k) =>
          Portrait.draw(ui.raw, art, b, (side + 16f + (k % 8) * 36f) * ui.scale, (at + 16f + (k / 8) * 36f) * ui.scale, 30f * ui.scale, 0, Color.White)
        }
        at += ((blocks.size + 7) / 8) * 36f
      }
      list("Made by", Blocks.all.filter(_.role match { case c: Role.Crafter => c.out.item eq item; case Role.Tap(_) => item eq Items.Resin; case Role.Garden(_) => item eq Items.Cloth; case _ => false }))
      list("Goes into", Blocks.all.filter(_.role match { case c: Role.Crafter => c.in.exists(_.item eq item); case Role.Press(in, _, _, _) => in.exists(_.item eq item); case _ => false }))
      list("Shot by", Blocks.all.filter(_.role match { case t: Role.Turret => t.ammo.exists(_._1 eq item); case _ => false }))
    }
  }

  private def creatures(gx: Float, gy: Float, side: Float): Unit = {
    var shown: UnitKind = null
    def row(title: String, kinds: Vector[UnitKind], y: Float): Float = {
      ui.text(title, gx, y, 13f, Sheets.Dim, bold = true)
      val columns = 9
      kinds.zipWithIndex.foreach { case (k, i) =>
        val bx = gx + (i % columns) * 68f
        val by = y + 20f + (i / columns) * 68f
        val seen = k.friendly || profile.met.contains(k) || world.met.contains(k)
        val _ = ui.slot(bx, by, 62f, hot = false)
        ui.picture(art.bodies(k.id), bx + 8f, by + 8f, 46f, 46f, if(seen) Color.White else Color(0.12f, 0.08f, 0.1f, 0.9f), -Pi / 2f)
        if(ui.over(bx, by, 62f, 62f)) shown = if(seen) k else null
      }
      y + 28f + ((kinds.size + columns - 1) / columns) * 68f
    }
    val below = row("Automata", UnitKinds.all.filter(_.friendly), gy)
    val _ = row("The Bramble", UnitKinds.all.filterNot(_.friendly), below)
    if(shown != null) {
      val k = shown
      ui.picture(art.bodies(k.id), side, gy, 84f, 84f, Color.White, -Pi / 2f)
      ui.text(k.label, side + 98f, gy + 14f, 20f, Sheets.Brown, bold = true)
      ui.text(k.walk match { case Walk.Ground => "Walks"; case Walk.Climb => "Climbs over anything"; case Walk.Fly => "Flies"; case Walk.Float => "Sails" }, side + 98f, gy + 42f, 13f, Sheets.Dim)
      var at = ui.wrapped(k.about, side, gy + 98f, 300f, 14f, Tint.Ink) + 8f
      val arms = k.arms match {
        case Arms.Shoots(shot, range, reload) => Vector(s"${(shot.damage + shot.splashDamage).toInt} every ${Layout.oneDecimal(reload)}s, reach ${Layout.oneDecimal(range)}")
        case Arms.Bites(damage, reload) => Vector(s"Bites for ${damage.toInt} every ${Layout.oneDecimal(reload)}s")
        case Arms.Bursts(damage, _) => Vector(s"Bursts for ${damage.toInt}")
        case Arms.Drops(shot, reload) => Vector(s"Drops ${shot.splashDamage.toInt} every ${Layout.oneDecimal(reload)}s")
        case Arms.Nothing => Vector("Unarmed")
      }
      val trade = k.trade match {
        case Trade.None => Vector.empty
        case Trade.Digs(_) => Vector("Digs copper, tin, flint and coal")
        case Trade.Builds => Vector("Builds along")
        case Trade.Mends(r, n) => Vector(s"Mends ${n.toInt} a second, reach ${Layout.oneDecimal(r)}")
      }
      (Vector(s"Health ${k.hp.toInt}", s"Armor ${k.armor.toInt}", s"Speed ${Layout.oneDecimal(k.speed / 32f)} tiles a second") ++ (if(k.shield > 0f) Vector(s"Shield ${k.shield.toInt}") else Vector.empty) ++ arms ++ trade).foreach { f =>
        at = ui.wrapped(f, side, at, 300f, 14f, Sheets.Dim, bold = true) + 2f
      }
    }
  }
}
private object CodexView {
  var tab = 0
  var lastBlock: Block = null
}

/** The whole valley at once. */
private final class MapView(play: Play, ui: Ui) {
  private val world = play.world
  def draw(): Unit = {
    val s = scala.math.min(ui.height - 110f, ui.width - 280f)
    val (x, y) = Sheets.open(ui, play.world.rules.name, s + 250f, s + 84f, () => play.overlay = Overlay.None)
    val mx = x + 24f
    val my = y + 60f
    val k = s / scala.math.max(world.width, world.height)
    val T = Valley.Tile.toFloat
    ui.rect(mx - 3f, my - 3f, s + 6f, s + 6f, Tint(0x6b4634))
    ui.raw.baked(play.minimap, mx * ui.scale, my * ui.scale, k * ui.scale)
    // Where grids lack power.
    world.networks.power.foreach { g =>
      if(g.needed > 0f && g.satisfaction < 0.5f) g.members.headOption.foreach(b => ui.picture(ui.art.boltIcon, mx + b.cx / T * k - 6f, my + b.cy / T * k - 6f, 12f, 12f))
    }
    world.creatures.foreach { c =>
      val color = if(UnitKinds.isLantern(c.kind)) Tint(0xfff3a8) else if(c.team == Valley.Player) Tint.Good else Tint.Bad
      val d = if(c.kind.boss) 9f else 4f
      ui.rect(mx + c.x / T * k - d / 2f, my + c.y / T * k - d / 2f, d, d, color)
    }
    world.nests.foreach { n =>
      val d = 16f + 2f * sin(ui.time * 4f)
      ui.picture(ui.art.icon("skull"), mx + n.cx / T * k - d / 2f, my + n.cy / T * k - d / 2f, d, d)
    }
    if(world.hearth != null) ui.picture(ui.art.items(Items.Ember.id), mx + world.hearth.cx / T * k - 9f, my + world.hearth.cy / T * k - 9f, 18f, 18f)
    ui.clip(mx, my, s, s) {
      val vx = mx + play.left / T * k
      val vy = my + play.top / T * k
      val vw = play.viewWidth / T * k
      val vh = play.viewHeight / T * k
      Vector((vx, vy, vw, 1.6f), (vx, vy + vh, vw, 1.6f), (vx, vy, 1.6f, vh), (vx + vw, vy, 1.6f, vh)).foreach { case (a, b, c, d) => ui.rect(a, b, c, d, Color.White) }
    }
    ui.dragged(mx, my, s, s).foreach { case (px, py) =>
      play.following = false
      play.centerX = clamp((px - mx) / k, 0f, world.width.toFloat) * T
      play.centerY = clamp((py - my) / k, 0f, world.height.toFloat) * T
    }
    // What the colors are.
    val lx = mx + s + 20f
    Vector("Copper" -> Items.Copper.color, "Tin" -> Items.Tin.color, "Coal" -> Items.Coal.color, "Flint" -> Items.Flint.color, "Iron" -> Items.Iron.color, "Vents" -> 0xff8a3c, "Turrets" -> 0xfff0d2, "Buildings" -> 0xf0a860, "Belts" -> 0xd8a878, "The Bramble" -> 0xe0407a).zipWithIndex.foreach { case ((label, color), i) =>
      ui.rect(lx, my + i * 24f, 16f, 16f, Tint.Ink)
      ui.rect(lx + 1f, my + 1f + i * 24f, 14f, 14f, Tint(color))
      ui.text(label, lx + 24f, my + i * 24f, 14f, Tint.Ink)
    }
    val _ = ui.wrapped("Drag to look somewhere else.", lx, my + 260f, 190f, 13f, Sheets.Dim)
  }
}

/** What there has been of each item, over the last minutes. */
private final class LedgerView(play: Play, ui: Ui) {
  private val world = play.world
  def draw(): Unit = {
    val w = 820f
    val h = 560f
    val (x, y) = Sheets.open(ui, "Ledger", w, h, () => play.overlay = Overlay.None)
    ui.text(s"The last ${(world.ledger.Samples * world.ledger.Every / 60f).toInt} minutes", x + 130f, y + 24f, 13f, Sheets.Dim)
    Items.all.foreach { i =>
      val bx = x + 26f + (i.id % 2) * 390f
      val by = y + 60f + (i.id / 2) * 52f
      ui.picture(ui.art.items(i.id), bx, by + 8f, 26f, 26f)
      ui.text(i.label, bx + 34f, by + 2f, 14f, Tint.Ink, bold = true)
      ui.text(Layout.amount(world.hearthStock(i)), bx + 34f, by + 21f, 14f, Sheets.Dim, bold = true)
      val rate = world.rates(i.id)
      if(rate > 0.5f) ui.text(s"+${scala.math.round(rate)} a minute", bx + 84f, by + 22f, 12f, Tint(0x5a8a3a), bold = true)
      val h = world.ledger.history(i.id)
      val gx = bx + 180f
      val gw = 180f
      ui.rect(gx, by, gw, 42f, Tint(0xe3cb9c))
      if(h.size > 1) {
        val most = scala.math.max(10, h.max).toFloat
        val step = gw / world.ledger.Samples
        val start = gx + gw - h.size * step
        var k = 1
        while(k < h.size) {
          val y0 = by + 41f - h(k - 1) / most * 38f
          val y1 = by + 41f - h(k) / most * 38f
          ui.line(start + (k - 1) * step, y0, start + k * step, y1, 2f, Tint(i.color))
          k += 1
        }
      }
    }
    val by = y + h - 66f
    val grids = world.networks.power.filter(g => g.produced > 0f || g.needed > 0f)
    ui.text(s"${grids.size} power grid${if(grids.size == 1) "" else "s"}, ${Layout.oneDecimal(grids.map(_.produced).sum)} made, ${Layout.oneDecimal(grids.map(_.needed).sum)} wanted", x + 26f, by, 14f, Tint.Ink, bold = true)
    ui.text(s"${world.built} built, ${world.lost} lost, ${world.slain} slain in ${Layout.time(world.time)}", x + 26f, by + 24f, 14f, Sheets.Dim)
  }
}

/** Layouts that were copied and kept. */
private final class LayoutsView(play: Play, ui: Ui) {
  def draw(): Unit = {
    val w = 820f
    val h = 540f
    val (x, y) = Sheets.open(ui, "Layouts", w, h, () => play.overlay = Overlay.None)
    play.tool match {
      case Tool.Stamp(plans) => if(ui.button(x + 150f, y + 12f, 190f, 32f, "Keep what is in hand", size = 14f)) {
        play.profile.layouts = play.profile.layouts :+ plans
        play.profile.save()
        play.sound(SoundKind.Place)
      }
      case _ => if(ui.width > 900f) ui.text(s"Copy an area with ${play.profile.bindings.name(Command.Copy)}, then come here to keep it.", x + 150f, y + 22f, 13f, Sheets.Dim)
    }
    if(ui.button(x + w - 220f, y + 12f, 100f, 32f, "Paste", size = 14f)) play.services.paste { t =>
      Share.readLayout(t) match {
        case Some(plans) =>
          play.profile.layouts = play.profile.layouts :+ plans
          play.profile.save()
          play.sound(SoundKind.Place)
        case None => play.say("There is no layout on the clipboard", Tint.Bad)
      }
    }
    play.profile.layouts.take(12).zipWithIndex.foreach { case (plans, i) =>
      val bx = x + 26f + (i % 4) * 194f
      val by = y + 60f + (i / 4) * 154f
      val s = 184f
      ui.nine(ui.art(if(ui.over(bx, by, s, 144f)) "slot_hot" else "slot"), bx, by, s, 144f, 10f)
      ui.catchMouse(bx, by, s, 144f)
      val x0 = plans.map(_.x).min
      val y0 = plans.map(_.y).min
      val x1 = plans.map(p => p.x + p.block.size).max
      val y1 = plans.map(p => p.y + p.block.size).max
      val k = scala.math.min(160f / (x1 - x0), 96f / (y1 - y0))
      val k2 = scala.math.min(k, 26f)
      plans.foreach { p =>
        val size = p.block.size * k2
        val px = bx + s / 2f + (p.x + p.block.size / 2f - (x0 + x1) / 2f) * k2
        val py = by + 58f + (p.y + p.block.size / 2f - (y0 + y1) / 2f) * k2
        Portrait.draw(ui.raw, ui.art, p.block, px * ui.scale, py * ui.scale, size * ui.scale, p.rotation, if(play.world.knows(p.block)) Color.White else Color(1f, 0.5f, 0.5f, 0.7f))
      }
      ui.text(s"${plans.size} buildings", bx + 12f, by + 118f, 13f, Tint.Soft)
      if(ui.button(bx + s - 38f, by + 110f, 28f, 26f, "", icon = ui.art.icon("remove"))) {
        play.profile.layouts = play.profile.layouts.patch(i, Nil, 1)
        play.profile.save()
      } else if(ui.button(bx + s - 70f, by + 110f, 28f, 26f, "", icon = ui.art.icon("share"))) {
        play.services.copy(Share.layout(plans))
        play.say("Copied", Tint.Good)
      } else if(ui.clicked(bx, by, s, 108f)) {
        play.tool = Tool.Stamp(plans)
        play.overlay = Overlay.None
      }
    }
  }
}

/** The waves to come. */
private final class WavesView(play: Play, ui: Ui) {
  private val world = play.world
  def draw(): Unit = {
    val w = 900f
    val h = 560f
    val (x, y) = Sheets.open(ui, "What is coming", w, h, () => play.overlay = Overlay.None)
    for(k <- 1 to 9) {
      val n = world.waves.number + k
      val season = world.waves.seasonOf(k)
      val by = y + 58f + (k - 1) * 52f
      if(k % 2 == 1) ui.rect(x + 18f, by - 4f, w - 36f, 48f, Tint(0xe3cb9c))
      ui.picture(ui.art.seasons(season.ordinal), x + 26f, by + 4f, 30f, 30f)
      ui.text(s"Wave $n", x + 68f, by + 3f, 15f, Sheets.Brown, bold = true)
      val seconds = scala.math.max(0f, world.waves.countdown) + (k - 1) * world.rules.waveSeconds
      ui.text(if(world.rules.timer) "in " + Layout.time(seconds) else "on call", x + 68f, by + 22f, 12f, Sheets.Dim)
      world.waves.compose(n, season).zipWithIndex.foreach { case ((kind, count), i) =>
        val known = world.met.contains(kind) || play.profile.met.contains(kind)
        ui.picture(ui.art.bodies(kind.id), x + 160f + i * 66f, by + 3f, 34f, 34f, if(known) Color.White else Color(0.15f, 0.1f, 0.12f, 0.9f), -Pi / 2f)
        ui.text(count.toString, x + 196f + i * 66f, by + 18f, 14f, Tint.Ink, bold = true)
      }
      if(n % 5 == 0) ui.picture(ui.art.crown, x + w - 60f, by + 8f, 24f, 24f)
      val shield = world.waves.shieldAt(n)
      if(shield > 0f) { ui.picture(ui.art.icon("shield"), x + w - 118f, by + 8f, 22f, 22f); ui.text(shield.toInt.toString, x + w - 92f, by + 11f, 13f, Tint.Ink, bold = true) }
    }
    if(!world.rules.peaceful && ui.button(x + w - 310f, y + 12f, 190f, 32f, "Call the next one now", size = 14f)) world.waves.launchNow()
  }
}

private object DeedsView {
  def draw(ui: Ui, profile: Profile, close: () => Unit): Unit = {
    val w = 900f
    val h = 580f
    val (x, y) = Sheets.open(ui, s"Deeds  ${profile.deeds.size} of ${Deeds.all.size}", w, h, close)
    Deeds.all.zipWithIndex.foreach { case (d, i) =>
      val bx = x + 26f + (i % 3) * 286f
      val by = y + 58f + (i / 3) * 46f
      val done = profile.deeds.contains(d.key)
      ui.nine(ui.art("slot"), bx, by, 278f, 43f, 10f, if(done) Color.White else Tint.alpha(0.65f))
      ui.picture(ui.art.icon("deed"), bx + 8f, by + 6f, 32f, 32f, if(done) Color.White else Color(0.3f, 0.28f, 0.3f, 0.7f))
      ui.text(d.label, bx + 48f, by + 5f, 14f, if(done) Tint.Gold else Tint(0x9a8a84), bold = true)
      ui.text(d.text, bx + 48f, by + 24f, 12f, if(done) Tint.Soft else Tint(0x8a7a74))
    }
  }
}

private object SavesView {
  def draw(ui: Ui, profile: Profile, play: Option[Play], context: GameContext, close: () => Unit): Unit = {
    val w = 680f
    val h = 500f
    val (x, y) = Sheets.open(ui, "Saved games", w, h, close)
    play.foreach { p =>
      if(p.world.outcome == Outcome.Playing && ui.button(x + w - 280f, y + 12f, 160f, 32f, "Save this one", profile.games.size < 10, 14f)) {
        profile.keepGame(p.world, p.valley)
        p.say("Saved", Tint.Good)
        p.sound(SoundKind.Place)
      }
    }
    if(ui.button(x + w - (if(play.isEmpty) 220f else 390f), y + 12f, 100f, 32f, "Paste", profile.games.size < 10, 14f)) profile.paste { t =>
      Share.readGame(t).foreach { case (valley, rules, text) => Codec.load(text, rules).foreach(w => profile.keepGame(rules, valley, w, text)) }
    }
    if(profile.games.isEmpty) ui.text("None yet.", x + 26f, y + 66f, 15f, Sheets.Dim)
    profile.games.reverse.take(10).zipWithIndex.foreach { case (g, i) =>
      val by = y + 58f + i * 42f
      if(i % 2 == 0) ui.rect(x + 18f, by, w - 36f, 40f, Tint(0xe3cb9c))
      ui.picture(ui.art.seasons(g.season), x + 28f, by + 6f, 28f, 28f)
      ui.text(g.name, x + 68f, by + 10f, 16f, Tint.Ink, bold = true)
      ui.text(s"Year ${g.year}, wave ${g.wave}", x + 300f, by + 12f, 14f, Sheets.Dim)
      if(ui.button(x + w - 190f, by + 5f, 90f, 30f, "Load", size = 14f)) profile.gameText(g).map(Codec.load(_, g.rules)) match {
        case Some(Success(world)) => context.runtime.loadScene(new PlayScene(() => world, g.valley))
        case Some(Failure(_)) | None => play.foreach(_.say("That save is damaged", Tint.Bad))
      }
      if(ui.button(x + w - 246f, by + 5f, 50f, 30f, "", icon = ui.art.icon("share"))) profile.gameText(g).foreach(t => profile.copy(Share.game(g.rules, g.valley, t)))
      if(ui.button(x + w - 90f, by + 5f, 50f, 30f, "", icon = ui.art.icon("remove"))) profile.dropGame(g)
    }
  }
}
