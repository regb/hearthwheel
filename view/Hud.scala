package hearthwheel
package core

import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d.Render2D
import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** Everything over the world: what there is, what to build, what to do next. */
object HudSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.hud")
  private var mapWait = 0f
  private var mapRevision = -1

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    mapWait -= context.dt
    if(mapWait <= 0f) {
      mapWait = 0.7f
      val stamp = play.world.revision * 4 + play.world.seasonIndex
      if(stamp != mapRevision) { mapRevision = stamp; play.minimap.invalidate() }
    }
    val dt = context.dt
    context.global[Render2D].screen(10) { p =>
      val ui = play.ui
      ui.begin(p, play.services.screen, Layout.playScale(play), play.controls.mouseX, play.controls.mouseY, dt)
      new Hud(context, play, ui).draw()
      ui.end()
    }
  }
}

object Layout {
  /** As large as the screen allows, then as the player likes. */
  def scale(services: Services, profile: Profile): Float = {
    val screen = services.screen
    val fit = scala.math.min(screen.x / 1180f, screen.y / 680f)
    clamp(fit, 0.5f, 2.5f) * profile.uiScale
  }
  /** Over a valley with no sheet open there is less to fit, so fingers get larger buttons. */
  def playScale(play: Play): Float = {
    val plain = scale(play.services, play.profile)
    if(!play.controls.touch || play.overlay != Overlay.None) plain
    else {
      val screen = play.services.screen
      scala.math.max(plain, clamp(scala.math.min(screen.x / 860f, screen.y / 440f), 0.5f, 2.5f) * play.profile.uiScale)
    }
  }
  def time(seconds: Float): String = {
    val s = scala.math.max(0, seconds.toInt)
    val rest = s % 60
    s"${s / 60}:${if(rest < 10) "0" else ""}$rest"
  }
  def amount(n: Int): String = if(n >= 10000) s"${n / 1000}k" else if(n >= 1000) s"${n / 1000}.${(n % 1000) / 100}k" else n.toString
  def oneDecimal(v: Float): String = { val t = scala.math.round(v * 10f); s"${t / 10}.${scala.math.abs(t % 10)}" }
}

private final class Hud(context: GameContext, play: Play, ui: Ui) {
  private val world = play.world
  private val art = play.art
  // What the pointer is over, described next to the build menu.
  private var tip: Block = null

  def draw(): Unit = if(!play.hidden || play.overlay != Overlay.None) {
    if(play.overlay == Overlay.None || play.overlay == Overlay.End) {
      vignette()
      stock()
      dial()
      topRight()
      messages()
      arrows()
      toasts()
      boss()
      goal()
      if(play.overlay == Overlay.None) {
        if(play.tool == Tool.Command) crewPanel()
        cargo()
        buildMenu()
        if(play.controls.touch) fingers()
        if(play.selected != null && play.selected.alive) inspect(play.selected)
        else hint()
        banner()
        if(play.paused) ui.centered("Paused", ui.width / 2f, ui.height * 0.36f, 40f, Tint.Cream, bold = true)
      }
      if(play.profile.stats) statistics()
    }
    play.overlay match {
      case Overlay.Menu => menu()
      case Overlay.Settings => Settings.draw(ui, play.profile, () => play.overlay = Overlay.Menu, () => play.overlay = Overlay.Keys)
      case Overlay.Learn => new LearnView(play, ui).draw()
      case Overlay.Help => help()
      case Overlay.End => ending()
      case Overlay.Keys => Settings.keys(ui, play.profile, play.binding, c => play.binding = c, () => play.overlay = Overlay.Settings)
      case Overlay.Codex => new CodexView(play.world, play.profile, ui, play.codexBlock, b => play.codexBlock = b, () => play.overlay = Overlay.None).draw()
      case Overlay.Map => new MapView(play, ui).draw()
      case Overlay.Ledger => new LedgerView(play, ui).draw()
      case Overlay.Layouts => new LayoutsView(play, ui).draw()
      case Overlay.Waves => new WavesView(play, ui).draw()
      case Overlay.Deeds => DeedsView.draw(ui, play.profile, () => play.overlay = Overlay.Menu)
      case Overlay.Saves => SavesView.draw(ui, play.profile, Some(play), context, () => play.overlay = Overlay.Menu)
      case Overlay.Loom => play.selected match {
        case loom: LoomBuilding if loom.alive => new LoomView(play, ui, loom).draw()
        case _ => play.overlay = Overlay.None
      }
      case Overlay.None => ()
    }
  }

  private def vignette(): Unit = {
    val h = world.hearth
    if(h != null && h.hitAgo < 2f) {
      val a = (2f - h.hitAgo) / 2f * (0.35f + 0.15f * sin(play.time * 9f))
      ui.picture(art("vignette"), 0f, 0f, ui.width, ui.height, Color(1f, 0.25f, 0.1f, a))
    }
  }

  // ------------------------------------------------------------------ goods

  private def stock(): Unit = {
    val shown = Items.all.filter(i => world.hearthStock(i) > 0 || world.deliveredCounts(i.id) > 0)
    val w = 128f
    val h = 16f + shown.size * 21f
    if(shown.nonEmpty) ui.panel(8f, 8f, w, h, 0.92f)
    shown.zipWithIndex.foreach { case (item, i) =>
      val y = 16f + i * 21f
      ui.picture(art.items(item.id), 18f, y, 20f, 20f)
      ui.text(if(world.rules.free) "free" else Layout.amount(world.hearthStock(item)), 44f, y + 1f, 14f, Tint.Cream, bold = true)
      val rate = world.rates(item.id)
      if(rate > 0.5f) ui.right("+" + scala.math.round(rate).toString, w - 2f, y + 2f, 12f, Tint.Good)
      if(ui.over(8f, y, w, 21f)) {
        ui.paper(w + 16f, y - 6f, 250f, 76f)
        ui.text(item.label, w + 28f, y + 2f, 15f, Tint.Ink, bold = true)
        val _ = ui.wrapped(item.about, w + 28f, y + 22f, 226f, 13f, Tint(0x5a4038))
      }
    }
  }

  // ------------------------------------------------------------------- year

  private def dial(): Unit = {
    val w = 330f
    val x = ui.width / 2f - w / 2f
    ui.panel(x, 6f, w, 62f, 0.92f)
    val season = world.season
    val cx = x + 38f
    val cy = 37f
    ui.picture(art.seasons(season.ordinal), cx - 17f, cy - 17f, 34f, 34f)
    // The year as a ring of ticks around it.
    val total = world.rules.seasons.sum
    var t = 0
    while(t < 40) {
      val share = t / 40f
      var acc = 0f
      var s = world.rules.startSeason
      var at = 0f
      var k = 0
      while(k < 4) {
        val len = world.rules.seasons((world.rules.startSeason + k) % 4) / total
        if(share >= acc) { s = (world.rules.startSeason + k) % 4; at = (share - acc) / len }
        acc += len
        k += 1
      }
      val order = (s - world.rules.startSeason + 4) % 4
      val now = (world.seasonIndex - world.rules.startSeason + 4) % 4
      val past = order < now || (order == now && at <= world.seasonProgress)
      val color = Vector(Tint(0x9ccb6a), Tint(0xf4cf5a), Tint(0xd8792f), Tint(0xb7d7ee))(s)
      val a = share * Tau - Pi / 2f
      ui.picture(art.mote, cx + cos(a) * 25f - 3.5f, cy + sin(a) * 25f - 3.5f, 7f, 7f, if(past) color else color.withAlpha(0.3f))
      if(past) ui.picture(art.mote, cx + cos(a) * 25f - 2.5f, cy + sin(a) * 25f - 2.5f, 5f, 5f, color)
      t += 1
    }
    ui.text(season.label, x + 76f, 14f, 18f, Tint.Cream, bold = true)
    ui.text("Year " + world.year, x + 78f + ui.textWidth(season.label, 18f, bold = true) + 6f, 19f, 12f, Tint.Soft)
    val left = if(world.longWinter) "without end" else if(world.held > 0f) "held for " + Layout.time(world.held) else Season.fromOrdinal((world.seasonIndex + 1) % 4).label + " in " + Layout.time(world.secondsLeft)
    ui.text(left, x + 76f, 40f, 13f, if(world.secondsLeft < 20f && !world.longWinter) Tint.Gold else Tint.Soft)
    if(ui.over(x, 6f, 190f, 62f)) {
      ui.paper(x, 74f, w, 70f)
      val _ = ui.wrapped(season.hint, x + 14f, 86f, w - 28f, 14f, Tint.Ink)
    }
    val sky = world.sky.event
    if(sky != null) {
      val label = s"${sky.label}  ${Layout.time(world.sky.left)}"
      val sw = ui.textWidth(label, 13f, bold = true) + 24f
      ui.rect(ui.width / 2f - sw / 2f, 70f, sw, 22f, Tint.Ink.withAlpha(0.7f))
      ui.centered(label, ui.width / 2f, 73f, 13f, Tint(0xbfe0ff), bold = true)
    }
    if(!world.rules.peaceful) {
      val wx = x + 208f
      ui.picture(art.icon("wave"), wx, 14f, 24f, 24f)
      ui.text("Wave " + (world.waves.number + 1), wx + 30f, 15f, 14f, Tint.Cream, bold = true)
      val soon = world.waves.countdown < 10f
      val when = if(!world.rules.timer) "on call" else if(world.waves.held) s"${world.waves.foesLeft} left" else "in " + Layout.time(world.waves.countdown)
      ui.text(when, wx + 30f, 33f, 13f, if(soon) Tint.Bad else Tint.Soft)
      if(ui.clicked(wx, 6f, 120f, 62f)) play.overlay = Overlay.Waves
      if(ui.over(wx, 6f, 120f, 62f)) {
        val coming = world.waves.compose(world.waves.number + 1, world.season)
        ui.paper(wx - 70f, 74f, 190f, 24f + coming.size * 26f)
        coming.zipWithIndex.foreach { case ((kind, n), i) =>
          ui.picture(art.bodies(kind.id), wx - 58f, 84f + i * 26f, 24f, 24f)
          ui.text(s"$n  ${kind.label}", wx - 26f, 87f + i * 26f, 14f, Tint.Ink, bold = true)
        }
      }
    }
  }

  // ------------------------------------------------------------------- map

  private def topRight(): Unit = {
    val s = if(play.profile.minimap) 150f else 0f
    val x = ui.width - s - 20f
    if(play.profile.minimap) minimap(x, s)
    tools(s)
  }

  /** Where Briar Hearts and what threatens the Hearth are, off the screen. */
  private def arrows(): Unit = {
    def arrow(wx: Float, wy: Float, color: Color, size: Float): Unit = if(!play.sees(wx, wy, 0f)) {
      val p = play.toScreen(wx, wy)
      val (sx, sy) = (p.x / ui.scale, p.y / ui.scale)
      val a = atan2(sy - ui.height / 2f, sx - ui.width / 2f)
      val k = scala.math.min((ui.width / 2f - 40f) / scala.math.max(0.01f, scala.math.abs(cos(a))), (ui.height / 2f - 40f) / scala.math.max(0.01f, scala.math.abs(sin(a))))
      ui.picture(art.arrow, ui.width / 2f + cos(a) * k - size / 2f, ui.height / 2f + sin(a) * k - size / 2f, size, size, color, a)
    }
    world.creatures.foreach(c => if(c.alive && c.kind.boss && c.team == Valley.Bramble) arrow(c.x, c.y, Color(1f, 0.4f, 0.5f, 0.6f + 0.3f * sin(play.time * 6f)), 30f))
    val h = world.hearth
    if(h != null && h.hitAgo < 3f) {
      arrow(h.cx, h.cy, Color(1f, 0.6f, 0.2f, 0.9f), 34f)
      val text = "The Hearth is under attack"
      ui.centered(text, ui.width / 2f, ui.height * 0.2f, 20f, Color(1f, 0.6f, 0.45f, 0.6f + 0.4f * sin(play.time * 8f)), bold = true)
    }
    // Buildings that were hit in the last seconds, a few of them.
    var n = 0
    world.buildings.foreach { b =>
      if(n < 4 && b.team == Valley.Player && b.hitAgo < 1.2f && !b.isInstanceOf[BeltBuilding]) { n += 1; arrow(b.cx, b.cy, Color(1f, 0.85f, 0.4f, 0.5f), 18f) }
    }
  }

  private def toasts(): Unit = {
    var y = ui.height - (if(play.controls.touch) 70f else 12f)
    play.toasts.foreach { t =>
      val w = 250f
      val lines = ui.linesOf(t.text, w - 62f, 13f)
      val h = (if(t.title.isEmpty) 22f else 42f) + lines * ui.lineHeight(13f)
      val slide = clamp(scala.math.min(t.age * 5f, (7f - t.age) * 4f), 0f, 1f)
      val x = -w + (w + 8f) * slide
      y -= h + 6f
      ui.paper(x, y, w, h, catches = false)
      ui.picture(art.icon(t.icon), x + 12f, y + 11f, 30f, 30f)
      if(t.title.nonEmpty) ui.text(t.title, x + 50f, y + 10f, 15f, Tint(0x8c3a24), bold = true)
      val _ = ui.wrapped(t.text, x + 50f, y + (if(t.title.isEmpty) 11f else 31f), w - 62f, 13f, Tint.Ink)
    }
  }

  private def minimap(x: Float, s: Float): Unit = {
    ui.panel(x - 8f, 6f, s + 16f, s + 16f, 0.95f)
    val k = s / scala.math.max(world.width, world.height)
    ui.raw.baked(play.minimap, x * ui.scale, 14f * ui.scale, k * ui.scale)
    val T = Valley.Tile.toFloat
    world.creatures.foreach { c =>
      val color = if(c.kind eq UnitKinds.Lantern) Tint(0xfff3a8) else if(c.team == Valley.Player) Tint.Good else Tint.Bad
      val d = if(c.kind.boss) 5f else if(c.kind eq UnitKinds.Lantern) 4f else 2.2f
      ui.rect(x + c.x / T * k - d / 2f, 14f + c.y / T * k - d / 2f, d, d, color)
    }
    world.buildings.foreach {
      case n: NestBuilding if n.alive =>
        val d = 6f + sin(play.time * 4f)
        ui.rect(x + n.cx / T * k - d / 2f, 14f + n.cy / T * k - d / 2f, d, d, Tint(0xff4a8a))
      case _ => ()
    }
    // What the camera sees.
    ui.clip(x, 14f, s, s) {
      val vx = x + play.left / T * k
      val vy = 14f + play.top / T * k
      val vw = play.viewWidth / T * k
      val vh = play.viewHeight / T * k
      val c = Color(1f, 1f, 1f, 0.8f)
      ui.rect(vx, vy, vw, 1.2f, c)
      ui.rect(vx, vy + vh, vw, 1.2f, c)
      ui.rect(vx, vy, 1.2f, vh, c)
      ui.rect(vx + vw, vy, 1.2f, vh, c)
    }
    ui.dragged(x, 14f, s, s).foreach { case (mx, my) =>
      play.following = false
      play.centerX = clamp((mx - x) / k, 0f, world.width.toFloat) * T
      play.centerY = clamp((my - 14f) / k, 0f, world.height.toFloat) * T
    }
  }

  private def tools(s: Float): Unit = {
    val by = if(s > 0f) s + 28f else 8f
    val bs = 32f
    def tool(i: Int, icon: String, on: Boolean, tip: String): Boolean = {
      val bx = ui.width - 8f - (i + 1) * (bs + 3f)
      val done = ui.slot(bx, by, bs, on)
      ui.picture(art.icon(icon), bx + 6f, by + 6f, bs - 12f, bs - 12f)
      if(ui.over(bx, by, bs, bs)) {
        val w = ui.textWidth(tip, 13f) + 24f
        ui.paper(ui.width - 8f - w, by + bs + 4f, w, 32f)
        ui.text(tip, ui.width - 8f - w + 12f, by + bs + 12f, 13f, Tint.Ink)
      }
      done
    }
    if(tool(0, "menu", on = false, "Menu (Esc)")) play.overlay = Overlay.Menu
    if(tool(1, "learn", canLearnSomething && sin(play.time * 4f) > 0f, "Almanac (T)")) play.overlay = Overlay.Learn
    if(tool(2, if(play.paused) "play" else "pause", play.paused, "Pause (Space)")) play.paused = !play.paused
    if(tool(3, "fast", play.speed > 1, s"Speed: ${play.speed}x (V)")) play.speed = if(play.speed == 1) 2 else if(play.speed == 2) 4 else 1
    if(tool(5, "map", on = false, s"Map (${play.profile.bindings.name(Command.Map)})")) play.overlay = Overlay.Map
    if(tool(6, "codex", on = false, s"Codex (${play.profile.bindings.name(Command.Codex)})")) play.overlay = Overlay.Codex
    if(tool(7, "ledger", on = false, s"Ledger (${play.profile.bindings.name(Command.Ledger)})")) play.overlay = Overlay.Ledger
    if(tool(4, "crewmode", play.tool == Tool.Command, "Command automata (F)")) InputSystem.toggle(play, Tool.Command)
  }
  private def canLearnSomething: Boolean = Pages.all.exists(world.canLearn)

  // ---------------------------------------------------------------- messages

  private def messages(): Unit = {
    var y = if(world.sky.event != null) 98f else 84f
    play.messages.reverseIterator.foreach { m =>
      val a = clamp((6f - m.age) / 1.2f, 0f, 1f) * clamp(m.age * 5f, 0f, 1f)
      val w = ui.textWidth(m.text, 15f, bold = true)
      ui.rect(ui.width / 2f - w / 2f - 12f, y, w + 24f, 24f, Tint.Ink.withAlpha(0.55f * a))
      ui.centered(m.text, ui.width / 2f, y + 3f, 15f, m.color.withAlpha(a), bold = true)
      y += 27f
    }
  }

  private def banner(): Unit = if(play.bannerAge < 3.4f) {
    val t = play.bannerAge
    val a = clamp(scala.math.min(t * 2.5f, (3.4f - t) * 1.4f), 0f, 1f)
    val y = ui.height * 0.27f
    ui.rect(0f, y - 14f, ui.width, 78f, Tint.Ink.withAlpha(0.45f * a))
    val w = ui.textWidth(play.banner, 46f, bold = true)
    val grow = 1f + (1f - a) * 0.3f
    ui.picture(art.seasons(play.bannerIcon), ui.width / 2f - w / 2f - 74f, y - 4f, 56f * grow, 56f * grow, Tint.alpha(a), play.time * 0.3f)
    ui.centered(play.banner, ui.width / 2f, y, 46f, Tint.Cream.withAlpha(a), bold = true)
  }

  private def boss(): Unit = world.creatures.find(c => c.alive && c.kind.boss && c.team == Valley.Bramble).foreach { b =>
    val w = 360f
    val x = ui.width / 2f - w / 2f
    val y = ui.height - 46f
    if(play.overlay == Overlay.None && play.selected == null) {
      ui.outlined(b.kind.label, ui.width / 2f - ui.textWidth(b.kind.label, 17f, bold = true) / 2f, y - 21f, 17f, Tint(0xffb0a0))
      ui.bar(x, y, w, 14f, b.hp / b.maxHp, Tint(0xe0506a))
    }
  }

  // -------------------------------------------------------------------- goal

  private def goal(): Unit = {
    val x = 8f
    var y = 34f + Items.all.count(i => world.hearthStock(i) > 0 || world.deliveredCounts(i.id) > 0) * 21f
    val w = 236f
    if(play.tutorial != null && play.tutorial.title.nonEmpty) {
      val lines = ui.linesOf(play.tutorial.text, w - 28f, 13f)
      val h = 46f + lines * ui.lineHeight(13f)
      ui.paper(x, y, w, h)
      ui.text(play.tutorial.title.get, x + 14f, y + 11f, 16f, Tint(0x8c3a24), bold = true)
      ui.right(play.tutorial.progress, x + w - 14f, y + 14f, 12f, Tint(0x9c7a5a))
      val _ = ui.wrapped(play.tutorial.text, x + 14f, y + 33f, w - 28f, 13f, Tint.Ink)
      if(play.tutorial.flash > 0f) ui.rect(x, y, w, h, Color(1f, 1f, 1f, 0.5f * play.tutorial.flash))
      y += h + 6f
    } else {
      val status = world.rules.goal match {
        case Goal.Survive(years) => s"Year ${world.year} of $years"
        case Goal.Cleanse => s"${world.nests.size} Briar Hearts left"
        case Goal.Wheel =>
          if(world.longWinter) "Outlast the Rimewood"
          else world.buildings.collectFirst { case g: GreatWheelBuilding if g.alive => g } match {
            case Some(g) => s"${(0 until 4).count(g.done)} of 4 spokes"
            case None => "Learn and build the Great Wheel"
          }
        case Goal.Endless => s"${world.slain} slain"
      }
      val lines = ui.linesOf(world.rules.goal.label, w - 30f, 13f)
      val h = 22f + (lines + 1) * ui.lineHeight(13f)
      ui.panel(x, y, w, h, 0.9f)
      val below = ui.wrapped(world.rules.goal.label, x + 15f, y + 11f, w - 30f, 13f, Tint.Soft)
      ui.text(status, x + 15f, below, 13f, Tint.Gold, bold = true)
      y += h + 6f
      world.aim.foreach { case (aim, reward) =>
        val lines = ui.linesOf(aim.label, w - 50f, 13f)
        val ah = 20f + lines * ui.lineHeight(13f) + 14f + (if(reward.nonEmpty) 22f else 0f)
        ui.panel(x, y, w, ah, 0.9f)
        ui.picture(art.icon("aim"), x + 11f, y + 10f, 22f, 22f)
        var at = ui.wrapped(aim.label, x + 39f, y + 10f, w - 50f, 13f, Tint.Cream) + 3f
        ui.bar(x + 15f, at, w - 92f, 8f, aim.done(world).toFloat / aim.goal, Tint.Gold)
        ui.right(s"${Layout.amount(aim.done(world))}/${Layout.amount(aim.goal)}", x + w - 14f, at - 4f, 12f, Tint.Soft, bold = true)
        at += 14f
        var rx = x + 15f
        reward.foreach { s =>
          ui.picture(art.items(s.item.id), rx, at, 17f, 17f)
          val t = "+" + s.amount
          ui.text(t, rx + 19f, at + 1f, 12f, Tint.Good, bold = true)
          rx += 28f + ui.textWidth(t, 12f, bold = true)
        }
      }
    }
  }

  private def hint(): Unit = {
    val builder = world.builder
    val text =
      play.tool match {
        case Tool.Build(b) if play.controls.touch => s"${b.label}: " + (if(b.isBelt) "drag to lay a line" else "tap to place, drag for a row")
        case Tool.Command if play.controls.touch => "Drag to choose automata, tap to send them"
        case Tool.Stamp(_) if play.controls.touch => "Tap to place the copy"
        case Tool.Build(b) => if(b.rotates) s"${b.label}: drag to lay a line, R turns, right click to stop" else s"${b.label}: click to place, drag for a row, right click to stop"
        case Tool.Remove => "Drag over what is to be taken down"
        case Tool.Copy => "Drag over what is to be copied"
        case Tool.Stamp(_) => "Click to place the copy, R turns it"
        case Tool.Rebuild => "Drag over ruins to put them up again"
        case Tool.Command => "Drag to choose automata, right click to send them, with Shift to go on from there. Shift and a number keeps a group."
        case Tool.Hand =>
          if(play.riding != null) s"Riding a ${play.riding.kind.label}: ${play.profile.bindings.name(Command.Ride)} to get off"
          else if(world.cargoAmount > 0) s"Carrying ${world.cargoAmount} ${world.cargo.label}: click a building to hand it over"
          else if(builder.paused) "Building is stopped (B)"
          else if(builder.starved) "The Hearth lacks goods for what is planned"
          else if(builder.plans.nonEmpty && builder.active == null) s"${builder.plans.size} planned, out of reach: fly closer"
          else if(builder.plans.nonEmpty) s"${builder.plans.size} planned"
          else ""
      }
    if(text.nonEmpty && !world.creatures.exists(_.kind.boss)) {
      val cost = play.tool match { case Tool.Build(b) if !world.rules.free => b.cost; case _ => Vector.empty }
      val costW = cost.map(s => 30f + ui.textWidth(s.amount.toString, 14f, bold = true)).sum
      val w = ui.textWidth(text, 14f) + 28f + costW
      val left = ui.width / 2f - w / 2f - 80f
      ui.rect(left, ui.height - 36f, w, 26f, Tint.Ink.withAlpha(0.6f))
      ui.text(text, left + 14f, ui.height - 32f, 14f, if(builder.starved && play.tool == Tool.Hand) Tint.Bad else Tint.Cream)
      var at = left + 24f + ui.textWidth(text, 14f)
      cost.foreach { s =>
        ui.picture(art.items(s.item.id), at, ui.height - 32f, 17f, 17f)
        ui.text(s.amount.toString, at + 19f, ui.height - 32f, 14f, if(world.hearthStock(s.item) >= s.amount) Tint.Good else Tint.Bad, bold = true)
        at += 30f + ui.textWidth(s.amount.toString, 14f, bold = true)
      }
    }
  }

  private def statistics(): Unit = {
    val lines = Vector(
      s"${scala.math.round(play.fps)} fps",
      s"${world.buildings.size} buildings, ${play.view.visibleBuildings} seen",
      s"${world.creatures.size} creatures, ${world.shots.flying.size} shots",
      s"${play.particles.count} particles"
    )
    lines.zipWithIndex.foreach { case (l, i) => ui.outlined(l, 150f, 10f + i * 16f, 12f, Tint.Cream, bold = false) }
  }

  /** What the lamplighter carries, next to it. */
  private def cargo(): Unit = if(world.cargoAmount > 0 && world.lantern != null && play.riding == null) {
    val p = play.toScreen(world.lantern.x, world.lantern.y)
    val x = p.x / ui.scale + 16f
    val y = p.y / ui.scale - 30f
    ui.picture(art.items(world.cargo.id), x, y, 18f, 18f)
    ui.outlined(world.cargoAmount.toString, x + 20f, y + 1f, 13f)
  }

  // What keys and the right button do, for fingers.
  private def fingers(): Unit = {
    val entries: Vector[(String, () => Unit)] = play.tool match {
      case Tool.Hand =>
        if(play.riding != null) Vector("back" -> (() => InputSystem.ride(play)))
        else Vector.empty
      case Tool.Build(b) => ("remove" -> (() => { play.tool = Tool.Hand; play.lastZipline = null })) +: (if(b.rotates) Vector("rotate" -> (() => play.rotation = (play.rotation + 1) & 3)) else Vector.empty)
      case Tool.Stamp(_) => Vector("remove" -> (() => play.tool = Tool.Hand), "rotate" -> (() => InputSystem.turn(play)), "loop" -> (() => InputSystem.flip(play)))
      case Tool.Command => Vector("remove" -> (() => play.tool = Tool.Hand), "crewmode" -> (() => InputSystem.crew(play).foreach(_.selected = true)), "loop" -> (() => InputSystem.crew(play).foreach(_.selected = false)))
      case _ => Vector("remove" -> (() => play.tool = Tool.Hand))
    }
    entries.zipWithIndex.foreach { case ((icon, act), i) =>
      val x = 10f + i * 58f
      val y = ui.height - 62f
      if(ui.slot(x, y, 52f, hot = false)) act()
      ui.picture(art.icon(icon), x + 11f, y + 11f, 30f, 30f)
    }
  }

  private def crewPanel(): Unit = {
    val chosen = InputSystem.crew(play).filter(_.selected)
    if(chosen.nonEmpty) {
      val kinds = chosen.groupBy(_.kind).toVector.sortBy(-_._1.hp)
      val w = 300f
      val h = 96f
      val x = ui.width / 2f - w / 2f - 60f
      val y = ui.height - h - 44f
      ui.panel(x, y, w, h, 0.96f)
      kinds.take(6).zipWithIndex.foreach { case ((k, cs), i) =>
        ui.picture(art.bodies(k.id), x + 14f + i * 44f, y + 10f, 32f, 32f, Color.White, -Pi / 2f)
        ui.text(cs.size.toString, x + 38f + i * 44f, y + 30f, 13f, Tint.Cream, bold = true)
      }
      Stance.values.foreach { s =>
        val on = chosen.forall(_.stance == s)
        val icon = s match { case Stance.Fight => "sword"; case Stance.Hold => "shield"; case Stance.Patrol => "loop" }
        if(ui.button(x + 12f + s.ordinal * 94f, y + 54f, 90f, 32f, s.label, enabled = !on, size = 13f, icon = art.icon(icon))) chosen.foreach { c =>
          c.stance = s
          if(s == Stance.Patrol) { c.patrolX = c.x; c.patrolY = c.y }
        }
      }
    }
  }

  // --------------------------------------------------------------- building

  private def buildMenu(): Unit = {
    val cell = 38f
    val columns = 6
    val rows = 4
    val w = columns * cell + 58f + 16f
    val rowsNeeded = scala.math.max(rows, (Blocks.buildable(play.category).count(world.knows) + columns - 1) / columns)
    val h = rowsNeeded * cell + 64f
    val x = ui.width - w - 8f
    val y = ui.height - h - 8f
    ui.panel(x, y, w, h, 0.96f)
    Category.values.foreach { c =>
      val cy = y + 8f + c.ordinal * 28.6f
      val known = Blocks.buildable(c).exists(world.knows)
      if(ui.slot(x + 10f, cy, 27f, play.category == c)) play.category = c
      ui.picture(art.categories(c.ordinal), x + 13f, cy + 3f, 21f, 21f, if(known) Color.White else Tint.alpha(0.35f))
      if(ui.over(x + 10f, cy, 27f, 27f)) label(c.label + s" (${c.ordinal + 1})", x - 4f, cy + 4f)
    }
    val gx = x + 52f
    val gy = y + 34f
    ui.text(play.category.label, gx + 2f, y + 11f, 15f, Tint.Soft, bold = true)
    val blocks = Blocks.buildable(play.category).filter(world.knows)
    blocks.zipWithIndex.foreach { case (b, i) =>
      val bx = gx + (i % columns) * cell
      val by = gy + (i / columns) * cell
      val on = play.tool == Tool.Build(b)
      if(ui.slot(bx, by, cell - 3f, on)) {
        play.selected = null
        play.tool = if(on) Tool.Hand else Tool.Build(b)
        play.lastZipline = null
      }
      val affordable = world.canPay(b.cost)
      val size = if(b.size == 1) 22f else 28f
      Portrait.draw(ui.raw, art, b, (bx + (cell - 3f) / 2f) * ui.scale, (by + (cell - 3f) / 2f) * ui.scale, size * ui.scale, 0, if(affordable) Color.White else Color(0.55f, 0.5f, 0.5f, 0.9f))
      if(ui.over(bx, by, cell - 3f, cell - 3f)) tip = b
    }
    if(blocks.isEmpty) { val _ = ui.wrapped("Nothing learned yet. Press T for the almanac.", gx + 4f, gy + 8f, columns * cell - 10f, 14f, Tint.Soft) }
    val ty = y + h - 40f
    def small(i: Int, icon: String, on: Boolean, what: String): Boolean = {
      val bx = gx + i * 34f
      val done = ui.slot(bx, ty, 31f, on)
      ui.picture(art.icon(icon), bx + 5f, ty + 5f, 21f, 21f)
      if(ui.over(bx, ty, 31f, 31f)) label(what, x - 4f, ty + 4f)
      done
    }
    if(small(0, "remove", play.tool == Tool.Remove, "Take down (X)")) InputSystem.toggle(play, Tool.Remove)
    if(small(1, "copy", play.tool == Tool.Copy, "Copy a layout (C)")) InputSystem.toggle(play, Tool.Copy)
    if(small(2, "rotate", on = false, "Turn (R)")) play.rotation = (play.rotation + 1) & 3
    if(small(3, if(world.builder.paused) "play" else "pause", world.builder.paused, "Stop or go on building (B)")) world.builder.paused = !world.builder.paused
    if(small(5, "layouts", on = false, s"Kept layouts (${play.profile.bindings.name(Command.Layouts)})")) play.overlay = Overlay.Layouts
    if(small(4, "check", world.builder.rebuild, "Put ruins up again (G)")) world.builder.rebuild = !world.builder.rebuild
    if(tip != null) describe(tip, x - 258f, ui.height - 8f)
  }
  private def label(text: String, rx: Float, y: Float): Unit = {
    val w = ui.textWidth(text, 13f) + 22f
    ui.paper(rx - w, y - 4f, w, 30f)
    ui.text(text, rx - w + 11f, y + 3f, 13f, Tint.Ink)
  }

  /** What a block is and takes, above a point. */
  private def describe(b: Block, x: Float, bottom: Float): Unit = {
    val w = 250f
    val lines = ui.linesOf(b.about, w - 28f, 13f)
    val facts = Facts.of(b)
    val h = 46f + b.cost.size * 20f + facts.size * 17f + lines * ui.lineHeight(13f) + 14f
    val y = bottom - h
    ui.paper(x, y, w, h, catches = false)
    ui.text(b.label, x + 14f, y + 11f, 17f, Tint(0x8c3a24), bold = true)
    var at = y + 34f
    b.cost.foreach { s =>
      ui.picture(art.items(s.item.id), x + 14f, at, 17f, 17f)
      val have = world.hearthStock(s.item)
      val ok = world.rules.free || have >= s.amount
      ui.text(s"${s.amount}  ${s.item.label}", x + 37f, at + 1f, 13f, Tint.Ink)
      ui.right(if(world.rules.free) "" else Layout.amount(have), x + w - 14f, at + 1f, 13f, if(ok) Tint(0x5a8a3a) else Tint(0xc0402c), bold = true)
      at += 20f
    }
    at += 3f
    facts.foreach { f =>
      ui.text(f, x + 14f, at, 12f, Tint(0x7a5a48), bold = true)
      at += 17f
    }
    val _ = ui.wrapped(b.about, x + 14f, at + 3f, w - 28f, 13f, Tint.Ink)
  }

  // ------------------------------------------------------------- inspecting

  private def inspect(b: Building): Unit = {
    val w = 300f
    val x = ui.width / 2f - w / 2f - 60f
    val lines = Inspector.lines(world, b)
    val extra = b match {
      case _: SorterBuilding | _: HatchBuilding => 78f
      case _: WorkshopBuilding => 58f
      case _: GovernorBuilding => 112f
      case _: LoomBuilding => 44f
      case _: SignBuilding => 40f
      case _: RefitBuilding => 34f
      case _: HearthBuilding => 62f
      case c: CatapultBuilding => if(c.stock.total > 0) 28f else 0f
      case _: AlmanacBuilding => 96f
      case _: GreatWheelBuilding => 116f
      case s: StoreBuilding => if(s.stock.total > 0) 28f else 0f
      case _: CrafterBuilding => 28f
      case _ => 0f
    }
    val h = 56f + lines.size * 18f + extra
    val y = ui.height - h - 10f
    ui.panel(x, y, w, h, 0.97f)
    val block = b match { case s: SiteBuilding => s.target; case o => o.block }
    Portrait.draw(ui.raw, art, block, (x + 32f) * ui.scale, (y + 30f) * ui.scale, 34f * ui.scale, b.rotation, Color.White)
    ui.text(block.label, x + 58f, y + 12f, 17f, Tint.Cream, bold = true)
    ui.bar(x + 58f, y + 35f, w - 76f, 8f, b.healthShare, if(b.healthShare > 0.5f) Tint.Good else Tint.Bad)
    var at = y + 50f
    lines.foreach { case (text, color) =>
      ui.text(text, x + 18f, at, 13f, color)
      at += 18f
    }
    b match {
      case _: SorterBuilding | _: HatchBuilding => itemChoice(b, x + 16f, at + 4f)
      case ws: WorkshopBuilding =>
        ws.makes.zipWithIndex.foreach { case (k, i) =>
          val bx = x + 14f + i * 96f
          if(ui.slot(bx, at + 4f, 46f, ws.config == i)) ws.configure(i)
          ui.picture(art.bodies(k.id), bx + 6f, at + 10f, 34f, 34f, Color.White, -Pi / 2f)
          ui.text(k.label, bx + 50f, at + 8f, 12f, Tint.Cream, bold = true)
          k.cost.zipWithIndex.foreach { case (s, j) =>
            ui.picture(art.items(s.item.id), bx + 50f + j * 22f, at + 25f, 12f, 12f)
            ui.text(s.amount.toString, bx + 54f + j * 22f, at + 36f, 10f, Tint.Soft)
          }
        }
      case loom: LoomBuilding => if(ui.button(x + 16f, at + 4f, w - 32f, 32f, s"Cards (${loom.cards.size})", size = 14f, icon = art.icon("cards"))) play.overlay = Overlay.Loom
      case s: SignBuilding => (0 until 4).foreach(d => if(ui.button(x + 16f + d * 67f, at + 4f, 62f, 30f, LoomView.drum(d), enabled = s.config != d, size = 14f)) s.configure(d))
      case g: GovernorBuilding => governor(g, x + 16f, at + 4f, w - 32f)
      case r: RefitBuilding =>
        r.role.cost.zipWithIndex.foreach { case (s, j) =>
          ui.picture(art.items(s.item.id), x + 18f + j * 66f, at + 4f, 18f, 18f)
          ui.text(s"${r.stock(s.item)}/${s.amount}", x + 38f + j * 66f, at + 5f, 12f, if(r.stock.has(s.item, s.amount)) Tint.Good else Tint.Soft)
        }
      case h: HearthBuilding => goods(h, x + 18f, at + 4f, w - 36f)
      case c: CatapultBuilding => goods(c, x + 18f, at + 4f, w - 36f)
      case a: AlmanacBuilding =>
        AlmanacAction.values.zipWithIndex.foreach { case (action, i) =>
          val by = at + 4f + i * 44f
          if(ui.button(x + 16f, by, 150f, 38f, action.label, a.canAfford(action) && !world.longWinter, 14f)) { val _ = a.perform(action) }
          action.cost.zipWithIndex.foreach { case (s, j) =>
            ui.picture(art.items(s.item.id), x + 178f + j * 56f, by + 10f, 18f, 18f)
            ui.text(s.amount.toString, x + 199f + j * 56f, by + 11f, 14f, if(world.canPay(Vector(s))) Tint.Cream else Tint.Bad)
          }
        }
      case g: GreatWheelBuilding =>
        (0 until 4).foreach { k =>
          val by = at + k * 20f
          ui.picture(art.seasons(k), x + 16f, by, 17f, 17f, if(world.seasonIndex == k) Color.White else Tint.alpha(0.5f))
          ui.bar(x + 40f, by + 4f, 90f, 9f, g.filled(k), if(g.done(k)) Tint.Good else Tint.Gold)
          Blocks.spokes(k).zipWithIndex.foreach { case (s, j) =>
            ui.picture(art.items(s.item.id), x + 142f + j * 74f, by, 16f, 16f)
            ui.text(s"${s.amount - g.need(k, s.item)}/${s.amount}", x + 161f + j * 74f, by + 1f, 12f, if(g.need(k, s.item) <= 0) Tint.Good else Tint.Soft)
          }
        }
        if(ui.button(x + 16f, at + 82f, w - 32f, 30f, "Give what the Hearth has", !world.longWinter, 14f)) {
          val moved = g.fillFromHearth()
          if(moved > 0) play.sound(SoundKind.Place) else play.sound(SoundKind.Deny)
        }
      case s: StoreBuilding => goods(s, x + 18f, at + 4f, w - 36f)
      case c: CrafterBuilding => goods(c, x + 18f, at + 4f, w - 36f)
      case _ => ()
    }
  }
  // What is inside. A click takes some along.
  private def goods(b: Building, x: Float, y: Float, w: Float): Unit = {
    var at = x
    var row = y
    Items.all.foreach { i =>
      if(b.stock(i) > 0) {
        val t = Layout.amount(b.stock(i))
        val tw = 28f + ui.textWidth(t, 13f)
        if(at + tw > x + w) { at = x; row += 20f }
        val takes = b.isInstanceOf[StoreBuilding] || b.isInstanceOf[HearthBuilding]
        if(takes && ui.over(at, row, tw, 20f)) ui.rect(at - 2f, row - 1f, tw, 20f, Color(1f, 1f, 1f, 0.15f))
        ui.picture(art.items(i.id), at, row, 18f, 18f)
        ui.text(t, at + 20f, row + 1f, 13f)
        if(takes && ui.clicked(at, row, tw, 20f)) {
          if(!world.nearLantern(b)) { play.say("Fly closer", Tint.Bad); play.sound(SoundKind.Deny) }
          else { val _ = world.takeFrom(b, i) }
        }
        at += tw + 4f
      }
    }
  }
  private def governor(g: GovernorBuilding, x: Float, y: Float, w: Float): Unit = {
    val all = Watch.values
    if(ui.button(x, y, 28f, 26f, "", icon = art.icon("back"))) g.choose(watch = all((g.watch.ordinal + all.length - 1) % all.length))
    ui.centered(g.watch.label, x + w / 2f, y + 5f, 14f, Tint.Cream, bold = true)
    if(ui.button(x + w - 28f, y, 28f, 26f, "", icon = art.icon("play"))) g.choose(watch = all((g.watch.ordinal + 1) % all.length))
    g.watch match {
      case Watch.InSeason => Season.values.foreach { s =>
        if(ui.slot(x + s.ordinal * 44f, y + 34f, 38f, g.season == s)) g.choose(season = s)
        ui.picture(art.seasons(s.ordinal), x + 6f + s.ordinal * 44f, y + 40f, 26f, 26f)
      }
      case Watch.Below | Watch.Above =>
        Items.all.foreach { i =>
          val bx = x + (i.id % 9) * 29.5f
          val by = y + 32f + (i.id / 9) * 26f
          if(ui.slot(bx, by, 25f, g.item eq i)) g.choose(item = i)
          ui.picture(art.items(i.id), bx + 4f, by + 4f, 17f, 17f)
        }
        val ax = x + 5 * 29.5f + 6f
        if(ui.button(ax, y + 58f, 24f, 24f, "-", g.amount > 100, 14f)) g.choose(amount = g.amount - 100)
        ui.centered(g.amount.toString, ax + 52f, y + 62f, 13f)
        if(ui.button(ax + 80f, y + 58f, 24f, 24f, "+", g.amount < 9000, 14f)) g.choose(amount = g.amount + 100)
      case _ => ()
    }
  }
  private def itemChoice(b: Building, x: Float, y: Float): Unit = Items.all.foreach { i =>
    val bx = x + (i.id % 9) * 29.5f
    val by = y + (i.id / 9) * 30f
    if(ui.slot(bx, by, 28f, b.config == i.id)) b.configure(if(b.config == i.id) -1 else i.id)
    ui.picture(art.items(i.id), bx + 5f, by + 5f, 18f, 18f)
  }

  // --------------------------------------------------------------- overlays

  private def shade(): Unit = {
    ui.beginModal()
    ui.catchMouse(0f, 0f, ui.width, ui.height)
    ui.rect(0f, 0f, ui.width, ui.height, Color(0.08f, 0.05f, 0.09f, 0.66f))
  }

  private def menu(): Unit = {
    shade()
    val w = 300f
    val h = 416f
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.panel(x, y, w, h)
    ui.centered(world.rules.name, ui.width / 2f, y + 20f, 24f, Tint.Gold, bold = true)
    def b(i: Int, label: String): Boolean = ui.button(x + 30f, y + 60f + i * 43f, w - 60f, 38f, label)
    if(b(0, "Go on")) play.overlay = Overlay.None
    if(b(1, "Save, load")) play.overlay = Overlay.Saves
    if(b(2, "Deeds")) play.overlay = Overlay.Deeds
    if(b(3, "Help")) play.overlay = Overlay.Help
    if(b(4, "Settings")) play.overlay = Overlay.Settings
    if(b(5, "Start over")) {
      play.profile.dropWorld(world.rules.name)
      Scenes.startOver(context, play)
    }
    if(b(6, "Save and leave")) {
      play.saveWorld()
      context.runtime.loadScene(ValleyScene)
    }
  }

  private def help(): Unit = {
    shade()
    val w = 740f
    val h = 476f
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.paper(x, y, w, h)
    ui.centered("How things are done", ui.width / 2f, y + 18f, 22f, Tint(0x8c3a24), bold = true)
    val b = play.profile.bindings
    val keys = Vector(
      s"${b.name(Command.North)} ${b.name(Command.West)} ${b.name(Command.South)} ${b.name(Command.East)}" -> "Fly", "Wheel" -> "Zoom, middle drag to look around",
      "Left" -> "Place, choose, dig, hold to shoot", "Right" -> "Let go; drag to take down", "Shift" -> "Belts find their own way"
    ) ++ Vector(Command.Turn, Command.Pick, Command.Remove, Command.Copy, Command.Flip, Command.Layouts, Command.Rebuild, Command.Crew, Command.AllCrew, Command.Ride, Command.Learn, Command.Codex, Command.Map, Command.Ledger, Command.Pause, Command.Speed, Command.NextWave).map(c => b.name(c) -> c.label)
    keys.zipWithIndex.foreach { case (k -> what, i) =>
      val cx = x + 30f + (i / 8) * 228f
      val cy = y + 56f + (i % 8) * 24f
      ui.text(k, cx, cy, ui.fitting(k, 58f, 13f), Tint(0x8c3a24), bold = true)
      ui.text(what, cx + 62f, cy, ui.fitting(what, 162f, 13f, bold = false), Tint.Ink)
    }
    Season.values.foreach { s =>
      val cx = x + 30f + (s.ordinal % 2) * 350f
      val cy = y + 262f + (s.ordinal / 2) * 72f
      ui.picture(art.seasons(s.ordinal), cx, cy, 34f, 34f)
      ui.text(s.label, cx + 42f, cy - 2f, 14f, Tint(0x8c3a24), bold = true)
      val _ = ui.wrapped(s.hint, cx + 42f, cy + 15f, 276f, 12f, Tint.Ink)
    }
    if(ui.button(ui.width / 2f - 60f, y + h - 56f, 120f, 40f, "Close")) play.overlay = Overlay.None
  }

  private def ending(): Unit = {
    shade()
    val won = world.outcome == Outcome.Won
    val w = 420f
    val h = 330f
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.panel(x, y, w, h)
    ui.centered(if(won) "The Hearth burns on" else "The Hearth has gone out", ui.width / 2f, y + 24f, 27f, if(won) Tint.Gold else Tint.Bad, bold = true)
    ui.centered(world.rules.name, ui.width / 2f, y + 60f, 15f, Tint.Soft)
    val facts = Vector(
      "Time" -> Layout.time(world.time), "Years" -> world.year.toString, "Waves" -> world.waves.number.toString,
      "Slain" -> world.slain.toString, "Built" -> world.built.toString, "Lost" -> world.lost.toString
    )
    facts.zipWithIndex.foreach { case ((k, v), i) =>
      val cx = x + 60f + (i % 2) * 170f
      val cy = y + 96f + (i / 2) * 34f
      ui.text(k, cx, cy, 15f, Tint.Soft)
      ui.right(v, cx + 130f, cy, 16f, Tint.Cream, bold = true)
    }
    val opened = if(won && play.valley >= 0) Valleys.places.indices.filter(i => Valleys.places(i).after.contains(play.valley) && play.profile.open(i) && !play.profile.won.contains(i)) else Vector.empty
    if(opened.nonEmpty)
      ui.centered(opened.map(Valleys.all(_).name).mkString(" and ") + (if(opened.size == 1) " lies open" else " lie open"), ui.width / 2f, y + 204f, 15f, Tint.Good, bold = true)
    if(ui.button(x + 40f, y + h - 66f, 160f, 44f, "Once more")) Scenes.startOver(context, play)
    if(ui.button(x + w - 200f, y + h - 66f, 160f, 44f, "Valleys")) context.runtime.loadScene(ValleyScene)
  }
}

/** Numbers about a block, as short lines. */
object Facts {
  /** What a block would come to where it is about to stand. */
  def atPlace(world: Valley, plan: Plan): Option[String] = plan.block.role match {
    case Role.Drill(_, base) =>
      val (item, n) = DrillBuilding.survey(world, plan.block, plan.x, plan.y)
      if(item == null) None else Some(s"${item.label} ${Layout.oneDecimal(n / base * 60f)} a minute")
    case Role.Tap(_) => Some(s"${TapBuilding.trees(world, plan.x, plan.y)} thickets")
    case Role.VentWell(output) => Some(s"${Layout.oneDecimal(output * world.ventsUnder(plan.block, plan.x, plan.y))} power")
    case Role.Garden(_) => Some(s"${scala.math.round(GardenBuilding.fertility(world, plan.block, plan.x, plan.y) * 100f)}% fertile")
    case _ => None
  }
  def of(b: Block): Vector[String] = {
    val lines = Vector.newBuilder[String]
    b.role match {
      case Role.Belt(s) => lines += s"${s.toInt} items a second"
      case Role.Drill(_, base) => lines += s"Up to ${Layout.oneDecimal(b.size * b.size / base)} items a second"
      case c: Role.Crafter => lines += c.in.map(s => s"${s.amount} ${s.item.label}").mkString(" + ") + s" = ${c.out.amount} ${c.out.item.label}, ${Layout.oneDecimal(c.seconds)}s"
      case g: Role.Generator => lines += s"Makes ${Layout.oneDecimal(g.output)} power"
      case t: Role.Turret =>
        lines += s"Reach ${Layout.oneDecimal(t.range)}" + (if(t.air && t.ground) "" else if(t.ground) ", ground only" else ", air only")
        if(t.ammo.nonEmpty) lines += "Shoots " + t.ammo.map(_._1.label).mkString(", ")
        if(b.coolable) lines += "Faster with water or brine"
      case Role.Brazier(r, _) => lines += s"Warms ${Layout.oneDecimal(r)} tiles around"
      case Role.Radiator(r) => lines += s"Warms ${Layout.oneDecimal(r)} tiles around"
      case Role.Flywheel(c) => lines += s"Stores ${c.toInt} power"
      case Role.Boiler(output, _) => lines += s"Makes ${Layout.oneDecimal(output)} power"
      case Role.VentWell(output) => lines += s"Makes ${Layout.oneDecimal(output)} power for each vent"
      case Role.Reactor(output, _) => lines += s"Makes up to ${Layout.oneDecimal(output)} power"
      case Role.Press(in, out, rate, seconds) => lines += in.map(s => s"${s.amount} ${s.item.label}").mkString(" + ") + s" = ${Layout.oneDecimal(rate * seconds)} ${out.label}"
      case Role.Chiller(rate) => lines += s"Makes ${Layout.oneDecimal(rate)} brine a second"
      case Role.Cistern(c) => lines += s"Holds ${c.toInt}"
      case Role.Aegis(r, strength) => lines += s"Reach ${Layout.oneDecimal(r)}, takes ${strength.toInt}"
      case Role.Metronome(r, speed) => lines += s"Reach ${Layout.oneDecimal(r)}, ${scala.math.round(speed * 100f - 100f)}% faster"
      case Role.Mender(r, s, share) => lines += s"Reach ${Layout.oneDecimal(r)}, ${scala.math.round(share * 100f)}% every ${Layout.oneDecimal(s)}s"
      case Role.Catapult(r) => lines += s"Reach ${r.toInt}"
      case Role.Refit(steps, cost, seconds) =>
        lines += steps.map { case (a, b) => s"${a.label} to ${b.label}" }.mkString(", ")
        lines += cost.map(s => s"${s.amount} ${s.item.label}").mkString(", ") + s", ${seconds.toInt}s each"
      case Role.Workshop(makes) => lines += makes.map(_.label).mkString(", ")
      case Role.Store(c) => lines += s"$c of every item"
      case _ => ()
    }
    if(b.size > 0 && b.hp > 0f) lines += s"Takes ${b.hp.toInt}"
    if(b.powerUse > 0f) lines += s"Uses ${Layout.oneDecimal(b.powerUse)} power"
    if(b.waterUse > 0f) lines += s"Uses ${Layout.oneDecimal(b.waterUse)} ${b.fluid.label.toLowerCase} a second"
    lines.result()
  }
}

/** What there is to say about a building. */
object Inspector {
  private def percent(v: Float): String = scala.math.round(v * 100f).toString + "%"
  def lines(world: Valley, b: Building): Vector[(String, Color)] = {
    val out = Vector.newBuilder[(String, Color)]
    def say(s: String, c: Color = Tint.Soft): Unit = out += ((s, c))
    if(b.stopped) say("Stopped by a switch or a governor", Tint.Bad)
    if(b.hurriedUntil >= world.time) say("Hurried by a metronome", Tint.Gold)
    if(b.flooded) say("Under water until the season turns", Tint.Water)
    else if(world.frozen && b.block.chills && b.team == Valley.Player && !b.warm) say("Cold: half speed. Needs a brazier.", Tint(0xbfeaff))
    b match {
      case d: DrillBuilding => if(d.ore != null) say(s"Digs ${d.ore.label}, ${Layout.oneDecimal(d.perSecond * d.condition * d.powerShare * (if(d.block.waterUse > 0f) 1f + d.waterShare else 1f))} a second")
      case t: TapBuilding => say(s"Resin from ${t.trees} thicket${if(t.trees == 1) "" else "s"}")
      case c: CrafterBuilding =>
        say(c.recipe.in.map(s => s"${s.amount} ${s.item.label}").mkString(" + ") + s" = ${c.recipe.out.amount} ${c.recipe.out.item.label}")
      case g: GeneratorBuilding => say(s"Makes ${Layout.oneDecimal(g.output)} of ${Layout.oneDecimal(g.role.output)} power")
      case p: PumpBuilding => say(if(p.output > 0f) s"Lifts ${p.output.toInt} water a second" else if(world.frozen) "Frozen" else "Dry")
      case t: TurretBuilding =>
        if(t.role.ammo.nonEmpty) {
          if(t.hasAmmo) say(s"${t.rounds.sum} shots of ${t.role.ammo(t.loaded)._1.label}") else say("Nothing to shoot: " + t.role.ammo.map(_._1.label).mkString(", "), Tint.Bad)
        }
      case z: BrazierBuilding => say(if(z.lit) "Burning" else if(world.frozen) "No fuel: coal, resin or embers" else s"Waits for winter, ${z.stock.total} fuel", if(world.frozen && !z.lit) Tint.Bad else Tint.Soft)
      case _: PylonBuilding => say("Tap another building to link it or let it go")
      case _: ZiplineBuilding => say("Tap another zipline in line to lead there")
      case _: SorterBuilding => say("Passes on:")
      case _: HatchBuilding => say(if(b.config < 0) "Takes out anything:" else "Takes out:")
      case w: WorkshopBuilding => say(s"Crew ${w.crew} of ${w.maxCrew}, ${percent(w.progress)} wound")
      case a: AlmanacBuilding => say(if(a.powerShare < 0.3f) "Needs power" else if(a.rest > 0f) s"Rests for ${Layout.time(a.rest)}" else "Ready", if(a.ready) Tint.Good else Tint.Soft)
      case _: GreatWheelBuilding => say(if(world.longWinter) "Complete" else s"Takes the goods of ${world.season.label} now")
      case f: FlywheelBuilding => if(f.power != null) say(s"${f.power.stored.toInt} of ${f.power.capacity.toInt} stored")
      case s: SiteBuilding => say((if(s.removing) "Coming down, " else "Going up, ") + percent(s.progress))
      case _: HearthBuilding => say(s"Crew ${world.crewCount} of ${world.crewLimit}. Click goods to carry some:")
      case c: CatapultBuilding => say(if(c.destination == null) "Catches crates. Tap another catapult to throw there." else s"Throws to another, ${c.stock.total} aboard")
      case g: GardenBuilding => say(if(world.frozen) "Asleep until spring" else s"${percent(g.fertile)} fertile, ${percent(g.progress)} grown")
      case g: BoilerBuilding => say(s"Makes ${Layout.oneDecimal(g.output)} power, ${g.stock.total} fuel")
      case v: VentBuilding => say(s"On ${v.vents} vents, makes ${Layout.oneDecimal(v.powerOutput)}")
      case r: ReactorBuilding =>
        say(s"${r.stock(Items.Ember)} embers, makes ${Layout.oneDecimal(r.powerOutput)}")
        say(s"Heat ${percent(r.heat)}", if(r.heat > 0.6f) Tint.Bad else Tint.Soft)
      case r: RatchetBuilding => say(s"${Layout.oneDecimal(r.flow)} a second goes through")
      case a: AegisBuilding => say(if(a.broken) "Broken, coming back" else s"${a.strength.toInt} of ${a.full.toInt}", if(a.broken) Tint.Bad else Tint.Soft)
      case r: RefitBuilding => say(if(r.inside != null) s"${r.inside.label} to ${r.becomes.label}, ${percent(r.progress)}" else "Waits for " + r.role.steps.map(_._1.label).mkString(", "))
      case w: WagonBuilding => say(s"${w.stock.total} of ${w.PerWagon} loaded")
      case g: GovernorBuilding => say(if(g.lets) "Lets them work" else "Holds them back", if(g.lets) Tint.Good else Tint.Bad)
      case l: LoomBuilding => say(if(l.powerShare <= 0.3f) "Needs power" else if(l.cards.isEmpty) "Has no cards yet" else s"${l.cards.size} cards, ${l.ties.size} links", if(l.powerShare <= 0.3f) Tint.Bad else Tint.Soft); say("Tap other buildings to link them")
      case s: SignBuilding => say(s"Drum ${LoomView.drum(s.config)}: ${s.shown}")
      case p: PodBuilding => say(s"${p.brood} of ${p.maxBrood} keep watch, ${p.kind.label} next", Tint.Bad)
      case _: NestBuilding => say("Burn it out to stop what comes from here", Tint.Bad)
      case _ => ()
    }
    if(b.isPowered && b.power != null && !b.isInstanceOf[SiteBuilding])
      say(s"Power: ${Layout.oneDecimal(b.power.produced)} made, ${Layout.oneDecimal(b.power.needed)} wanted" + (if(b.power.capacity > 0f) s", ${b.power.stored.toInt} stored" else ""), if(b.power.satisfaction > 0.95f || b.power.needed <= 0f) Tint.Good else Tint.Bad)
    else if(b.block.usesPower) say("Not linked to any pylon", Tint.Bad)
    def fluidLine(g: FluidGraph): Unit =
      if(g.fluid == null) say("Dry", Tint.Soft)
      else say(s"${g.fluid.label}: ${Layout.oneDecimal(g.supplied)} in, ${Layout.oneDecimal(g.needed)} wanted, ${g.stored.toInt} of ${g.capacity.toInt}", Tint(g.fluid.color))
    if(b.fluid != null) fluidLine(b.fluid)
    else if(b.tanks.nonEmpty) b.tanks.foreach(fluidLine)
    else if(b.block.waterUse > 0f) say(s"No ${b.block.fluid.label.toLowerCase}: lead a pipe here", Tint.Bad)
    out.result()
  }
}

object Settings {
  private def shade(ui: Ui): Unit = {
    ui.beginModal()
    ui.catchMouse(0f, 0f, ui.width, ui.height)
    ui.rect(0f, 0f, ui.width, ui.height, Color(0.08f, 0.05f, 0.09f, 0.72f))
  }
  def draw(ui: Ui, profile: Profile, back: () => Unit, keys: () => Unit): Unit = {
    shade(ui)
    val w = 640f
    val h = 440f
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.panel(x, y, w, h)
    ui.centered("Settings", ui.width / 2f, y + 18f, 24f, Tint.Gold, bold = true)
    val c1 = x + 32f
    val c2 = x + 336f
    profile.musicVolume = ui.slider(c1, y + 62f, 260f, "Music", profile.musicVolume)
    profile.soundVolume = ui.slider(c1, y + 114f, 260f, "Sounds", profile.soundVolume)
    profile.airVolume = ui.slider(c1, y + 166f, 260f, "Wind and weather", profile.airVolume)
    profile.wires = ui.slider(c1, y + 218f, 260f, "Wires", profile.wires)
    ui.text("Size of the interface", c1, y + 272f, 15f)
    Vector(0.85f, 1f, 1.15f, 1.3f).zipWithIndex.foreach { case (v, i) =>
      if(ui.button(c1 + i * 66f, y + 294f, 61f, 30f, (v * 100f).round.toString, enabled = scala.math.abs(profile.uiScale - v) > 0.01f, size = 14f)) profile.uiScale = v
    }
    ui.text("How hard new valleys are", c1, y + 336f, 15f)
    Difficulty.values.foreach { d =>
      if(ui.button(c1 + d.ordinal * 66f, y + 358f, 61f, 30f, d.label, enabled = profile.difficulty != d, size = 12f)) profile.difficulty = d
    }
    def t(i: Int, label: String, value: Boolean): Boolean = ui.toggle(c2, y + 60f + i * 28f, 270f, label, value)
    profile.bloom = t(0, "Glow (from the next valley on)", profile.bloom)
    profile.weather = t(1, "Weather", profile.weather)
    profile.clouds = t(2, "Shadows of clouds", profile.clouds)
    profile.ripples = t(3, "Moving water", profile.ripples)
    profile.particles = t(4, "Particles", profile.particles)
    profile.shake = t(5, "Shake the screen", profile.shake)
    profile.signs = t(6, "Signs on what lacks something", profile.signs)
    profile.minimap = t(7, "Minimap", profile.minimap)
    profile.hints = t(8, "Hints", profile.hints)
    profile.autoFire = t(9, "The lamplighter shoots by itself", profile.autoFire)
    profile.stats = t(10, "Statistics", profile.stats)
    Lang.Language.values.foreach { l =>
      if(ui.button(c1 + l.ordinal * 66f, y + 398f, 61f, 30f, l.label, enabled = Lang.language != l, size = 12f)) Lang.language = l
    }
    if(profile.canFullscreen && ui.button(c1 + 136f, y + 398f, 124f, 30f, if(profile.isFullscreen) "Back to a window (F11)" else "Full screen (F11)", size = 14f)) profile.toggleFullscreen()
    if(ui.button(c2, y + 372f, 130f, 38f, "Keys")) keys()
    if(ui.button(c2 + 140f, y + 372f, 130f, 38f, "Back")) { profile.save(); back() }
  }

  def keys(ui: Ui, profile: Profile, binding: Option[Command], bind: Option[Command] => Unit, back: () => Unit): Unit = {
    shade(ui)
    val w = 720f
    val h = 470f
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.panel(x, y, w, h)
    ui.centered("Keys", ui.width / 2f, y + 18f, 24f, Tint.Gold, bold = true)
    Command.values.foreach { c =>
      val cx = x + 28f + (c.ordinal / 13) * 338f
      val cy = y + 58f + (c.ordinal % 13) * 27f
      ui.text(c.label, cx, cy + 4f, ui.fitting(c.label, 200f, 14f, bold = false))
      val waiting = binding.contains(c)
      if(ui.button(cx + 206f, cy, 108f, 24f, if(waiting) "press a key" else profile.bindings.name(c), size = 13f)) bind(if(waiting) None else Some(c))
    }
    if(ui.button(x + w / 2f - 170f, y + h - 54f, 160f, 38f, "As they were")) profile.bindings.reset()
    if(ui.button(x + w / 2f + 10f, y + h - 54f, 160f, 38f, "Back")) { bind(None); profile.save(); back() }
  }
}

/** The almanac: a page for every block, paid for out of the Hearth. */
private final class LearnView(play: Play, ui: Ui) {
  private val world = play.world
  private val art = play.art

  def draw(): Unit = {
    ui.beginModal()
    ui.catchMouse(0f, 0f, ui.width, ui.height)
    ui.rect(0f, 0f, ui.width, ui.height, Color(0.08f, 0.05f, 0.09f, 0.78f))
    val w = scala.math.min(ui.width - 24f, 1150f)
    val h = scala.math.min(ui.height - 16f, 660f)
    val x = ui.width / 2f - w / 2f
    val y = ui.height / 2f - h / 2f
    ui.paper(x, y, w, h)
    ui.text("Almanac", x + 26f, y + 16f, 25f, Tint(0x8c3a24), bold = true)
    ui.text("Pages are paid for out of the Hearth, and kept for every valley after.", x + 156f, y + 25f, 14f, Tint(0x7a5a48))
    if(ui.button(x + w - 118f, y + 12f, 96f, 34f, "Close", size = 15f)) play.overlay = Overlay.None

    val side = 250f
    val left = x + 34f
    val top = y + 58f
    val stepX = (w - side - 56f) / 9f
    val stepY = (h - 84f) / 15f
    def px(p: Page) = left + p.column * stepX
    def py(p: Page) = top + p.row * stepY
    val box = scala.math.min(40f, stepY - 4f)
    // What leads to the page pointed at stands out.
    val pointed = Pages.all.find(p => ui.over(px(p), py(p), box, box))
    val leading = scala.collection.mutable.HashSet.empty[Block]
    def lead(p: Page): Unit = if(leading.add(p.block)) p.requires.foreach(r => lead(Pages.byBlock(r)))
    pointed.foreach(lead)
    val order = Pages.all.sortBy(p => leading.contains(p.block))
    order.foreach { p =>
      p.requires.foreach { r =>
        val from = Pages.byBlock(r)
        val known = world.knows(r)
        val c = if(leading.contains(p.block)) Tint(0xd1503e) else if(known && world.knows(p.block)) Tint(0xb9784a) else if(known) Tint(0xcf9a66) else Tint(0xe3cda2)
        val x0 = px(from) + box
        val y0 = py(from) + box / 2f
        val x1 = px(p)
        val y1 = py(p) + box / 2f
        val mx = x1 - 7f - (p.row % 2f) * 3f
        ui.line(x0, y0, mx, y0, 1.8f, c)
        if(y0 != y1) ui.line(mx, y0, mx, y1, 1.8f, c)
        ui.line(mx, y1, x1, y1, 1.8f, c)
      }
    }
    var shown: Page = null
    Pages.all.foreach { p =>
      val bx = px(p)
      val by = py(p)
      val known = world.knows(p.block)
      val can = world.canLearn(p)
      val open = p.requires.forall(world.knows)
      if(ui.slot(bx, by, box, can && sin(play.time * 4f + p.column) > 0f)) {
        if(!known) {
          if(!world.learn(p)) play.sound(SoundKind.Deny)
        }
      }
      val s = box * 0.66f
      Portrait.draw(ui.raw, art, p.block, (bx + box / 2f) * ui.scale, (by + box / 2f) * ui.scale, s * ui.scale, 0, if(known) Color.White else if(open) Color(0.85f, 0.8f, 0.8f, 0.85f) else Color(0.46f, 0.42f, 0.46f, 0.95f))
      if(leading.contains(p.block) && !known) ui.rect(bx + 2f, by + box - 4f, box - 4f, 3f, Tint(0xd1503e))
      if(known) ui.picture(art.icon("check"), bx + box - 14f, by + box - 14f, 14f, 14f)
      else if(!open) ui.picture(art.icon("lock"), bx + box - 14f, by + box - 14f, 14f, 14f)
      if(ui.over(bx, by, box, box)) shown = p
    }
    val sx = x + w - side - 14f
    ui.line(sx - 8f, y + 62f, sx - 8f, y + h - 24f, 1.5f, Tint(0xd9bd8c))
    if(shown != null) page(shown, sx, y + 70f, side - 6f)
    else {
      val next = Pages.all.filter(p => !world.knows(p.block) && p.requires.forall(world.knows)).sortBy(p => p.cost.map(s => s.amount * (if(s.item.id >= 5) 3 else 1)).sum)
      ui.text("Within reach", sx, y + 70f, 16f, Tint(0x8c3a24), bold = true)
      next.take(9).zipWithIndex.foreach { case (p, i) =>
        val ly = y + 98f + i * 46f
        Portrait.draw(ui.raw, art, p.block, (sx + 16f) * ui.scale, (ly + 16f) * ui.scale, 26f * ui.scale, 0, Color.White)
        ui.text(p.block.label, sx + 38f, ly, 14f, if(world.canLearn(p)) Tint(0x5a8a3a) else Tint.Ink, bold = true)
        costRow(p, sx + 38f, ly + 18f)
      }
    }
  }
  private def costRow(p: Page, x: Float, y: Float): Unit = {
    var at = x
    p.cost.foreach { s =>
      ui.picture(art.items(s.item.id), at, y, 15f, 15f)
      val t = s.amount.toString
      ui.text(t, at + 17f, y, 12f, if(world.rules.free || world.hearthStock(s.item) >= s.amount) Tint(0x5a8a3a) else Tint(0xc0402c), bold = true)
      at += 26f + ui.textWidth(t, 12f, bold = true)
    }
  }
  private def page(p: Page, x: Float, y: Float, w: Float): Unit = {
    ui.text(p.block.label, x, y, 18f, Tint(0x8c3a24), bold = true)
    var at = ui.wrapped(p.block.about, x, y + 28f, w, 14f, Tint.Ink) + 6f
    Facts.of(p.block).foreach { f => at = ui.wrapped(f, x, at, w, 13f, Tint(0x7a5a48), bold = true) }
    at += 10f
    if(world.knows(p.block)) ui.text("Known", x, at, 15f, Tint(0x5a8a3a), bold = true)
    else {
      ui.text("To learn it", x, at, 14f, Tint(0x8c3a24), bold = true)
      at += 22f
      p.cost.foreach { s =>
        val have = world.hearthStock(s.item)
        ui.picture(art.items(s.item.id), x, at, 20f, 20f)
        ui.text(s"${s.amount} ${s.item.label}", x + 26f, at + 2f, 14f, Tint.Ink)
        ui.right(Layout.amount(have), x + w - 10f, at + 2f, 14f, if(have >= s.amount) Tint(0x5a8a3a) else Tint(0xc0402c), bold = true)
        at += 24f
      }
      val missing = p.requires.filterNot(world.knows)
      if(missing.nonEmpty) { val _ = ui.wrapped("After " + missing.map(_.label).mkString(", "), x, at + 6f, w, 13f, Tint(0xc0402c)) }
    }
  }
}
