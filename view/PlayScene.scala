package hearthwheel
package core

import sgl.{Input, InputActions}
import sgl.Input.Keys
import sgl.engine.gameobject._
import sgl.engine.gameobject.render2d._
import sgl.engine.gameobject.spatial2d._
import hearthwheel.model._
import hearthwheel.model.MathUtil._
import sgl.geometry.Vec2

/** A valley being played.
  *
  * @param valley its place in the campaign, or -1
  */
class PlayScene(makeWorld: () => Valley, valley: Int) extends Scene {
  protected def attract: Boolean = false

  override def load(context: GameContext)(using scope: Scope): Unit = {
    val world = makeWorld()
    val camera = context.spawn2D("camera", Transform2D(Vec2(world.hearth.cx, world.hearth.cy))).attach(new Camera2D(_, Vec2(800f, 450f)))
    val art = context.global[Gallery]
    val play = new Play(
      world, art, context.global[Services], context.global[Profile], valley,
      camera.get[Camera2D], camera.spatial, new Terrain(world, art, context.engine.renderer), attract
    )
    play.effects.create(context.engine.renderer)
    play.minimap = context.engine.renderer.bake2D(Minimap.paint(world, _))
    if(valley >= 0 && !world.rules.free) world.learned ++= play.profile.learned
    if(play.profile.bloom) context.setGlobal(Bloom(0.5f, 0.82f))
    context.setGlobal(play)
    Input.setInputProcessor(play.controls, Set(InputActions.Back))
    scope.onDestroy(() => Input.clearInputProcessor())

    context.addSystem(Stage.PreUpdate, InputSystem)
    context.addSystem(Stage.FixedUpdate, SimulationSystem, when = c => c.global[Play].running)
    context.addSystem(Stage.Update, EffectsSystem)
    context.addSystem(Stage.PreRender, CameraSystem)
    context.addSystem(Stage.PreRender, PuppetSystem, after = Set(CameraSystem))
    context.addSystem(Stage.Render, LandRenderSystem)
    context.addSystem(Stage.Render, PuppetRenderSystem)
    context.addSystem(Stage.Render, AirRenderSystem)
    addInterface(context)
  }
  protected def addInterface(context: GameContext)(using Scope): Unit = {
    context.addSystem(Stage.Render, PlanRenderSystem)
    context.addSystem(Stage.Render, HudSystem)
  }

  // A browser tab or a phone may never come back from this.
  override def pause(context: GameContext)(using Scope): Unit = context.global[Play].saveWorld()
  override def unload(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    play.saveWorld()
    if(valley >= 0) {
      play.profile.learned = play.profile.learned ++ play.world.learned
      play.profile.collect(play.world)
    }
    if(!attract) play.profile.met = play.profile.met ++ play.world.met
    play.profile.save()
  }
}

/** Steps the world. */
object SimulationSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.simulation")

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val dt = context.dt
    var i = 0
    while(i < play.speed) {
      driveLantern(play, dt)
      play.world.update(dt)
      i += 1
    }
  }

  private def driveLantern(play: Play, dt: Float): Unit = if(!play.attract) {
    val lantern = play.world.lantern
    if(play.riding != null && !play.riding.alive) play.riding = null
    val led = if(play.riding != null) play.riding else lantern
    val (mx, my) = way(play)
    if(play.riding != null && lantern != null) lantern.drive(0f, 0f, dt)
    if(led != null) {
      var dx = mx
      var dy = my
      if(dx != 0f || dy != 0f) play.following = true
      else if(!play.following && (led eq lantern)) {
        // It goes where the player is looking, to build there.
        val d = dist(lantern.x, lantern.y, play.centerX, play.centerY)
        if(d > 60f) { dx = (play.centerX - lantern.x) / d; dy = (play.centerY - lantern.y) / d }
      }
      if(led eq lantern) led.drive(dx, dy, dt) else { led.driveX = dx; led.driveY = dy }
      val p = play.pointer
      if(play.shooting) {
        led.fireAt(p.x, p.y)
        led.rotation = turnTo(led.rotation, atan2(p.y - led.y, p.x - led.x), dt * 14f)
      } else if(play.profile.autoFire && led.weaponRange > 0f) {
        // Left to itself it shoots at what comes close.
        val foe = play.world.closestCreature(led.x, led.y, led.weaponRange * 0.85f, Valley.Bramble, air = true, ground = true)
        if(foe != null) led.fireAt(foe.x, foe.y)
      }
    }
  }
  private def way(play: Play): (Float, Float) = {
    val c = play.controls
    val b = play.profile.bindings
    def held(command: Command, arrow: Keys.Key): Float = if(c.isHeld(b.keyOf(command)) || c.isHeld(arrow)) 1f else 0f
    val dx = held(Command.East, Keys.Right) - held(Command.West, Keys.Left)
    val dy = held(Command.South, Keys.Down) - held(Command.North, Keys.Up)
    if(dx != 0f && dy != 0f) (dx * 0.7071f, dy * 0.7071f) else (dx, dy)
  }
}

object Endings {
  def reached(play: Play): Unit = {
    play.overlay = Overlay.End
    play.tool = Tool.Hand
    play.selected = null
    play.profile.dropWorld(play.world.rules.name)
    if(play.valley >= 0) play.profile.collect(play.world)
    if(play.world.outcome == Outcome.Won && play.valley >= 0) {
      // Some of what the Hearth held goes along.
      if(!play.profile.won.contains(play.valley)) Items.all.foreach(i => play.profile.vault(i.id) += play.world.hearthStock(i) / 4)
      play.profile.won = play.profile.won + play.valley
      play.profile.learned = play.profile.learned ++ play.world.learned
    }
    play.profile.save()
  }
}

/** Turns what happened in the world into particles, sounds and messages. */
object EffectsSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.effects")
  private var autosave = 0f

  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val world = play.world
    val dt = context.dt
    play.time += dt
    if(world.outcome != Outcome.Playing && !play.ended && !play.attract) {
      play.ended = true
      Endings.reached(play)
    }
    if(dt > 0f) play.fps += (1f / dt - play.fps) * 0.05f

    var i = 0
    while(i < world.effects.size) {
      val e = world.effects(i)
      if(play.sees(e.x, e.y, 120f)) {
        if(play.profile.particles) play.particles.emit(e)
        if(e.kind == FxKind.Explosion) play.decals.scorch(e.x, e.y, e.a)
        else if(e.kind == FxKind.Break) play.decals.scorch(e.x, e.y, e.a * 14f)
        else if(e.kind == FxKind.Death) play.decals.splat(e.x, e.y, e.a, e.color)
        if(e.kind == FxKind.Explosion && e.a > 50f) play.shake(e.a * 0.04f)
      }
      i += 1
    }
    world.effects.clear()
    hear(play)
    world.notices.foreach { n =>
      play.say(n.text, Tint(n.color))
      if(n.text.endsWith("has come")) {
        play.banner = world.season.label
        play.bannerIcon = world.seasonIndex
        play.bannerAge = 0f
      }
    }
    world.notices.clear()

    if(play.running) {
      play.particles.update(dt)
      play.decals.update(dt)
      trails(play, dt)
    }
    play.toasts.foreach(_.age += dt)
    play.toasts.filterInPlace(_.age < 7f)
    checkWait -= dt
    if(checkWait <= 0f && !play.attract) {
      checkWait = 1f
      Deeds.check(play)
      Hints.check(play)
    }
    val site = world.builder.active
    if(site != null && play.running) play.particles.weld(site.cx, site.cy, site.size * 14f)
    play.messages.foreach(_.age += dt)
    play.messages.filterInPlace(_.age < 6f)
    play.bannerAge += dt
    if(play.tutorial != null && play.running) play.tutorial.update(dt)
    if(play.selected != null && !play.selected.alive) play.selected = world.buildingAt(play.selected.x, play.selected.y)

    // Another piece when it gets serious.
    val h = world.hearth
    val close = h != null && world.creatures.count(c => c.team == Valley.Bramble && dist2(c.x, c.y, h.cx, h.cy) < 620f * 620f) >= 6
    if(!play.attract && (close || world.creatures.exists(_.kind.boss))) battle = 9f else battle -= dt
    play.services.music(if(battle > 0f) 4 else world.seasonIndex)
    play.services.air(world.seasonIndex)
    play.services.updateAudio(dt, play.profile.musicVolume * (if(play.attract) 0.8f else 0.6f), play.profile.airVolume * (if(play.attract) 0.5f else 0.8f))
    if(!play.attract && play.running) {
      autosave += dt
      if(autosave > 45f) { autosave = 0f; play.saveWorld() }
    }
  }

  private var battle = 0f
  private var checkWait = 2f
  private var trailClock = 0f
  // Rockets smoke, and flames glow on.
  private def trails(play: Play, dt: Float): Unit = {
    trailClock += dt
    if(trailClock > 0.03f && play.profile.particles) {
      trailClock = 0f
      var i = 0
      val flying = play.world.shots.flying
      while(i < flying.size) {
        val f = flying(i)
        if((f.shot.homing > 0f || f.shot.sprite == "shot_flame" || f.lobbed > 0f) && f.shot.speed > 0f && play.sees(f.x, f.y, 30f))
          play.particles.trail(f.x, f.y, f.shot.color, f.shot.size * (if(f.shot.homing > 0f) 0.7f else 0.9f))
        i += 1
      }
    }
  }
  private val perKind = new Array[Int](SoundKind.values.length)
  private def hear(play: Play): Unit = {
    java.util.Arrays.fill(perKind, 0)
    play.world.sounds.foreach { s =>
      val k = s.kind.ordinal
      val near = s.x < 0f || play.sees(s.x, s.y, 200f)
      // A few of a kind at once sound like many.
      if(near && perKind(k) < 2) {
        perKind(k) += 1
        val distance = if(s.x < 0f) 0f else dist(s.x, s.y, play.left + play.viewWidth / 2f, play.top + play.viewHeight / 2f) / play.viewWidth
        val base = s.kind match {
          case SoundKind.Sling | SoundKind.Spark | SoundKind.Sting | SoundKind.Flame => 0.16f
          case SoundKind.Dig | SoundKind.Rocket => 0.24f
          case SoundKind.Built | SoundKind.Place => 0.28f
          case SoundKind.Chomp | SoundKind.Death | SoundKind.Spray | SoundKind.Beam => 0.2f
          case SoundKind.Zap | SoundKind.Bolt | SoundKind.Lob => 0.28f
          case SoundKind.Boom | SoundKind.Break => 0.36f
          case _ => 0.45f
        }
        if(!play.attract) play.services.play(s.kind, scala.math.max(0.02f, base * (1f - 0.6f * distance)) * play.profile.soundVolume, 1f)
      }
    }
    play.world.sounds.clear()
  }
}

object CameraSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.camera")
  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    play.updateCamera(context.dt)
    play.terrain.update(context.dt, 1f)
    play.view.look(play.left, play.top, play.left + play.viewWidth, play.top + play.viewHeight, play.time)
  }
}

/** The land, and what stands on it. */
object LandRenderSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.land")
  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val draw = context.global[Render2D]
    val (x0, y0, x1, y1) = (play.left, play.top, play.left + play.viewWidth, play.top + play.viewHeight)
    draw.world(0f) { p =>
      play.terrain.drawFloors(p, x0, y0, x1, y1)
      if(play.profile.ripples) {
        play.terrain.drawRipples(p, x0, y0, x1, y1, play.time)
        if(!play.world.frozen && play.effects.water != null) p.effect(play.effects.water, a = play.time, b = 0.16f)(play.terrain.drawWater(p, x0, y0, x1, y1))
      }
      play.decals.draw(p, x0, y0, x1, y1)
      play.view.drawRoots(p)
    }
    draw.world(2f)(play.terrain.drawSolids(_, x0, y0, x1, y1))
    draw.world(3f)(play.view.drawBuildings)
    draw.world(4f) { p =>
      play.view.drawFires(p)
      play.view.drawConditions(p, signs = !play.attract && play.profile.signs)
      if(play.profile.wires > 0.02f) play.view.drawWires(p, play.profile.wires)
    }
  }
}

/** Shots, particles, weather and light. */
object AirRenderSystem extends GameSystem {
  override val id: SystemId = SystemId("hearthwheel.air")
  override def run(context: GameContext)(using Scope): Unit = {
    val play = context.global[Play]
    val draw = context.global[Render2D]
    val (x0, y0, w, h) = (play.left, play.top, play.viewWidth, play.viewHeight)
    val between = if(play.running) context.global[FixedStepProgress].value else 0f
    draw.world(6f)(play.view.drawShots(_, between))
    draw.world(8f) { p =>
      play.particles.draw(p, x0, y0, x0 + w, y0 + h)
      play.view.drawDomes(p, play.effects)
    }
    draw.world(9f) { p =>
      val now = play.world.seasonIndex
      val t = play.terrain.seasonBlend
      val before = play.terrain.lastSeason
      val light = Tint.mix(play.weather.ambience(before), play.weather.ambience(now), t)
      p.rect(x0 - 2f, y0 - 2f, w + 4f, h + 4f, light)
      play.view.drawLights(p, lerp(play.weather.glow(before), play.weather.glow(now), t))
      if(play.profile.clouds) play.weather.clouds(p, now, x0, y0, w, h, play.time)
      if(play.profile.weather) {
        val strong = if(play.world.sky.event != null) 2.4f else 1f
        if(t < 1f && before != now) play.weather.draw(p, before, 1f - t, x0, y0, w, h, play.time)
        play.weather.draw(p, now, t * strong, x0, y0, w, h, play.time)
        if(play.world.sky.is(SkyEvent.Blizzard)) p.rect(x0 - 2f, y0 - 2f, w + 4f, h + 4f, Color(0.85f, 0.92f, 1f, 0.16f))
        if(play.world.sky.is(SkyEvent.Heatwave)) p.rect(x0 - 2f, y0 - 2f, w + 4f, h + 4f, Color(1f, 0.6f, 0.2f, 0.10f))
        if(play.world.sky.is(SkyEvent.Cloudburst)) p.rect(x0 - 2f, y0 - 2f, w + 4f, h + 4f, Color(0.1f, 0.18f, 0.3f, 0.2f))
      }
    }
  }
}
