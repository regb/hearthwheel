package hearthwheel
package core

import scala.collection.mutable

import sgl.engine.gameobject.Color
import sgl.engine.gameobject.render2d.{Painter2D, Texture2D}
import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** Draws what stands on the land. */
final class WorldView(world: Valley, art: Gallery, particles: Particles) {
  private val T = Valley.Tile.toFloat
  private val rng = new Rng(7L)
  // What is on the screen, gathered once a frame.
  private val visible = mutable.ArrayBuffer.empty[Building]
  private var x0 = 0f
  private var y0 = 0f
  private var x1 = 0f
  private var y1 = 0f
  private var time = 0f
  def visibleBuildings: Int = visible.size
  def shown: collection.Seq[Building] = visible

  private def isShown(b: Building): Boolean = {
    val margin = 6 * T
    b.cx > x0 - margin && b.cx < x1 + margin && b.cy > y0 - margin && b.cy < y1 + margin
  }

  def look(left: Float, top: Float, right: Float, bottom: Float, seconds: Float): Unit = {
    x0 = left; y0 = top; x1 = right; y1 = bottom
    time = seconds
    visible.clear()
    var i = 0
    while(i < world.buildings.size) {
      val b = world.buildings(i)
      if(b.alive && isShown(b)) visible += b
      i += 1
    }
  }

  // ------------------------------------------------------------ buildings

  private val Shade = Color(0.08f, 0.05f, 0.1f, 0.15f)
  private val SoftShade = Color(1f, 1f, 1f, 0.34f)
  private def round(b: Block): Boolean = b.role match {
    case _: Role.Turret => b.size == 1
    case Role.Pylon(_, _) | Role.Brazier(_, _) | Role.Tap(_) | Role.Pump(_) => true
    case Role.Nest | Role.Thorn(_, _, _) | Role.Pipe | Role.Caltrop(_) | Role.Hedge | Role.Pod(_) => true
    case _ => false
  }

  def drawBuildings(p: Painter2D): Unit = {
    var i = 0
    while(i < visible.size) {
      val b = visible(i)
      if(!b.isInstanceOf[BeltBuilding]) {
        val s = b.size * T
        // Round things cast round shadows.
        if(round(b.block)) p.sprite(art.shadow, b.cx + 4f, b.cy + 6f, s * 1.2f, s * 1.2f, 0f, SoftShade)
        else {
          p.rect(b.x * T + 5f, b.y * T + 5f, s - 4f, s - 2f, Shade)
          p.rect(b.x * T + 7f, b.y * T + 8f, s - 4f, s - 3f, Shade)
        }
      }
      i += 1
    }
    // Belts go below everything else, with what they carry.
    i = 0
    while(i < visible.size) {
      visible(i) match {
        case b: BeltBuilding => belt(p, b)
        case b if b.block.isPipe => pipe(p, b)
        case _ => ()
      }
      i += 1
    }
    i = 0
    while(i < visible.size) {
      visible(i) match {
        case b: BeltBuilding => carried(p, b)
        case _ => ()
      }
      i += 1
    }
    i = 0
    while(i < visible.size) {
      val b = visible(i)
      if(!b.isInstanceOf[BeltBuilding] && !b.block.isPipe) building(p, b)
      i += 1
    }
  }

  private val RootShade = Color(0.75f, 0.35f, 0.6f, 0.2f)
  private val RootsWarm = Array.tabulate(4)(n => Color(1f, 0.96f, 1f, 0.42f + 0.12f * n))
  private val RootsCold = Array.tabulate(4)(n => Color(0.8f, 0.86f, 0.95f, 0.4f + 0.1f * n))
  /** The Bramble underfoot. */
  def drawRoots(p: Painter2D): Unit = if(world.roots.size > 0) {
    val tx0 = clamp(floor(x0 / T) - 1, 0, world.width - 1)
    val tx1 = clamp(floor(x1 / T) + 1, 0, world.width - 1)
    val ty0 = clamp(floor(y0 / T) - 1, 0, world.height - 1)
    val ty1 = clamp(floor(y1 / T) + 1, 0, world.height - 1)
    var y = ty0
    while(y <= ty1) {
      var x = tx0
      while(x <= tx1) {
        val n = world.roots.thick(y * world.width + x)
        if(n > 0) {
          val g = x * 7 + y * 13
          val s = T * (1.05f + 0.12f * n)
          p.sprite(art.shadow, (x + 0.5f) * T, (y + 0.5f) * T, T * 1.9f, T * 1.9f, 0f, RootShade)
          p.sprite(art.roots(g & 3), (x + 0.5f) * T, (y + 0.5f) * T, s, s, ((g >> 2) & 3) * Pi / 2f, if(world.frozen) RootsCold(n) else RootsWarm(n))
        }
        x += 1
      }
      y += 1
    }
  }

  private def belt(p: Painter2D, b: BeltBuilding): Unit = {
    val swift = b.block eq Blocks.SwiftBelt
    val covered = b.block eq Blocks.CoveredBelt
    val frame = if(b.count > 0 || b.moving > 0f) ((time * b.perSecond * 0.25f * 16f * (if(b.moving > 0.05f) 1f else 0f)).toInt & 7) else 0
    val angle = b.rotation * Pi / 2f
    if(!b.fromBack && (b.fromLeft != b.fromRight)) {
      // A bend: the picture goes from the top to the right.
      val t = if(covered) art.coveredTurn(frame) else if(swift) art.swiftTurn(frame) else art.beltTurn(frame)
      if(b.fromLeft) p.sprite(t, b.cx, b.cy, T, T, angle)
      else p.sprite(t, b.cx, b.cy, T, T, angle, Color.White, 0f, 1f, 1f, 0f)
    } else {
      p.sprite(if(covered) art.covered(frame) else if(swift) art.swift(frame) else art.belt(frame), b.cx, b.cy, T, T, angle)
      val mouth = if(covered) art.coveredIn else if(swift) art.swiftIn else art.beltIn
      if(b.fromLeft) mouthAt(p, mouth, b, (b.rotation + 3) & 3)
      if(b.fromRight) mouthAt(p, mouth, b, (b.rotation + 1) & 3)
    }
    if(b.hp < b.maxHp) cracks(p, b)
  }
  private def mouthAt(p: Painter2D, t: Texture2D, b: Building, side: Int): Unit =
    p.sprite(t, b.cx + Dir.dx(side) * (T / 2f - 3.5f), b.cy + Dir.dy(side) * (T / 2f - 3.5f), T, 7f, (side + 1) * Pi / 2f)

  private def carried(p: Painter2D, b: BeltBuilding): Unit = {
    val dx = Dir.dx(b.rotation)
    val dy = Dir.dy(b.rotation)
    var i = b.count - 1
    while(i >= 0) {
      val along = (b.at(i) - 0.5f) * T
      val off = b.side(i) * T
      // The left of the way it goes is at a quarter turn back.
      val ix = b.cx + dx * along + (-dy) * off
      val iy = b.cy + dy * along + dx * off
      p.sprite(art.items(b.items(i)), ix, iy, 13f, 13f)
      i -= 1
    }
  }

  private def pipe(p: Painter2D, b: Building): Unit = {
    val kind = if(b.fluid == null || b.fluid.stored <= 0.5f) null else b.fluid.fluid
    val around = b.neighbours
    var arms = 0
    var i = 0
    while(i < around.length) {
      val n = around(i)
      if(n.team == b.team && n.isPlumbed && !(n.block.coolable && n.tanks.isEmpty)) {
        val d = b.sideOf(n)
        arms |= 1 << d
        p.sprite(art.pipeArm, b.cx, b.cy, T, T, d * Pi / 2f)
      }
      i += 1
    }
    // A straight piece needs no joint.
    if(arms != 5 && arms != 10) p.sprite(art.pipeCore, b.cx, b.cy, T, T)
    if(kind != null) {
      val phase = (time * 1.2f + (b.x + b.y) * 0.23f) % 1f
      p.sprite(art.mote, b.cx, b.cy, 9f, 9f, 0f, Tint(kind.color, 0.4f + 0.4f * sin(phase * Pi)))
    }
    if(b.hp < b.maxHp) cracks(p, b)
  }

  private def building(p: Painter2D, b: Building): Unit = {
    val s = b.size * T
    val id = b.block.id
    val base = art.base(id)
    b match {
      case site: SiteBuilding => this.site(p, site)
      case _ =>
        if(base != null) p.sprite(base, b.cx, b.cy, s, s)
        details(p, b, s, id)
        if(b.hp < b.maxHp) cracks(p, b)
        if(b.hitAgo < 0.12f && base != null) p.additive(p.sprite(base, b.cx, b.cy, s, s, 0f, Color(1f, 1f, 1f, 0.7f)))
    }
  }

  private def details(p: Painter2D, b: Building, s: Float, id: Int): Unit = b match {
    case d: DrillBuilding =>
      if(d.ore != null) p.sprite(art.items(d.ore.id), b.cx + s * 0.31f, b.cy + s * 0.31f, 12f, 12f)
      p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 3f)
    case t: TurretBuilding =>
      val kick = t.recoil * (if(b.size == 1) 3f else 6f)
      t.role.kind match {
        case TurretKind.Bell =>
          val r = 1f + t.strength * 0.12f
          p.sprite(art.top(id), b.cx, b.cy, s * r, s * r, b.spin * 0.4f)
          if(b.activity > 0.05f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 1.1f, s * 1.1f, 0f, Color(0.55f, 0.9f, 1f, 0.25f * b.activity + 0.6f * t.strength)))
        case TurretKind.Swatter =>
          p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 1.4f)
          if(b.activity > 0.1f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 0.8f, s * 0.8f, 0f, Color(1f, 0.85f, 0.5f, 0.2f + 0.6f * t.strength)))
        case _ =>
          p.sprite(art.shadow, b.cx + 3f, b.cy + 4f, s * 0.8f, s * 0.8f, 0f, Color(1f, 1f, 1f, 0.25f))
          p.sprite(art.top(id), b.cx - cos(t.aim) * kick, b.cy - sin(t.aim) * kick, s, s, t.aim)
          if(t.coolant != null && t.activity > 0.2f && rng.chance(0.03f)) particles.steam(b.cx, b.cy, t.coolant.color)
      }
    case c: CrafterBuilding =>
      if(c.block eq Blocks.Gearworks) {
        p.sprite(art.gearBig, b.x * T + 36f, b.y * T + 39f, 46f, 46f, b.spin * 1.4f)
        p.sprite(art.gearSmall, b.x * T + 64f, b.y * T + 64f, 34f, 34f, -b.spin * 1.9f + 0.2f)
      } else if(c.block eq Blocks.PowderMill) p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 1.5f)
      else if(art.hot(id) != null) {
        val flicker = 0.85f + 0.15f * sin(time * 9f + b.x * 1.7f)
        if(b.activity > 0.02f) p.sprite(art.hot(id), b.cx, b.cy, s, s, 0f, Tint.alpha(b.activity * flicker))
        if(b.activity > 0.3f && rng.chance(0.02f)) particles.smoke(b.cx - s * 0.36f, b.cy - s * 0.36f, dark = true)
      }
    case g: GeneratorBuilding => g.role.source match {
      case PowerSource.Wind =>
        p.sprite(art.shadow, b.cx + 8f, b.cy + 12f, 84f, 84f, 0f, Color(1f, 1f, 1f, 0.13f))
        p.sprite(art.top(id), b.cx, b.cy, 90f, 90f, b.spin * 2.2f + b.x)
      case PowerSource.Stream => p.sprite(art.wheelTop((b.spin * 8f).toInt & 3), b.cx, b.cy, s, s)
      case PowerSource.Sun => if(b.activity > 0.1f) p.additive(p.sprite(art.glow, b.cx - 6f, b.cy - 6f, 44f, 44f, 0f, Color(1f, 0.95f, 0.7f, scala.math.min(0.6f, 0.3f * b.activity))))
      case PowerSource.Fire => if(b.activity > 0.05f) {
        flame(p, b.cx, b.cy - 10f, 26f, b.activity)
        if(rng.chance(0.03f)) particles.smoke(b.cx + s * 0.31f, b.cy - s * 0.31f, dark = true)
      }
    }
    case c: ConduitBuilding => if(c.fluid != null && c.fluid.fluid != null && c.fluid.capacity > 0f) {
      val share = c.fluid.stored / c.fluid.capacity
      p.sprite(art.cisternFill, b.cx, b.cy, s, s, time * 0.15f, Tint(c.fluid.fluid.color, 0.25f + 0.75f * share))
    }
    case c: CulvertBuilding => if(c.fluid != null && c.fluid.fluid != null) p.sprite(art.mote, b.cx, b.cy, 13f, 13f, 0f, Tint(c.fluid.fluid.color, 0.9f))
    case _: BurnerBuilding => if(b.activity > 0.02f) p.sprite(art.hot(id), b.cx, b.cy, s, s, 0f, Tint.alpha(b.activity))
    case c: CatapultBuilding =>
      p.sprite(art.top(id), b.cx - cos(c.aim) * c.recoil * 8f, b.cy - sin(c.aim) * c.recoil * 8f, s, s, c.aim)
    case w: PressBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, w.spin * 1.2f)
    case w: ChillerBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, w.spin * 6f)
    case g: GardenBuilding => p.sprite(art.crop, b.cx, b.cy, s, s, 0f, if(world.frozen) Color(0.7f, 0.6f, 0.5f, 0.5f) else Tint.alpha(0.3f + 0.7f * g.progress))
    case g: BoilerBuilding =>
      if(g.activity > 0.05f) {
        p.sprite(art.hot(id), b.cx, b.cy, s, s, 0f, Tint.alpha(g.activity))
        if(rng.chance(0.06f)) particles.smoke(b.cx, b.cy - s * 0.2f, dark = false)
      }
    case _: VentBuilding =>
      p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 3f)
      if(rng.chance(0.03f)) particles.smoke(b.cx, b.cy, dark = false)
    case r: ReactorBuilding =>
      val pulse = 0.85f + 0.15f * sin(time * (3f + r.heat * 12f))
      if(r.activity > 0.02f) p.sprite(art.hot(id), b.cx, b.cy, s, s, 0f, Tint.alpha(r.activity * pulse))
      if(r.heat > 0.4f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 1.3f, s * 1.3f, 0f, Color(1f, 0.3f, 0.1f, (r.heat - 0.4f) * pulse)))
    case r: RatchetBuilding =>
      p.sprite(art.base(id), b.cx, b.cy, s, s, r.rotation * Pi / 2f)
      if(r.flow > 0.1f) p.additive(p.sprite(art.mote, b.cx, b.cy, 14f, 14f, 0f, Color(1f, 0.9f, 0.5f, 0.6f)))
    case a: AegisBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, time * 0.5f, if(a.broken) Color(0.4f, 0.4f, 0.5f) else Color.White)
    case m: MetronomeBuilding =>
      p.sprite(art.top(id), b.cx, b.cy, s, s, sin(time * 5f) * 0.4f * m.activity)
    case w: SwitchBuilding => p.sprite(if(w.on) art.switchOn else art.switchOff, b.cx, b.cy, s, s)
    case g: GovernorBuilding => p.sprite(art.mote, b.cx + 8f, b.cy - 8f, 9f, 9f, 0f, if(g.lets) Tint(0x9ce07a) else Tint(0xff7a5c))
    case r: RefitBuilding => if(r.inside != null) {
      val inner = s * 0.5f
      p.sprite(art.bodies(r.inside.id), b.cx, b.cy, inner, inner, -Pi / 2f, Tint.alpha(1f - r.progress))
      p.sprite(art.bodies(r.becomes.id), b.cx, b.cy, inner, inner, -Pi / 2f, Tint.alpha(r.progress))
      p.additive(p.sprite(art.glow, b.cx, b.cy, s, s, 0f, Color(0.6f, 0.9f, 1f, 0.25f + 0.15f * sin(time * 9f))))
    }
    case w: WagonBuilding => if(w.progress > 0f) p.rect(b.cx - 26f, b.cy + s * 0.32f, 52f * w.progress, 4f, Tint.Gold)
    case _: FlywheelBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, time * 5f * b.activity)
    case _: PumpBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 5f)
    case z: BrazierBuilding => if(z.activity > 0.05f) {
      flame(p, b.cx, b.cy - 5f, 26f, z.activity)
      if(rng.chance(0.06f)) particles.ember(b.cx, b.cy - 8f)
    }
    case r: RadiatorBuilding => if(r.activity > 0.05f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 1.2f, s * 1.2f, 0f, Color(1f, 0.5f, 0.25f, 0.4f * r.activity)))
    case _: AlmanacBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, (world.seasonIndex + world.seasonProgress) * Pi / 2f)
    case w: GreatWheelBuilding =>
      var k = 0
      while(k < 4) {
        val f = w.filled(k)
        if(f > 0f) p.sprite(art.spokes(k), b.cx, b.cy, s, s, k * Pi / 2f, Tint.alpha(0.25f + 0.75f * f))
        k += 1
      }
      p.sprite(art.top(id), b.cx, b.cy, s, s, b.spin * 0.6f)
      if(w.activity > 0.1f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 0.8f, s * 0.8f, 0f, Color(1f, 0.85f, 0.5f, 0.35f * w.activity)))
    case _: HearthBuilding =>
      val k = 1f + (b.size - 3) * 0.28f
      p.sprite(art.hearthRing, b.cx, b.cy, s, s, time * 0.15f + b.spin * 0.2f)
      flame(p, b.cx, b.cy - 10f * k, 50f * k, 1f)
      flame(p, b.cx - 9f * k, b.cy - 4f * k, 30f * k, 0.9f)
      flame(p, b.cx + 10f * k, b.cy - 3f * k, 28f * k, 0.9f)
      if(rng.chance(0.12f)) particles.ember(b.cx + rng.range(-10f, 10f), b.cy - 14f)
    case w: SorterBuilding => if(w.config >= 0) p.sprite(art.items(w.config), b.cx, b.cy, 15f, 15f)
    case w: HatchBuilding => if(w.config >= 0) p.sprite(art.items(w.config), b.cx, b.cy + 9f, 12f, 12f)
    case s0: SplitterBuilding => if(s0.held >= 0) p.sprite(art.items(s0.held), b.cx, b.cy, 13f, 13f)
    case m: MenderBuilding => if(m.pulse > 0f) p.additive(p.sprite(art.ring, b.cx, b.cy, (1f - m.pulse) * b.block.range * T * 2.1f, (1f - m.pulse) * b.block.range * T * 2.1f, 0f, Color(0.6f, 1f, 0.6f, m.pulse * 0.5f)))
    case w: WorkshopBuilding => if(w.progress > 0.02f) {
      val inside = if(w.block eq Blocks.Foundry) 54f else 34f
      p.sprite(art.bodies(w.kind.id), b.cx, b.cy, inside * 0.8f, inside * 0.8f, -Pi / 2f, Tint.alpha(0.25f + 0.7f * w.progress))
    }
    case _: NestBuilding =>
      val beat = 1f + 0.06f * sin(time * 3f) + 0.03f * sin(time * 6f + 1f)
      p.sprite(art.nestHeart, b.cx, b.cy, 40f * beat, 40f * beat, 0f)
      p.additive(p.sprite(art.glow, b.cx, b.cy, 110f * beat, 110f * beat, 0f, Color(1f, 0.35f, 0.3f, 0.22f)))
    case pod: PodBuilding =>
      // It swells as what is inside is about to come out.
      val k = 0.75f + 0.45f * pod.progress + 0.05f * sin(time * 4f + pod.x)
      p.sprite(art.top(id), b.cx, b.cy, s * k, s * k, 0f, Tint.alpha(0.6f + 0.4f * pod.progress))
      p.additive(p.sprite(art.glow, b.cx, b.cy, s * 1.4f * k, s * 1.4f * k, 0f, Color(1f, 0.6f, 0.3f, 0.2f * pod.progress)))
    case sign: SignBuilding =>
      val t = sign.shown.toString
      val size = if(t.length <= 2) 13f else if(t.length == 3) 10f else 8f
      p.text(t, sgl.geometry.Vec2(b.cx - sgl.engine.gameobject.render2d.Text2D.width(t, size, true) / 2f, b.cy - 2f - size * 0.75f), size, Tint(0xffd98a), true)
    case loom: LoomBuilding =>
      if(loom.fired > 0f) p.additive(p.sprite(art.glow, b.cx, b.cy, s * 1.5f, s * 1.5f, 0f, Color(1f, 0.85f, 0.5f, 0.4f * loom.fired)))
      if(loom.activity > 0.05f) p.rect(b.x * T + 13f, b.y * T + 14f + ((time * 14f) % 28f), 38f, 1.6f, Color(1f, 0.9f, 0.6f, 0.7f * loom.activity))
    case t: ThornBuilding => p.sprite(art.top(id), b.cx, b.cy, s, s, t.aim)
    case _ =>
      if(art.top(id) != null) p.sprite(art.top(id), b.cx, b.cy, s, s)
  }

  private def flame(p: Painter2D, x: Float, y: Float, size: Float, alpha: Float): Unit = {
    val i = (time * 11f + x * 0.13f).toInt & 3
    val sway = 1f + 0.08f * sin(time * 7f + x)
    p.sprite(art.flames(i), x, y, size, size * sway, 0f, Tint.alpha(alpha))
    p.additive(p.sprite(art.glow, x, y + size * 0.15f, size * 1.9f, size * 1.9f, 0f, Color(1f, 0.55f, 0.2f, 0.35f * alpha)))
  }

  private def cracks(p: Painter2D, b: Building): Unit = {
    val h = b.healthShare
    if(h < 0.85f) {
      val s = b.size * T
      p.sprite(art.cracks(if(h < 0.35f) 2 else if(h < 0.6f) 1 else 0), b.cx, b.cy, s, s, ((b.x + b.y) & 3) * Pi / 2f, Tint.alpha(1f - h))
    }
  }

  private def site(p: Painter2D, s0: SiteBuilding): Unit = {
    val s = s0.size * T
    p.sprite(art.site, s0.cx, s0.cy, s, s)
    // It rises from the bottom.
    val done = s0.progress
    portrait(p, s0.target, s0.cx, s0.cy, s, s0.rotation, Color(1f, 1f, 1f, 0.28f))
    if(done > 0f) p.clip(sgl.geometry.Vec2(s0.x * T - 8f, s0.y * T + s * (1f - done)), sgl.geometry.Vec2(s + 16f, s * done + 8f)) {
      portrait(p, s0.target, s0.cx, s0.cy, s, s0.rotation, Color.White)
    }
    if(done < 1f) p.rect(s0.x * T, s0.y * T + s * (1f - done) - 1f, s, 2f, if(s0.removing) Color(1f, 0.5f, 0.35f, 0.9f) else Color(1f, 0.85f, 0.5f, 0.9f))
  }

  /** A block as it looks when nothing is going on. */
  def portrait(p: Painter2D, b: Block, cx: Float, cy: Float, s: Float, rotation: Int, color: Color): Unit = Portrait.draw(p, art, b, cx, cy, s, rotation, color)

  // ------------------------------------------------------------- overlays

  private var wireAlpha = 1f
  def drawWires(p: Painter2D, alpha: Float): Unit = {
    wireAlpha = alpha
    var i = 0
    while(i < visible.size) {
      visible(i) match {
        case pylon: PylonBuilding =>
          val power = if(pylon.power == null) 0f else pylon.power.satisfaction
          val color = Tint.mix(Tint(0x8c5a4a), Tint(0xffd98a), power).withAlpha(wireAlpha)
          var k = 0
          while(k < pylon.links.size) {
            val o = pylon.links(k)
            val drawn = o match { case q: PylonBuilding => q.id < pylon.id && isShown(q); case _ => false }
            if(!drawn && o.alive) wire(p, pylon.cx, pylon.cy, o.cx, o.cy, color, power)
            k += 1
          }
        case c: CulvertBuilding =>
          val d = c.destination
          if(d != null) {
            val color = if(c.fluid != null && c.fluid.fluid != null) Tint(c.fluid.fluid.color, 0.75f) else Color(0.6f, 0.7f, 0.75f, 0.6f)
            p.line(c.cx, c.cy, d.cx, d.cy, 7f, Color(0.15f, 0.1f, 0.12f, 0.35f))
            p.line(c.cx, c.cy, d.cx, d.cy, 3.5f, color)
          }
        case z: ZiplineBuilding =>
          val d = z.destination
          if(d != null) {
            p.line(z.cx, z.cy, d.cx, d.cy, 5f, Color(0.15f, 0.1f, 0.12f, 0.7f))
            p.line(z.cx, z.cy, d.cx, d.cy, 2.5f, Tint(0xd8b07a))
            val n = 3
            var k = 0
            while(k < n) {
              val t = (time * 0.8f + k.toFloat / n) % 1f
              p.sprite(art.arrow, lerp(z.cx, d.cx, t), lerp(z.cy, d.cy, t), 9f, 9f, ZiplineBuilding.towards(z.x, z.y, d.x, d.y) * Pi / 2f, Tint(0xffe9b0))
              k += 1
            }
          }
        case _ => ()
      }
      i += 1
    }
  }
  private def wire(p: Painter2D, ax: Float, ay: Float, bx: Float, by: Float, color: Color, power: Float): Unit = {
    // Sags a little in the middle.
    val mx = (ax + bx) / 2f
    val my = (ay + by) / 2f + 5f
    p.line(ax + 1f, ay + 3f, mx + 1f, my + 4f, 2.5f, Color(0f, 0f, 0f, 0.18f))
    p.line(mx + 1f, my + 4f, bx + 1f, by + 3f, 2.5f, Color(0f, 0f, 0f, 0.18f))
    p.line(ax, ay, mx, my, 3.4f, Color(0.16f, 0.1f, 0.12f, 0.85f * wireAlpha))
    p.line(mx, my, bx, by, 3.4f, Color(0.16f, 0.1f, 0.12f, 0.85f * wireAlpha))
    p.line(ax, ay, mx, my, 1.8f, color)
    p.line(mx, my, bx, by, 1.8f, color)
    if(power > 0.05f) p.additive {
      val t = (time * 1.1f + ax * 0.01f) % 1f
      val qx = if(t < 0.5f) lerp(ax, mx, t * 2f) else lerp(mx, bx, t * 2f - 1f)
      val qy = if(t < 0.5f) lerp(ay, my, t * 2f) else lerp(my, by, t * 2f - 1f)
      p.sprite(art.mote, qx, qy, 8f, 8f, 0f, Color(1f, 0.9f, 0.55f, 0.8f * power))
    }
  }

  /** Frost on what is cold, water over what is drowned, and signs on what lacks something. */
  def drawConditions(p: Painter2D, signs: Boolean): Unit = {
    val winter = world.frozen
    var i = 0
    while(i < visible.size) {
      val b = visible(i)
      val s = b.size * T
      if(b.block.chills && b.team == Valley.Player && !b.isInstanceOf[SiteBuilding]) {
        if(winter && !b.warm) p.sprite(art.frost, b.cx, b.cy, s, s, 0f, Tint.alpha(0.85f))
        if(b.flooded) {
          p.rect(b.x * T, b.y * T, s, s, Color(0.25f, 0.55f, 0.7f, 0.45f))
          p.sprite(art.ripples((time * 2f).toInt & 3), b.cx, b.cy, s, s, 0f, Tint.alpha(0.7f))
        }
      }
      if(signs && b.team == Valley.Player) sign(p, b)
      i += 1
    }
  }
  private def sign(p: Painter2D, b: Building): Unit = {
    val t: Texture2D = b match {
      case _ if b.stopped => art.cross
      case t: TurretBuilding if !t.hasAmmo => art.ammoIcon
      case _ if b.block.usesPower && b.wantsToWork && b.powerShare < 0.3f => art.boltIcon
      case _ if b.block.waterUse > 0f && b.wantsToWork && b.waterShare < 0.3f && !b.isInstanceOf[DrillBuilding] => art.dropIcon
      case t: TurretBuilding if t.block.usesPower && t.powerShare < 0.1f => art.boltIcon
      case t: TurretBuilding if t.block.waterUse > 0f && t.waterShare < 0.1f => art.dropIcon
      case _ => null
    }
    if(t != null) {
      val bob = sin(time * 4f + b.x) * 1.5f
      p.sprite(art.glow, b.cx, b.cy + bob, 30f, 30f, 0f, Color(0.1f, 0.05f, 0.1f, 0.6f))
      p.sprite(t, b.cx, b.cy + bob, 15f, 15f)
    }
  }

  def drawShots(p: Painter2D, between: Float): Unit = {
    val dt = between / 60f
    var i = 0
    while(i < world.shots.flying.size) {
      val f = world.shots.flying(i)
      val x = f.x + f.vx * dt
      val y = f.y + f.vy * dt
      if(x > x0 - 40f && x < x1 + 40f && y > y0 - 60f && y < y1 + 60f) {
        val t = art.shot(f.shot)
        val s = f.shot.size * 1.5f
        if(f.lobbed > 0f) {
          val u = scala.math.min(1f, f.age / f.lobbed)
          val height = 4f * u * (1f - u) * (if(f.shot.speed > 0f) 70f * f.lobbed else 14f)
          p.sprite(art.shadow, x, y, s * 0.8f, s * 0.6f, 0f, Color(1f, 1f, 1f, 0.4f))
          val k = 1f + height * 0.006f
          p.sprite(t, x, y - height, s * k, s * k, f.age * 7f)
        } else {
          val long = f.shot.sprite == "shot_bolt"
          p.additive(p.sprite(art.spark, x - cos(f.angle) * s * 0.8f, y - sin(f.angle) * s * 0.8f, s * 2.4f, s * 0.6f, f.angle, Tint(f.shot.color, 0.5f)))
          p.sprite(t, x, y, s, if(long) s * 0.375f else s, f.angle)
        }
      }
      i += 1
    }
    i = 0
    while(i < visible.size) {
      visible(i) match {
        case t: TurretBuilding if t.role.kind == TurretKind.Lance && t.strength > 0.02f => beam(p, t)
        case t: TurretBuilding if t.role.kind == TurretKind.Grapnel && t.strength > 0.05f && t.target != null =>
          p.line(t.cx, t.cy, t.target.x, t.target.y, 3.4f, Color(0.16f, 0.1f, 0.12f, 0.9f))
          p.line(t.cx, t.cy, t.target.x, t.target.y, 1.6f, Tint(0xb4c4d4))
        case c: CatapultBuilding => c.crates.foreach { crate =>
          val u = scala.math.min(1f, crate.age / crate.seconds)
          val x = lerp(c.cx, crate.to.cx, u)
          val y = lerp(c.cy, crate.to.cy, u)
          val h = 4f * u * (1f - u) * 60f
          p.sprite(art.shadow, x, y, 16f, 12f, 0f, Color(1f, 1f, 1f, 0.4f))
          p.sprite(art.crate, x, y - h, 18f + h * 0.12f, 18f + h * 0.12f, crate.age * 3f)
        }
        case _ => ()
      }
      i += 1
    }
  }
  private def beam(p: Painter2D, t: TurretBuilding): Unit = {
    val start = 44f
    val length = t.beam - start
    val mx = t.cx + cos(t.aim) * (start + length / 2f)
    val my = t.cy + sin(t.aim) * (start + length / 2f)
    val w = (20f + 4f * sin(time * 30f)) * scala.math.min(1f, t.strength)
    p.additive {
      p.sprite(art.beam, mx, my, length, w * 2.2f, t.aim, Color(1f, 0.6f, 0.2f, 0.5f))
      p.sprite(art.beam, mx, my, length, w, t.aim, Color(1f, 0.9f, 0.6f, 1f))
      p.sprite(art.glow, t.cx + cos(t.aim) * start, t.cy + sin(t.aim) * start, w * 3f, w * 3f, 0f, Color(1f, 0.8f, 0.4f, 0.9f))
    }
  }

  /** Fires on the ground and on what is built. */
  def drawFires(p: Painter2D): Unit = {
    var i = 0
    val fires = world.fires.burning
    while(i < fires.size) {
      val f = fires(i)
      val x = (f.x + 0.5f) * T
      val y = (f.y + 0.5f) * T
      if(x > x0 - 40f && x < x1 + 40f && y > y0 - 40f && y < y1 + 40f) {
        val a = scala.math.min(1f, f.left) * scala.math.min(1f, f.age * 2f)
        flame(p, x, y - 5f, 26f + 4f * sin(time * 5f + f.x), a)
      }
      i += 1
    }
  }

  /** Domes over what they shield. */
  def drawDomes(p: Painter2D, effects: Effects): Unit = {
    var i = 0
    while(i < visible.size) {
      visible(i) match {
        case a: AegisBuilding if a.open =>
          val r = a.reach * 2.1f
          val strong = a.strength / a.full
          val color = Tint.mix(Color(1f, 0.5f, 0.5f), Color(0.55f, 0.85f, 1f), strong)
          val shown = color.withAlpha(0.3f + 0.5f * a.struck)
          p.additive {
            if(effects.heat != null) p.effect(effects.heat, a.cx, a.cy, r / 2f, time)(p.sprite(art.dome, a.cx, a.cy, r, r, 0f, shown))
            else p.sprite(art.dome, a.cx, a.cy, r, r, 0f, shown)
          }
        case _ => ()
      }
      i += 1
    }
  }

  /** Warm light around fires. */
  def drawLights(p: Painter2D, strength: Float): Unit = if(strength > 0.01f) p.additive {
    var i = 0
    while(i < visible.size) {
      val b = visible(i)
      val l = b.block.light
      if(l > 0f && b.activity > 0.02f) {
        val r = (b.size * T) * 1.6f + 90f * l
        val flicker = 0.92f + 0.08f * sin(time * 6f + b.x * 2.3f + b.y)
        p.sprite(art.glow, b.cx, b.cy, r * 2f, r * 2f, 0f, Color(1f, 0.6f, 0.28f, 0.32f * strength * flicker * scala.math.min(1f, b.activity)))
      }
      i += 1
    }
  }
}

/** A block as it looks when nothing is going on, wherever it is to be drawn. */
object Portrait {
  def draw(p: Painter2D, art: Gallery, b: Block, cx: Float, cy: Float, s: Float, rotation: Int, color: Color): Unit = {
    val k = s / (b.size * Valley.Tile)
    b.role match {
      case Role.Belt(_) => p.sprite(if(b eq Blocks.CoveredBelt) art.covered(0) else if(b eq Blocks.SwiftBelt) art.swift(0) else art.belt(0), cx, cy, s, s, rotation * Pi / 2f, color)
      case Role.Pipe =>
        p.sprite(art.pipeArm, cx, cy, s, s, 0f, color)
        p.sprite(art.pipeArm, cx, cy, s, s, Pi, color)
        p.sprite(art.pipeCore, cx, cy, s, s, 0f, color)
      case _ =>
        if(art.base(b.id) != null) p.sprite(art.base(b.id), cx, cy, s, s, 0f, color)
        b.role match {
          case _: Role.Turret => p.sprite(art.top(b.id), cx, cy, s, s, -Pi / 2f, color)
          case Role.Generator(PowerSource.Stream, _, _) => p.sprite(art.wheelTop(0), cx, cy, s, s, 0f, color)
          case Role.Generator(PowerSource.Wind, _, _) => p.sprite(art.top(b.id), cx, cy, 90f * k, 90f * k, 0.5f, color)
          case _ if b eq Blocks.Gearworks =>
            p.sprite(art.gearBig, cx - 12f * k, cy - 9f * k, 46f * k, 46f * k, 0f, color)
            p.sprite(art.gearSmall, cx + 16f * k, cy + 16f * k, 34f * k, 34f * k, 0.2f, color)
          case _ => if(art.top(b.id) != null) p.sprite(art.top(b.id), cx, cy, s, s, 0f, color)
        }
    }
  }
}
