package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** Fire on tiles: it eats at what is built there, and goes on to what burns. */
final class Fires(world: Valley) {
  final class Fire(val x: Int, val y: Int) { var left = 6f; var age = 0f; var spread = 1.5f }
  val burning = mutable.ArrayBuffer.empty[Fire]
  private val on = mutable.HashMap.empty[Int, Fire]
  private val Max = 300

  def start(x: Int, y: Int): Unit = if(world.inside(x, y) && !world.floorAt(x, y).water && burning.size < Max) {
    val i = world.tileIndex(x, y)
    on.get(i) match {
      case Some(f) => f.left = scala.math.max(f.left, 6f)
      case None =>
        val f = new Fire(x, y)
        burning += f
        on(i) = f
    }
  }
  def at(x: Int, y: Int): Boolean = world.inside(x, y) && on.contains(world.tileIndex(x, y))

  /** Puts out what burns around a point. */
  def douse(x: Float, y: Float, r: Float): Unit = burning.foreach { f =>
    if(dist2(x, y, (f.x + 0.5f) * Valley.Tile, (f.y + 0.5f) * Valley.Tile) < r * r) f.left = scala.math.min(f.left, 0.1f)
  }

  /** How well a building burns, from what is in it. */
  def flammability(b: Building): Float = {
    val inside = if(b.stock == null) 0f else {
      var sum = 0f
      var i = 0
      while(i < Items.Count) { sum += b.stock.amounts(i) * Items.all(i).burn; i += 1 }
      scala.math.min(1f, sum / 40f)
    }
    b match {
      case _: HearthBuilding | _: BrazierBuilding | _: BurnerBuilding => 0f
      case _: PlainBuilding => 0.05f
      case _: BeltBuilding => 0.12f
      case _ => 0.2f + inside
    }
  }

  /** What becomes of the goods in a building that is destroyed. */
  def burst(b: Building): Unit = if(b.team == Valley.Player && b.stock != null && !b.isInstanceOf[HearthBuilding]) {
    var fuel = 0f
    var bang = 0f
    var i = 0
    while(i < Items.Count) {
      fuel += b.stock.amounts(i) * Items.all(i).burn
      bang += b.stock.amounts(i) * Items.explosive(Items.all(i))
      i += 1
    }
    if(fuel > 6f) {
      val n = scala.math.min(6, 1 + (fuel / 12f).toInt)
      var k = 0
      while(k < n) { start(b.x + world.rng.int(b.size + 2) - 1, b.y + world.rng.int(b.size + 2) - 1); k += 1 }
    }
    if(bang > 10f) world.explode(b.cx, b.cy, 40f + scala.math.min(80f, bang), scala.math.min(300f, bang * 3f), 1 - b.team, 3f, 0f, air = false, ground = true, 0xffb05a)
  }

  def update(dt: Float): Unit = {
    // Rain and snow put it out; summer feeds it.
    val fade = world.sky.damp * (world.season match {
      case Season.Spring => 1.6f
      case Season.Summer => 0.7f
      case Season.Autumn => 1f
      case Season.Winter => 2f
    })
    var i = 0
    while(i < burning.size) {
      val f = burning(i)
      f.age += dt
      f.left -= dt * fade
      val b = world.buildingAt(f.x, f.y)
      if(b != null && b.team == Valley.Player) {
        val burns = flammability(b)
        f.left += dt * scala.math.min(0.9f, burns) * 0.9f
        b.damage(dt * 4f * (0.4f + burns))
        f.spread -= dt * (0.3f + burns) * (if(world.dry) 1.6f else 1f)
        if(f.spread <= 0f) {
          f.spread = 3.5f
          val d = world.rng.int(4)
          val nx = f.x + Dir.dx(d)
          val ny = f.y + Dir.dy(d)
          val n = world.buildingAt(nx, ny)
          if(n != null && n.team == Valley.Player && world.rng.chance(flammability(n))) start(nx, ny)
        }
      }
      if(world.rng.chance(dt * 5f)) world.fx(FxKind.Flame, (f.x + world.rng.float()) * Valley.Tile, (f.y + world.rng.float()) * Valley.Tile, 0f, 0)
      if(f.left <= 0f) {
        on.remove(world.tileIndex(f.x, f.y))
        val last = burning.remove(burning.size - 1)
        if(!(last eq f)) burning(i) = last
      } else i += 1
    }
  }
}

/** What the sky is up to, now and then, on top of the season. */
enum SkyEvent(val label: String, val text: String, val season: Season) {
  case Cloudburst extends SkyEvent("Cloudburst", "Fires go out, and the wheels race", Season.Spring)
  case Heatwave extends SkyEvent("Heatwave", "The sun is fierce, and everything burns", Season.Summer)
  case Gale extends SkyEvent("Gale", "The mills scream, and whatever flies is swept along", Season.Autumn)
  case Blizzard extends SkyEvent("Blizzard", "Warmth does not reach as far", Season.Winter)
}

final class Sky(world: Valley) {
  var event: SkyEvent = null
  var left = 0f
  private var next = 45f
  def is(e: SkyEvent): Boolean = event eq e
  def wind: Float = if(is(SkyEvent.Gale)) 1.5f else if(is(SkyEvent.Blizzard)) 1.4f else 1f
  def sun: Float = if(is(SkyEvent.Heatwave)) 1.4f else if(is(SkyEvent.Cloudburst)) 0.5f else if(is(SkyEvent.Blizzard)) 0.4f else 1f
  def stream: Float = if(is(SkyEvent.Cloudburst)) 1.5f else 1f
  def flying: Float = if(is(SkyEvent.Gale)) 1.35f else 1f
  def warmth: Float = if(is(SkyEvent.Blizzard)) 0.65f else 1f
  /** How fast fires die, against what is usual. */
  def damp: Float = if(is(SkyEvent.Cloudburst)) 3f else if(is(SkyEvent.Heatwave)) 0.6f else 1f

  def update(dt: Float): Unit = if(!world.rules.calm) {
    if(event != null) {
      left -= dt
      if(left <= 0f || event.season != world.season) end()
    } else {
      next -= dt
      if(next <= 0f) {
        next = world.rng.range(70f, 130f)
        if(world.rng.chance(0.6f)) begin(SkyEvent.values.find(_.season == world.season).get)
      }
    }
  }
  def begin(e: SkyEvent): Unit = {
    event = e
    left = world.rng.range(26f, 40f)
    world.notices += Notice(s"${e.label}: ${e.text}", 0xbfe0ff)
    if(e == SkyEvent.Blizzard) world.warmthChanged()
  }
  private def end(): Unit = {
    val was = event
    event = null
    if(was == SkyEvent.Blizzard) world.warmthChanged()
  }
  def write(out: Writer): Unit = { out.int(if(event == null) -1 else event.ordinal); out.float(left); out.float(next) }
  def read(in: Reader): Unit = {
    val e = in.int()
    event = if(e < 0) null else SkyEvent.fromOrdinal(e)
    left = in.float()
    next = in.float()
  }
}

/** What the Hearth had, over time, for the ledger. */
final class Ledger(world: Valley) {
  val Samples = 120
  val Every = 5f
  private var clock = 0f
  /** By item, the oldest first. */
  val history: Array[mutable.ArrayDeque[Int]] = Array.fill(Items.Count)(mutable.ArrayDeque.empty[Int])
  def update(dt: Float): Unit = {
    clock += dt
    if(clock >= Every) {
      clock -= Every
      Items.all.foreach { i =>
        val h = history(i.id)
        h.append(world.hearthStock(i))
        if(h.size > Samples) { val _ = h.removeHead() }
      }
    }
  }
}
