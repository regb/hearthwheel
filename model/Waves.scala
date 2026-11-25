package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** When the Bramble comes, and with what. */
final class Waves(world: Valley) {
  import UnitKinds._
  var number = 0
  var countdown: Float = world.rules.firstWave
  var bossesSent = 0
  // Creatures still to come out of the nests, with the nest and what shields them.
  private final case class Coming(kind: UnitKind, nest: Int, shield: Float, guardian: Boolean)
  private val queue = mutable.ArrayDeque.empty[Coming]
  private var drip = 0f

  def waiting: Int = queue.size
  def foesLeft: Int = queue.size + world.creatures.count(c => c.alive && c.team == Valley.Bramble)
  /** Whether the countdown stands still until the last of them is gone. */
  def held: Boolean = world.rules.patient && foesLeft > 0

  def update(dt: Float): Unit = {
    if(world.rules.timer && !held) {
      countdown -= dt
      if(countdown <= 0f) {
        countdown = world.rules.waveSeconds
        launch()
      }
    }
    if(queue.nonEmpty) {
      drip -= dt
      if(drip <= 0f) {
        drip = scala.math.max(0.12f, 0.5f - queue.size * 0.01f)
        emerge(queue.removeHead())
      }
    }
  }

  def launchNow(): Unit = {
    countdown = world.rules.waveSeconds
    launch()
  }

  /** What each one of a wave wears against the first hits. */
  def shieldAt(n: Int): Float = scala.math.max(0, n - 12) * 4f * world.rules.threat

  private def launch(): Unit = {
    number += 1
    val nests = world.nests
    if(nests.nonEmpty) {
      val parts = compose(number, world.season)
      // Every third wave, what has been keeping watch around the pods marches with it.
      if(number % 3 == 0) {
        val brood = world.creatures.filter(c => c.alive && c.guarding)
        brood.foreach { c => c.guarding = false; c.home = -1 }
        if(brood.size >= 4) world.notices += Notice("The brood of the pods marches with them", 0xffb08a)
      }
      parts.foreach { case (kind, count) =>
        for(i <- 0 until count) queue.append(Coming(kind, i % nests.size, shieldAt(number), guardian = false))
      }
      // Every fifth wave one of them leads.
      if(number % 5 == 0 && parts.nonEmpty) {
        val leader = parts.map(_._1).maxBy(_.hp)
        queue.append(Coming(leader, number % nests.size, 120f + number * 18f, guardian = true))
        world.notices += Notice(s"A guardian ${leader.label} leads them", 0xffb08a)
      }
      val boss = world.season match {
        case Season.Winter => Some(Gnarl)
        case Season.Autumn if world.year >= 2 => Some(Queen)
        case _ => None
      }
      boss.foreach { kind =>
        val mark = world.year * 2 + (if(kind eq Gnarl) 1 else 0)
        if(bossesSent < mark && world.seasonProgress > 0.3f) {
          bossesSent = mark
          queue.append(Coming(kind, 0, (world.year - 1) * 800f, guardian = false))
          world.notices += Notice(s"${kind.label} has woken", 0xff9a7a)
          world.sound(SoundKind.Alarm)
        }
      }
      world.sound(SoundKind.Wave)
    }
  }

  /** The season a wave that many waves from now is likely to come in. */
  def seasonOf(ahead: Int): Season = {
    var seconds = scala.math.max(0f, countdown) + (ahead - 1) * world.rules.waveSeconds
    if(world.longWinter) return Season.Winter
    var s = world.seasonIndex
    var left = world.secondsLeft + world.held
    while(seconds > left && left > 0f) {
      seconds -= left
      s = (s + 1) % 4
      left = world.rules.seasons(s)
    }
    Season.fromOrdinal(s)
  }

  /** What a wave is made of. Pure, so that it can be announced ahead. */
  def compose(n: Int, season: Season): Vector[(UnitKind, Int)] = {
    val budget = (5f + n * 2.5f + n * n * 0.15f) * world.rules.threat
    val shares: Vector[(UnitKind, Float, Float, Int)] = season match {
      // Kind, share of the budget, price of one, first wave it comes in.
      case Season.Spring => Vector((Burr, 0.5f, 1f, 1), (Nettle, 0.3f, 2.2f, 3), (Catkin, 0.2f, 5f, 8), (Podder, 0.25f, 14f, 14), (Creepvine, 0.15f, 8f, 12), (Dewmoth, 0.1f, 6f, 10), (Briarhulk, 0.2f, 40f, 24))
      case Season.Summer => Vector((Burr, 0.3f, 1f, 1), (Nettle, 0.35f, 2.2f, 2), (Stumper, 0.25f, 9f, 8), (Catkin, 0.2f, 5f, 7), (Hornwing, 0.2f, 6f, 9), (Podder, 0.2f, 14f, 16), (Briarhulk, 0.25f, 40f, 22))
      case Season.Autumn => Vector((Dandel, 0.5f, 1.8f, 3), (Hornwing, 0.25f, 6f, 7), (Dewmoth, 0.1f, 6f, 8), (Stormseed, 0.25f, 18f, 13), (Burr, 0.2f, 1f, 1), (Nettle, 0.2f, 2.2f, 5), (Creepvine, 0.2f, 8f, 13))
      case Season.Winter => Vector((Frostling, 0.4f, 3f, 4), (Stumper, 0.35f, 9f, 5), (Nettle, 0.2f, 2.2f, 2), (Burr, 0.2f, 1f, 1), (Creepvine, 0.25f, 8f, 9), (Catkin, 0.15f, 5f, 10), (Podder, 0.2f, 14f, 18), (Briarhulk, 0.3f, 40f, 20))
    }
    shares.flatMap { case (kind, share, price, first) =>
      val count = if(n < first) 0 else scala.math.round(budget * share / price)
      if(count > 0) Some(kind -> count) else None
    }
  }

  def summonRimewood(): Unit = {
    queue.prepend(Coming(Rimewood, 0, 0f, guardian = false))
    for(_ <- 0 until 6) queue.append(Coming(Creepvine, 1, 60f, guardian = false))
    for(_ <- 0 until 14) queue.append(Coming(Frostling, 2, 20f, guardian = false))
    for(_ <- 0 until 6) queue.append(Coming(Stumper, 3, 60f, guardian = false))
    world.sound(SoundKind.Alarm)
  }

  private def emerge(next: Coming): Unit = {
    val kind = next.kind
    val nests = world.nests
    val (x, y) =
      if(nests.nonEmpty) {
        val nest = nests(next.nest % nests.size)
        (nest.cx, nest.cy)
      } else edge()
    val a = world.rng.float() * Tau
    // Just outside of the nest, which nothing can stand in.
    val c = world.spawn(kind, Valley.Bramble, x + cos(a) * 60f, y + sin(a) * 60f)
    c.shield += next.shield
    c.maxShield = c.shield
    c.guardian = next.guardian
    if(!kind.flies && world.blocks(c, c.tileX, c.tileY)) {
      val free = openTile(x, y)
      c.x = free._1
      c.y = free._2
    }
    world.fx(FxKind.Puff, c.x, c.y, 0f, 0)
  }
  // Where bosses come from when there is no nest left.
  private def edge(): (Float, Float) = {
    var best = (Valley.Tile * 2f, Valley.Tile * 2f)
    var far = -1f
    for(_ <- 0 until 60) {
      val x = world.rng.int(world.width)
      val y = world.rng.int(world.height)
      val px = (x + 0.5f) * Valley.Tile
      val py = (y + 0.5f) * Valley.Tile
      val d = if(world.hearth == null) 0f else dist(px, py, world.hearth.cx, world.hearth.cy)
      if(world.solidAt(x, y) == Solid.Open && !world.floorAt(x, y).water && world.groundField.reaches(x, y) && d > far) {
        far = d
        best = (px, py)
      }
    }
    best
  }
  private def openTile(x: Float, y: Float): (Float, Float) = {
    val tx = floor(x / Valley.Tile)
    val ty = floor(y / Valley.Tile)
    val found = for {
      r <- (2 to 8).iterator
      dy <- (-r to r).iterator
      dx <- (-r to r).iterator
      if world.inside(tx + dx, ty + dy) && world.buildingAt(tx + dx, ty + dy) == null
      if world.walkCost(world.tileIndex(tx + dx, ty + dy), Walk.Ground, Valley.Bramble) >= 0
    } yield ((tx + dx + 0.5f) * Valley.Tile, (ty + dy + 0.5f) * Valley.Tile)
    found.nextOption().getOrElse((x, y))
  }

  def write(out: Writer): Unit = {
    out.int(number); out.float(countdown); out.int(bossesSent)
    out.int(queue.size)
    queue.foreach { c => out.int(c.kind.id); out.int(c.nest); out.float(c.shield); out.int(if(c.guardian) 1 else 0) }
  }
  def read(in: Reader): Unit = {
    number = in.int(); countdown = in.float(); bossesSent = in.int()
    val n = in.int()
    for(_ <- 0 until n) queue.append(Coming(UnitKinds.all(in.int()), in.int(), in.float(), in.int() == 1))
  }
}
