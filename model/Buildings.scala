package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** Something standing on the land. `x` and `y` are its top left tile. */
abstract class Building(val world: Valley, val block: Block, val x: Int, val y: Int, var rotation: Int, val team: Int) {
  import Valley.Tile

  final val size: Int = block.size
  final val cx: Float = (x + size / 2f) * Tile
  final val cy: Float = (y + size / 2f) * Tile
  var id: Int = 0
  var alive = true
  val maxHp: Float = block.hp
  var hp: Float = block.hp
  /** Seconds since it was last hit, to flash it and to stop menders overlapping. */
  var hitAgo = 10f
  var stock: Stock = null

  // Conditions, worked out by the world before each update.
  var warm = true
  /** Stopped by a switch or a governor, until then. */
  var stoppedUntil = -1f
  def stopped: Boolean = stoppedUntil >= world.time
  /** Hurried by a metronome, until then. */
  var hurriedUntil = -1f
  var hurry = 1f
  var burning = 0f
  var flooded = false
  /** 1 when all is well. */
  var condition = 1f
  /** How much of its work it is doing, eased for the view. */
  var activity = 0f
  /** A turning angle or scroll that moves with the work, for the view. */
  var spin = 0f

  // Networks.
  var power: PowerGraph = null
  /** The graph it is part of, for what fluids run through. */
  var fluid: FluidGraph = null
  /** The graphs it touches, for what gives or takes a fluid. */
  val tanks = mutable.ArrayBuffer.empty[FluidGraph]
  var heldFluid: Fluid = null
  val links = mutable.ArrayBuffer.empty[Building]
  /** What it was told, beyond a number: carried along by copies, and back to what is rebuilt. No spaces. */
  def note: String = ""
  def renote(text: String): Unit = { val _ = text }
  var charge = 0f
  var charge2 = 0f

  private var neighboursValue: Array[Building] = null
  private var dumpIndex = 0

  def isPowered: Boolean = block.usesPower
  def isPlumbed: Boolean = block.waterUse > 0f || block.conducts || block.coolable
  def powerOutput: Float = 0f
  def powerCapacity: Float = 0f
  def powerDemand: Float = if(block.usesPower && wantsToWork) block.powerUse else 0f
  /** How much of a fluid it would take, a second. */
  def fluidDemand(f: Fluid): Float = if(block.waterUse > 0f && (f eq block.fluid) && wantsToWork) block.waterUse else 0f
  def fluidMade: Float = 0f
  def fluidKind: Fluid = Fluid.Water
  def fluidCapacity: Float = 0f
  /** How much of what it would take of a fluid it gets. */
  final def fluidShare(f: Fluid): Float = {
    var best = 0f
    var i = 0
    while(i < tanks.size) {
      val t = tanks(i)
      if((t.fluid eq f) && t.satisfaction > best) best = t.satisfaction
      i += 1
    }
    best
  }
  def powerShare: Float = if(!block.usesPower) 1f else if(power == null) 0f else power.satisfaction
  def waterShare: Float = if(block.waterUse <= 0f) 1f else fluidShare(block.fluid)

  /** Whether it would work if it had power, which is when it asks for it. */
  def wantsToWork: Boolean = false

  def update(dt: Float): Unit = ()

  def accepts(item: Item, from: Building): Boolean = false
  def receive(item: Item, from: Building): Unit = ()

  /** What is drawn out of a store by a hatch. */
  def canGive(item: Item): Boolean = false
  def give(item: Item): Unit = ()

  def onPlaced(): Unit = ()
  def onRemoved(): Unit = ()
  /** Called when the buildings around it have changed. */
  def onNeighbours(): Unit = ()

  /** Tells `items` apart in saves and the settings panel. -1 for none. */
  def config: Int = -1
  def configure(value: Int): Unit = ()

  def write(out: Writer): Unit = ()
  def read(in: Reader): Unit = ()

  final def contains(tx: Int, ty: Int): Boolean = tx >= x && ty >= y && tx < x + size && ty < y + size
  final def healthShare: Float = hp / maxHp

  /** Buildings along its sides, each once. */
  final def neighbours: Array[Building] = {
    if(neighboursValue == null) {
      val found = mutable.ArrayBuffer.empty[Building]
      def look(tx: Int, ty: Int): Unit = {
        val b = world.buildingAt(tx, ty)
        if(b != null && !found.contains(b)) found += b
      }
      var i = 0
      while(i < size) { look(x + size, y + i); i += 1 }
      i = size - 1
      while(i >= 0) { look(x + i, y + size); i -= 1 }
      i = size - 1
      while(i >= 0) { look(x - 1, y + i); i -= 1 }
      i = 0
      while(i < size) { look(x + i, y - 1); i += 1 }
      neighboursValue = found.toArray
    }
    neighboursValue
  }
  final def neighboursChanged(): Unit = {
    neighboursValue = null
    onNeighbours()
  }

  /** The building it faces, for a one tile building. */
  final def ahead(direction: Int): Building = world.buildingAt(x + Dir.dx(direction), y + Dir.dy(direction))

  /** The side of this building where the other one is: 0 right, 1 down, 2 left, 3 up. */
  final def sideOf(other: Building): Int = {
    if(other.x >= x + size) 0
    else if(other.y >= y + size) 1
    else if(other.x + other.size <= x) 2
    else 3
  }

  /** Hands one item to a neighbour, in turns. */
  protected final def dump(item: Item): Boolean = {
    val around = neighbours
    if(around.length == 0) return false
    var i = 0
    while(i < around.length) {
      val index = (dumpIndex + i) % around.length
      val b = around(index)
      if(b.team == team && b.alive && b.accepts(item, this)) {
        b.receive(item, this)
        dumpIndex = (index + 1) % around.length
        return true
      }
      i += 1
    }
    false
  }

  protected final def ease(working: Float, dt: Float): Unit = {
    activity += (working - activity) * scala.math.min(1f, dt * 4f)
    spin += activity * dt
  }

  def damage(amount: Float): Unit = if(alive) {
    hitAgo = 0f
    hp -= amount
    if(hp <= 0f) world.destroyBuilding(this)
  }
  def heal(amount: Float): Unit = hp = scala.math.min(maxHp, hp + amount)
}

// ------------------------------------------------------------------- haulage

/** Up to four items moving along. Positions run from 0 at the back to 1 at the front. */
final class BeltBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val perSecond: Float)
    extends Building(world, block, x, y, rotation, team) {
  val items = new Array[Int](4)
  val at = new Array[Float](4)
  /** How far off the middle an item still is, from where it came in. */
  val side = new Array[Float](4)
  var count = 0
  /** Whether goods move, for the view. */
  var moving = 0f
  private var next: Building = null
  private var nextBelt: BeltBuilding = null
  private var aligned = false
  // The shape: which of back, left and right bring goods in.
  var fromBack = false
  var fromLeft = false
  var fromRight = false

  override def onNeighbours(): Unit = {
    next = ahead(rotation)
    if(next != null && next.team != team) next = null
    nextBelt = next match { case b: BeltBuilding => b; case _ => null }
    aligned = nextBelt != null && nextBelt.rotation == rotation
    def feeds(direction: Int): Boolean = ahead(direction) match {
      case null => false
      case b if b.team != team => false
      case b: BeltBuilding => b.ahead(b.rotation) eq this
      case b => !covered && Valley.offloads(b)
    }
    fromBack = feeds(Dir.opposite(rotation))
    fromLeft = feeds((rotation + 3) & 3)
    fromRight = feeds((rotation + 1) & 3)
  }
  def turned(newRotation: Int): Unit = rotation = newRotation

  override def update(dt: Float): Unit = {
    if(count == 0) { moving = scala.math.max(0f, moving - dt * 4f); return }
    val step = perSecond * 0.25f * dt
    var limit = 1f
    if(nextBelt != null && aligned && nextBelt.count > 0) {
      // Keep the gap with the last item of the belt ahead.
      limit = scala.math.min(1f, 1f + nextBelt.at(nextBelt.count - 1) - BeltBuilding.Gap)
    }
    var moved = false
    var i = 0
    while(i < count) {
      val target = scala.math.min(limit, at(i) + step)
      if(target > at(i) + 0.0001f) { at(i) = target; moved = true }
      if(side(i) != 0f) {
        val s = step * 1.2f
        side(i) = if(side(i) > s) side(i) - s else if(side(i) < -s) side(i) + s else 0f
      }
      limit = at(i) - BeltBuilding.Gap
      i += 1
    }
    if(at(0) >= 1f && next != null) {
      val item = Items.all(items(0))
      if(next.alive && next.accepts(item, this)) {
        removeFirst()
        next.receive(item, this)
        moved = true
      }
    }
    moving = if(moved) scala.math.min(1f, moving + dt * 6f) else scala.math.max(0f, moving - dt * 6f)
  }

  private def removeFirst(): Unit = {
    var i = 1
    while(i < count) {
      items(i - 1) = items(i); at(i - 1) = at(i); side(i - 1) = side(i)
      i += 1
    }
    count -= 1
  }

  // Where an item coming from that building enters: 0 from the back, 1 from
  // the left side, 2 from the right, -1 from the front.
  private def entry(from: Building): Int = {
    val s = sideOf(from)
    if(s == rotation) -1
    else if(s == Dir.opposite(rotation)) 0
    else if(s == ((rotation + 3) & 3)) 1
    else 2
  }

  private val covered = block eq Blocks.CoveredBelt
  override def accepts(item: Item, from: Building): Boolean = {
    if(count >= 4) return false
    if(covered && !from.isInstanceOf[BeltBuilding]) return false
    entry(from) match {
      case -1 => false
      case 0 => count == 0 || at(count - 1) >= BeltBuilding.Gap
      case _ =>
        var i = 0
        while(i < count) {
          if(scala.math.abs(at(i) - 0.5f) < BeltBuilding.Gap * 0.9f) return false
          i += 1
        }
        true
    }
  }

  override def receive(item: Item, from: Building): Unit = {
    val e = entry(from)
    val position = if(e == 0) 0f else 0.5f
    var index = count
    while(index > 0 && at(index - 1) < position) index -= 1
    var i = count
    while(i > index) {
      items(i) = items(i - 1); at(i) = at(i - 1); side(i) = side(i - 1)
      i -= 1
    }
    items(index) = item.id
    at(index) = position
    side(index) = if(e == 1) -0.5f else if(e == 2) 0.5f else 0f
    count += 1
  }

  def clear(): Unit = count = 0

  override def write(out: Writer): Unit = {
    out.int(count)
    var i = 0
    while(i < count) { out.int(items(i)); out.float(at(i)); out.float(side(i)); i += 1 }
  }
  override def read(in: Reader): Unit = {
    count = scala.math.min(4, in.int())
    var i = 0
    while(i < count) { items(i) = in.int(); at(i) = in.float(); side(i) = in.float(); i += 1 }
  }
}
object BeltBuilding {
  val Gap = 0.25f
}

/** Four lines in one tile, one for each direction of travel. */
final class CrossingBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  private val Capacity = 4
  private val Seconds = 0.22f
  private val items = Array.fill(4)(new Array[Int](Capacity))
  private val waits = Array.fill(4)(new Array[Float](Capacity))
  private val counts = new Array[Int](4)

  override def update(dt: Float): Unit = {
    var d = 0
    while(d < 4) {
      val n = counts(d)
      var i = 0
      while(i < n) { waits(d)(i) -= dt; i += 1 }
      if(n > 0 && waits(d)(0) <= 0f) {
        val to = ahead(d)
        val item = Items.all(items(d)(0))
        if(to != null && to.team == team && to.accepts(item, this)) {
          to.receive(item, this)
          i = 1
          while(i < n) { items(d)(i - 1) = items(d)(i); waits(d)(i - 1) = waits(d)(i); i += 1 }
          counts(d) = n - 1
        }
      }
      d += 1
    }
  }
  override def accepts(item: Item, from: Building): Boolean = {
    val d = Dir.opposite(sideOf(from))
    val to = ahead(d)
    counts(d) < Capacity && to != null && to.team == team
  }
  override def receive(item: Item, from: Building): Unit = {
    val d = Dir.opposite(sideOf(from))
    items(d)(counts(d)) = item.id
    waits(d)(counts(d)) = Seconds
    counts(d) += 1
  }
  override def write(out: Writer): Unit = {
    var d = 0
    while(d < 4) {
      out.int(counts(d))
      var i = 0
      while(i < counts(d)) { out.int(items(d)(i)); i += 1 }
      d += 1
    }
  }
  override def read(in: Reader): Unit = {
    var d = 0
    while(d < 4) {
      val n = in.int()
      counts(d) = scala.math.min(n, Capacity)
      var i = 0
      while(i < n) { val item = in.int(); if(i < Capacity) { items(d)(i) = item; waits(d)(i) = 0f }; i += 1 }
      d += 1
    }
  }
}

final class SplitterBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  var held = -1
  private var from: Building = null
  private var delay = 0f
  private var turn = 0

  override def update(dt: Float): Unit = if(held >= 0) {
    delay -= dt
    if(delay <= 0f) {
      val around = neighbours
      var i = 0
      var done = false
      while(i < around.length && !done) {
        val b = around((turn + i) % around.length)
        // Back where it came from is no way to go, unless it came off a belt that now leads away.
        val back = (b eq from) && !b.isInstanceOf[SplitterBuilding]
        if(!back && b.team == team && b.accepts(Items.all(held), this)) {
          b.receive(Items.all(held), this)
          turn = (turn + i + 1) % around.length
          held = -1
          from = null
          done = true
        }
        i += 1
      }
    }
  }
  override def accepts(item: Item, source: Building): Boolean = held < 0
  override def receive(item: Item, source: Building): Unit = {
    held = item.id
    from = source
    delay = 0.11f
  }
  override def write(out: Writer): Unit = out.int(held)
  override def read(in: Reader): Unit = held = in.int()
}

/** Passes goods on at once: the chosen item ahead, or all but it. */
final class SorterBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, inverted: Boolean)
    extends Building(world, block, x, y, rotation, team) {
  private var chosen = -1
  private var turn = 0
  override def config: Int = chosen
  override def configure(value: Int): Unit = chosen = if(value >= 0 && value < Items.Count) value else -1

  private def target(item: Item, from: Building, commit: Boolean): Building = {
    val d = Dir.opposite(sideOf(from))
    def open(direction: Int): Building = {
      val b = ahead(direction)
      if(b == null || b.team != team || b.isInstanceOf[SorterBuilding] || b.isInstanceOf[SpillwayBuilding] || !b.accepts(item, this)) null else b
    }
    if((item.id == chosen) != inverted) open(d)
    else {
      val left = open((d + 3) & 3)
      val right = open((d + 1) & 3)
      if(left != null && right != null) {
        val chosenSide = if(turn == 0) left else right
        if(commit) turn = 1 - turn
        chosenSide
      } else if(left != null) left else right
    }
  }
  override def accepts(item: Item, from: Building): Boolean = target(item, from, commit = false) != null
  override def receive(item: Item, from: Building): Unit = {
    val to = target(item, from, commit = true)
    if(to != null) to.receive(item, this)
  }
  override def write(out: Writer): Unit = out.int(chosen)
  override def read(in: Reader): Unit = chosen = in.int()
}

final class SpillwayBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, inverted: Boolean)
    extends Building(world, block, x, y, rotation, team) {
  private var turn = 0
  private def target(item: Item, from: Building, commit: Boolean): Building = {
    val d = Dir.opposite(sideOf(from))
    def open(direction: Int): Building = {
      val b = ahead(direction)
      if(b == null || b.team != team || b.isInstanceOf[SorterBuilding] || b.isInstanceOf[SpillwayBuilding] || !b.accepts(item, this)) null else b
    }
    def sides: Building = {
      val left = open((d + 3) & 3)
      val right = open((d + 1) & 3)
      if(left != null && right != null) {
        val side = if(turn == 0) left else right
        if(commit) turn = 1 - turn
        side
      } else if(left != null) left else right
    }
    if(inverted) {
      val side = sides
      if(side != null) side else open(d)
    } else {
      val front = open(d)
      if(front != null) front else sides
    }
  }
  override def accepts(item: Item, from: Building): Boolean = target(item, from, commit = false) != null
  override def receive(item: Item, from: Building): Unit = {
    val to = target(item, from, commit = true)
    if(to != null) to.receive(item, this)
  }
}

final class ZiplineBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val reach: Int)
    extends Building(world, block, x, y, rotation, team) {
  // Where it leads, as a tile index; -1 when it is an end.
  private var target = -1
  private val Capacity = 8
  private val items = mutable.ArrayDeque.empty[Int]
  private var delay = 0f
  private var turn = 0
  val incoming = mutable.ArrayBuffer.empty[ZiplineBuilding]

  override def config: Int = target
  override def configure(value: Int): Unit = {
    val old = destination
    if(old != null) old.incoming -= this
    target = value
    if(destination == null && value >= 0) {
      val tx = value % world.width
      val ty = value / world.width
      if(!ZiplineBuilding.inReach(x, y, tx, ty, reach)) target = -1
    }
    if(destination != null && (destination.destination eq this)) destination.configure(-1)
    if(destination != null && !destination.incoming.contains(this)) destination.incoming += this
  }
  def destination: ZiplineBuilding = if(target < 0) null else world.buildingAt(target % world.width, target / world.width) match {
    case z: ZiplineBuilding if z.team == team && ZiplineBuilding.inReach(x, y, z.x, z.y, reach) => z
    case _ => null
  }
  override def onNeighbours(): Unit = {
    val d = destination
    if(d != null && !d.incoming.contains(this)) d.incoming += this
  }
  override def onRemoved(): Unit = {
    val d = destination
    if(d != null) d.incoming -= this
  }

  override def update(dt: Float): Unit = {
    var i = 0
    while(i < incoming.size) {
      val z = incoming(i)
      if(!z.alive || !(z.destination eq this)) { val _ = incoming.remove(i) } else i += 1
    }
    if(items.nonEmpty) {
      delay -= dt
      if(delay <= 0f) {
        val to = destination
        val item = Items.all(items.head)
        if(to != null) {
          // It may have been built after this one.
          if(!to.incoming.contains(this)) to.incoming += this
          if(to.items.size < Capacity) {
            val _ = items.removeHead()
            to.items.append(item.id)
            to.delay = scala.math.max(to.delay, 0.05f)
            delay = 0.1f
            spin += 1f
          }
        } else if(unload(item)) {
          val _ = items.removeHead()
          delay = 0.1f
        }
      }
    }
  }
  private def unload(item: Item): Boolean = {
    var i = 0
    while(i < 4) {
      val d = (turn + i) & 3
      val b = ahead(d)
      // Never back towards where the line comes from.
      val blocked = incoming.exists(z => ZiplineBuilding.towards(x, y, z.x, z.y) == d)
      if(!blocked && b != null && b.team == team && b.accepts(item, this)) {
        b.receive(item, this)
        turn = (d + 1) & 3
        return true
      }
      i += 1
    }
    false
  }
  override def accepts(item: Item, from: Building): Boolean =
    items.size < Capacity && destination != null && !(from eq destination) && {
      val d = destination
      sideOf(from) != ZiplineBuilding.towards(x, y, d.x, d.y) || !from.isInstanceOf[BeltBuilding]
    }
  override def receive(item: Item, from: Building): Unit = items.append(item.id)
  override def write(out: Writer): Unit = {
    out.int(target)
    out.int(items.size)
    items.foreach(out.int)
  }
  override def read(in: Reader): Unit = {
    target = in.int()
    val n = in.int()
    for(_ <- 0 until n) items.append(in.int())
  }
}
object ZiplineBuilding {
  def inReach(x0: Int, y0: Int, x1: Int, y1: Int, reach: Int): Boolean =
    (x0 == x1 && y0 != y1 && scala.math.abs(y0 - y1) <= reach) || (y0 == y1 && x0 != x1 && scala.math.abs(x0 - x1) <= reach)
  def towards(x0: Int, y0: Int, x1: Int, y1: Int): Int =
    if(x1 > x0) 0 else if(y1 > y0) 1 else if(x1 < x0) 2 else 3
}

final class StoreBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, capacity: Int)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock(capacity)
  override def accepts(item: Item, from: Building): Boolean = stock(item) < capacity
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def canGive(item: Item): Boolean = stock.has(item)
  override def give(item: Item): Unit = stock.remove(item, 1)
  override def write(out: Writer): Unit = out.stock(stock)
  override def read(in: Reader): Unit = in.stock(stock)
}

final class HatchBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, perSecond: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var chosen = -1
  private var delay = 0f
  private var next = 0
  override def config: Int = chosen
  override def configure(value: Int): Unit = chosen = if(value >= 0 && value < Items.Count) value else -1
  override def update(dt: Float): Unit = {
    delay -= dt
    if(delay <= 0f) {
      val around = neighbours
      var i = 0
      var done = false
      while(i < around.length && !done) {
        val source = around(i)
        if(source.team == team && (source.isInstanceOf[StoreBuilding] || source.isInstanceOf[HearthBuilding])) {
          if(chosen >= 0) done = move(source, Items.all(chosen), around)
          else {
            var k = 0
            while(k < Items.Count && !done) {
              val item = Items.all((next + k) % Items.Count)
              if(move(source, item, around)) { done = true; next = (next + k + 1) % Items.Count }
              k += 1
            }
          }
        }
        i += 1
      }
      delay = if(done) 1f / perSecond else 0.1f
      ease(if(done) 1f else 0f, dt)
    }
  }
  private def move(source: Building, item: Item, around: Array[Building]): Boolean = {
    if(!source.canGive(item)) return false
    var i = 0
    while(i < around.length) {
      val b = around(i)
      if(!(b eq source) && b.team == team && !b.isInstanceOf[StoreBuilding] && !b.isInstanceOf[HearthBuilding] && b.accepts(item, this)) {
        source.give(item)
        b.receive(item, this)
        return true
      }
      i += 1
    }
    false
  }
  override def write(out: Writer): Unit = out.int(chosen)
  override def read(in: Reader): Unit = chosen = in.int()
}

final class PlainBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team)

/** Pipes and cisterns. */
final class ConduitBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, capacity: Float)
    extends Building(world, block, x, y, rotation, team) {
  override def fluidCapacity: Float = capacity
  override def write(out: Writer): Unit = {
    val share = if(fluid != null && fluid.capacity > 0f) fluid.stored / fluid.capacity else 0f
    out.int(if(fluid == null || fluid.fluid == null) -1 else fluid.fluid.ordinal)
    out.float(share * capacity)
  }
  override def read(in: Reader): Unit = {
    val f = in.int()
    heldFluid = if(f >= 0) Fluid.fromOrdinal(f) else null
    charge2 = in.float()
  }
}

final class CulvertBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val reach: Int)
    extends Building(world, block, x, y, rotation, team) {
  private var target = -1
  val incoming = mutable.ArrayBuffer.empty[CulvertBuilding]
  override def fluidCapacity: Float = 8f
  override def config: Int = target
  override def configure(value: Int): Unit = {
    val old = destination
    if(old != null) old.incoming -= this
    target = value
    onNeighbours()
    world.networks.fluidsChanged()
  }
  def destination: CulvertBuilding = if(target < 0) null else world.buildingAt(target % world.width, target / world.width) match {
    case c: CulvertBuilding if c.team == team && ZiplineBuilding.inReach(x, y, c.x, c.y, reach) => c
    case _ => null
  }
  override def onNeighbours(): Unit = {
    val d = destination
    if(d != null && !d.incoming.contains(this)) { d.incoming += this; world.networks.fluidsChanged() }
  }
  override def update(dt: Float): Unit = {
    // Its other end may have come after it, or have gone.
    onNeighbours()
    if(incoming.exists(c => !c.alive || !(c.destination eq this))) {
      incoming.filterInPlace(c => c.alive && (c.destination eq this))
      world.networks.fluidsChanged()
    }
  }
  override def onRemoved(): Unit = {
    val d = destination
    if(d != null) d.incoming -= this
  }
  override def write(out: Writer): Unit = out.int(target)
  override def read(in: Reader): Unit = target = in.int()
}

/** Takes anything, and gives nothing back. */
final class BurnerBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  override def update(dt: Float): Unit = activity = scala.math.max(0f, activity - dt)
  override def accepts(item: Item, from: Building): Boolean = !stopped
  override def receive(item: Item, from: Building): Unit = activity = 1f
}

/** Throws crates to another one. */
final class CatapultBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val reach: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var target = -1
  stock = new Stock
  var aim = 0f
  var recoil = 0f
  private var reload = 0f
  private var dumpWait = 0f
  /** Crates on their way: where they are going, how far they are, and what is in them. */
  final class Crate(val to: CatapultBuilding, val amounts: Array[Int], val seconds: Float) { var age = 0f }
  val crates = mutable.ArrayBuffer.empty[Crate]
  private val Capacity = 120
  override def config: Int = target
  override def configure(value: Int): Unit = target = value
  def inReach(o: CatapultBuilding): Boolean = !(o eq this) && dist2(cx, cy, o.cx, o.cy) <= reach * reach * Valley.Tile * Valley.Tile
  def destination: CatapultBuilding = if(target < 0) null else world.buildingAt(target % world.width, target / world.width) match {
    case c: CatapultBuilding if c.team == team && inReach(c) => c
    case _ => null
  }
  override def wantsToWork: Boolean = destination != null && stock.total >= 10
  override def update(dt: Float): Unit = {
    recoil = scala.math.max(0f, recoil - dt * 2f)
    val to = destination
    if(to != null) {
      aim = turnTo(aim, atan2(to.cy - cy, to.cx - cx), dt * 3f)
      val speed = condition * powerShare
      ease(if(stock.total > 0) speed else 0f, dt)
      reload += dt * speed
      if(reload >= 2.6f && stock.total >= 10 && to.stock.total + to.arriving < to.room && speed > 0.1f) {
        reload = 0f
        recoil = 1f
        val amounts = stock.amounts.clone()
        java.util.Arrays.fill(stock.amounts, 0)
        crates += new Crate(to, amounts, dist(cx, cy, to.cx, to.cy) / 420f)
        world.sound(SoundKind.Lob, cx, cy)
      }
    } else {
      dumpWait -= dt
      val i = stock.first
      if(i >= 0 && dumpWait <= 0f) {
        dumpWait = 0.06f
        // Whichever goods have a way out.
        var k = 0
        while(k < Items.Count) {
          val item = Items.all((i + k) % Items.Count)
          if(stock.has(item) && dump(item)) { stock.remove(item, 1); k = Items.Count } else k += 1
        }
      }
    }
    var i = 0
    while(i < crates.size) {
      val c = crates(i)
      c.age += dt
      if(c.age >= c.seconds) {
        if(c.to.alive) {
          c.amounts.indices.foreach(k => c.to.stock.amounts(k) += c.amounts(k))
          world.fx(FxKind.Puff, c.to.cx, c.to.cy, 0f, 0)
        }
        val _ = crates.remove(i)
      } else i += 1
    }
  }
  private def room: Int = Capacity
  private def arriving: Int = 0
  override def accepts(item: Item, from: Building): Boolean = destination != null && stock.total < Capacity
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.int(target); out.stock(stock); out.float(aim) }
  override def read(in: Reader): Unit = { target = in.int(); in.stock(stock); aim = in.float() }
}

// ----------------------------------------------------------------- gathering

final class DrillBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, base: Float)
    extends Building(world, block, x, y, rotation, team) {
  var ore: Item = null
  var oreCount = 0
  private var progress = 0f
  private var held = 0
  private var dumpWait = 0f
  override def onPlaced(): Unit = {
    val found = DrillBuilding.survey(world, block, x, y)
    ore = found._1
    oreCount = found._2
  }
  override def wantsToWork: Boolean = ore != null && held < 10
  override def update(dt: Float): Unit = {
    if(ore == null) return
    val speed = if(held >= 10) 0f else condition * powerShare * (if(block.waterUse > 0f && waterShare > 0f) 1f + waterShare else 1f)
    ease(speed, dt)
    progress += dt * speed
    val seconds = base / oreCount
    if(progress >= seconds) {
      progress -= seconds
      held += 1
      world.gathered(ore, 1)
    }
    dumpWait -= dt
    if(held > 0 && dumpWait <= 0f) {
      dumpWait = 0.08f
      if(dump(ore)) held -= 1
    }
  }
  def perSecond: Float = if(ore == null) 0f else oreCount / base
  override def write(out: Writer): Unit = { out.float(progress); out.int(held) }
  override def read(in: Reader): Unit = { progress = in.float(); held = in.int() }
}
object DrillBuilding {
  /** The ore a drill would dig there, and on how many of its tiles. */
  def survey(world: Valley, block: Block, x: Int, y: Int): (Item, Int) = {
    val counts = new Array[Int](Items.Count)
    var ty = y
    while(ty < y + block.size) {
      var tx = x
      while(tx < x + block.size) {
        val ore = world.oreAt(tx, ty)
        if(ore >= 0) counts(ore) += 1
        tx += 1
      }
      ty += 1
    }
    var best = -1
    var i = 0
    while(i < counts.length) {
      if(counts(i) > 0 && (best < 0 || counts(i) > counts(best))) best = i
      i += 1
    }
    if(best < 0) (null, 0) else (Items.all(best), counts(best))
  }
}

final class TapBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var progress = 0f
  private var held = 0
  var trees = 0
  override def onPlaced(): Unit = trees = TapBuilding.trees(world, x, y)
  override def update(dt: Float): Unit = {
    val speed = if(held >= 6) 0f else trees * world.season.sap * condition * (if(world.season == Season.Winter) 0f else 1f)
    ease(scala.math.min(1f, speed), dt)
    progress += dt * speed
    if(progress >= seconds) {
      progress -= seconds
      held += 1
      world.gathered(Items.Resin, 1)
    }
    if(held > 0 && dump(Items.Resin)) held -= 1
  }
  override def write(out: Writer): Unit = { out.float(progress); out.int(held) }
  override def read(in: Reader): Unit = { progress = in.float(); held = in.int() }
}
object TapBuilding {
  def trees(world: Valley, x: Int, y: Int): Int = {
    var n = 0
    var d = 0
    while(d < 4) {
      if(world.solidAt(x + Dir.dx(d), y + Dir.dy(d)) == Solid.Thicket) n += 1
      d += 1
    }
    n
  }
}

final class PumpBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, rate: Float)
    extends Building(world, block, x, y, rotation, team) {
  var output = 0f
  override def fluidMade: Float = output
  override def fluidCapacity: Float = 10f
  override def update(dt: Float): Unit = {
    val frozen = world.season == Season.Winter && !warm
    val dry = world.season == Season.Summer && world.floorAt(x, y) == Floor.Shallow
    output = if(frozen || dry || stopped) 0f else rate
    ease(output / rate, dt)
  }
}

// ------------------------------------------------------------------ crafting

final class CrafterBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val recipe: Role.Crafter)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  var progress = 0f
  private var dumpWait = 0f
  private val limit = recipe.in.map(s => s.item.id -> scala.math.max(8, s.amount * 4)).toMap
  private def full: Boolean = stock(recipe.out.item) >= 12
  private def supplied: Boolean = {
    var i = 0
    while(i < recipe.in.size) {
      if(!stock.has(recipe.in(i).item, recipe.in(i).amount)) return false
      i += 1
    }
    true
  }
  override def wantsToWork: Boolean = !full && supplied
  override def update(dt: Float): Unit = {
    val speed = if(wantsToWork) condition * powerShare * waterShare else 0f
    ease(speed, dt)
    progress += dt * speed
    if(progress >= recipe.seconds) {
      progress = 0f
      recipe.in.foreach(s => stock.remove(s.item, s.amount))
      stock.add(recipe.out.item, recipe.out.amount)
      world.crafted(recipe.out.item, recipe.out.amount)
      if(world.rng.chance(0.5f)) world.fx(FxKind.Puff, cx, cy - size * 6f, 0f, 0)
    }
    dumpWait -= dt
    if(dumpWait <= 0f && stock.has(recipe.out.item)) {
      dumpWait = 0.08f
      if(dump(recipe.out.item)) stock.remove(recipe.out.item, 1)
    }
  }
  override def accepts(item: Item, from: Building): Boolean = limit.get(item.id) match {
    case Some(n) => stock(item) < n
    case None => false
  }
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.stock(stock); out.float(progress) }
  override def read(in: Reader): Unit = { in.stock(stock); progress = in.float() }
}

// --------------------------------------------------------------------- power

final class GeneratorBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val role: Role.Generator)
    extends Building(world, block, x, y, rotation, team) {
  var output = 0f
  var fuel = 0f
  private var fuelSeconds = 1f
  if(role.source == PowerSource.Fire) stock = new Stock
  override def isPowered: Boolean = true
  override def powerOutput: Float = output
  override def update(dt: Float): Unit = {
    output = role.source match {
      case PowerSource.Wind => role.output * world.windNow * (0.85f + 0.15f * sin(world.time * 0.7f + x))
      case PowerSource.Stream => if(world.season == Season.Winter && warm) role.output * 0.6f else role.output * world.season.stream * world.sky.stream
      case PowerSource.Sun => role.output * world.sunNow * (if(world.season == Season.Winter && !warm) 0.6f else 1f)
      case PowerSource.Fire =>
        if(fuel <= 0f) {
          val i = stock.first
          if(i >= 0) {
            stock.remove(Items.all(i), 1)
            fuelSeconds = role.seconds * Items.all(i).burn
            fuel = fuelSeconds
          }
        }
        if(fuel > 0f) { fuel -= dt; role.output } else 0f
    }
    if((flooded && role.source != PowerSource.Stream) || stopped) output = 0f
    ease(output / role.output, dt)
  }
  override def accepts(item: Item, from: Building): Boolean = stock != null && item.burn > 0f && stock.total < 8
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { if(stock != null) out.stock(stock); out.float(fuel) }
  override def read(in: Reader): Unit = { if(stock != null) in.stock(stock); fuel = in.float() }
}

final class PylonBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val reach: Float, val maxLinks: Int)
    extends Building(world, block, x, y, rotation, team) {
  override def isPowered: Boolean = true
  def inReach(b: Building): Boolean = {
    // From edge to edge, so that large buildings are not out of reach.
    val dx = scala.math.max(0f, scala.math.abs(cx - b.cx) - (b.size + size) * Valley.Tile / 2f + Valley.Tile)
    val dy = scala.math.max(0f, scala.math.abs(cy - b.cy) - (b.size + size) * Valley.Tile / 2f + Valley.Tile)
    val far = b match {
      case p: PylonBuilding => scala.math.max(reach, p.reach)
      case _ => reach
    }
    dx * dx + dy * dy <= far * far * Valley.Tile * Valley.Tile
  }
  def canLink(b: Building): Boolean =
    !(b eq this) && b.alive && b.team == team && b.isPowered && inReach(b) && links.size < maxLinks && (b match {
      case p: PylonBuilding => p.links.size < p.maxLinks
      case _ => true
    })
  def toggle(b: Building): Boolean =
    if(links.contains(b)) { world.unlink(this, b); true }
    else if(canLink(b)) { world.link(this, b); true }
    else false
  /** Finds what to power when it is placed: nothing that the same grid already reaches. */
  override def onPlaced(): Unit = if(!world.loading) {
    world.networks.update(0f)
    val near = world.buildings.iterator.filter(b => canLink(b)).toVector
      .sortBy(b => dist2(cx, cy, b.cx, b.cy))
    val (pylons, rest) = near.partition(_.isInstanceOf[PylonBuilding])
    val joined = mutable.HashSet.empty[PowerGraph]
    pylons.foreach { p =>
      if((p.power == null || !joined.contains(p.power)) && joined.size < 3 && canLink(p)) {
        world.link(this, p)
        if(p.power != null) joined += p.power
      }
    }
    rest.foreach { b =>
      if(canLink(b) && b.links.isEmpty) world.link(this, b)
    }
  }
}

final class FlywheelBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, capacity: Float)
    extends Building(world, block, x, y, rotation, team) {
  override def isPowered: Boolean = true
  override def powerCapacity: Float = capacity
  override def update(dt: Float): Unit = ease(if(power != null && power.capacity > 0f) power.stored / power.capacity else 0f, dt)
  override def write(out: Writer): Unit = out.float(if(power != null && power.capacity > 0f) power.stored / power.capacity * capacity else charge)
  override def read(in: Reader): Unit = charge = in.float()
}

// ------------------------------------------------------------------- warmth

final class BrazierBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val radius: Float, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  var fuel = 0f
  var lit = false
  override def update(dt: Float): Unit = {
    val cold = world.season == Season.Winter
    if(cold && fuel <= 0f) {
      val i = stock.first
      if(i >= 0) {
        stock.remove(Items.all(i), 1)
        fuel = seconds * Items.all(i).burn
      }
    }
    if(cold && fuel > 0f) fuel -= dt
    val now = cold && fuel > 0f
    if(now != lit) {
      lit = now
      world.warmthChanged()
    }
    ease(if(lit) 1f else 0f, dt)
  }
  override def onRemoved(): Unit = world.warmthChanged()
  override def accepts(item: Item, from: Building): Boolean = item.burn > 0f && stock.total < 10
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.stock(stock); out.float(fuel) }
  override def read(in: Reader): Unit = { in.stock(stock); fuel = in.float() }
}

final class RadiatorBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val radius: Float)
    extends Building(world, block, x, y, rotation, team) {
  var lit = false
  override def wantsToWork: Boolean = world.season == Season.Winter
  override def update(dt: Float): Unit = {
    val now = wantsToWork && powerShare > 0.4f && waterShare > 0.4f
    if(now != lit) {
      lit = now
      world.warmthChanged()
    }
    ease(if(lit) 1f else 0f, dt)
  }
  override def onRemoved(): Unit = world.warmthChanged()
}

// ------------------------------------------------------------------- defense

final class TurretBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val role: Role.Turret)
    extends Building(world, block, x, y, rotation, team) {
  import Valley.Tile
  var aim: Float = -Pi / 2f
  var recoil = 0f
  var reload = 0f
  /** Shots left, by position in the list of ammunition. */
  val rounds = new Array[Int](role.ammo.size)
  var loaded = -1
  var target: Creature = null
  /** A structure of the other side to shoot at when nothing else comes. */
  private var siege: Building = null
  private var search = world.rng.float() * 0.3f
  private var barrel = 0
  private var douse = 0f
  /** Length of the beam, for a lance. */
  var beam = 0f
  /** The share of the beam or bell's strength, for the view. */
  var strength = 0f

  private val PerItem = 3
  /** The fluid it is cooled with at the moment, if any. */
  def coolant: Fluid = if(!block.coolable) null else if(fluidShare(Fluid.Brine) > 0.2f) Fluid.Brine else if(fluidShare(Fluid.Water) > 0.2f) Fluid.Water else null
  override def fluidDemand(f: Fluid): Float =
    if(block.coolable) (if(f.coolant > 0f && hasTarget) 1.2f * size else 0f) else super.fluidDemand(f)
  override def wantsToWork: Boolean = hasTarget
  private def hasTarget: Boolean = target != null || siege != null
  private def targetX: Float = if(target != null) target.x else siege.cx
  private def targetY: Float = if(target != null) target.y else siege.cy
  override def accepts(item: Item, from: Building): Boolean = {
    var i = 0
    while(i < role.ammo.size) {
      if(role.ammo(i)._1 eq item) return rounds.sum + PerItem <= 40 || rounds(i) < PerItem * 3
      i += 1
    }
    false
  }
  override def receive(item: Item, from: Building): Unit = {
    val i = role.ammo.indexWhere(_._1 eq item)
    if(i >= 0) {
      rounds(i) += PerItem
      if(loaded < 0 || rounds(loaded) == 0) loaded = i else if(i > loaded) loaded = i
    }
  }
  def hasAmmo: Boolean = role.ammo.isEmpty || (loaded >= 0 && rounds(loaded) > 0)
  def ammoShare: Float = if(role.ammo.isEmpty) 1f else scala.math.min(1f, rounds.sum / 30f)

  override def update(dt: Float): Unit = {
    recoil = scala.math.max(0f, recoil - dt * 4f)
    val cool = coolant
    reload -= dt * condition * (if(cool == null) 1f else 1f + cool.coolant)
    search -= dt
    if(target != null && (!target.alive || !inRange(target))) target = null
    if(search <= 0f) {
      search = 0.25f
      val fresh = world.closestCreature(cx, cy, role.range * Tile, 1 - team, role.air, role.ground)
      if(target == null || fresh == null) target = fresh
      siege = if(target == null && role.ground && role.kind != TurretKind.Mister && role.kind != TurretKind.Bell) world.closestBuilding(cx, cy, role.range * Tile, 1 - team) else null
    }
    if(siege != null && (!siege.alive || target != null)) siege = null
    if(loaded >= 0 && rounds(loaded) == 0) loaded = rounds.lastIndexWhere(_ > 0)
    role.kind match {
      case TurretKind.Shooter | TurretKind.Lobber => shoot(dt)
      case TurretKind.Bell => ring(dt)
      case TurretKind.Lance => burn(dt)
      case TurretKind.Mister => mist(dt)
      case TurretKind.Rail => rail(dt)
      case TurretKind.Grapnel => reel(dt)
      case TurretKind.Swatter => swat(dt)
    }
  }

  private def inRange(c: Creature): Boolean = {
    val r = role.range * Tile + c.kind.radius
    dist2(cx, cy, c.x, c.y) <= r * r
  }

  private def lead(speed: Float): Float = {
    if(target == null) return atan2(siege.cy - cy, siege.cx - cx)
    val d = dist(cx, cy, target.x, target.y)
    val t = if(speed > 0f) d / speed else 0f
    atan2(target.y + target.vx2(1) * t - cy, target.x + target.vx2(0) * t - cx)
  }

  private def shoot(dt: Float): Unit = {
    ease(if(hasTarget && hasAmmo) 1f else 0f, dt)
    if(!hasTarget || !hasAmmo || condition <= 0f) return
    val shot = role.ammo(loaded)._2
    val wanted = lead(shot.speed)
    aim = turnTo(aim, wanted, dt * 7f)
    if(reload <= 0f && scala.math.abs(angleDiff(aim, wanted)) < 0.2f) {
      reload = role.reload
      recoil = 1f
      rounds(loaded) -= 1
      barrel = (barrel + 1) % role.barrels
      val off = if(role.barrels > 1) (barrel - 0.5f) * 9f else 0f
      val angle = aim + world.rng.range(-role.inaccuracy, role.inaccuracy)
      val muzzle = size * 12f
      val mx = cx + cos(aim) * muzzle - sin(aim) * off
      val my = cy + sin(aim) * muzzle + cos(aim) * off
      val d = if(shot.lob) (if(target != null) dist(mx, my, target.x + target.vx2(0) * 0.6f, target.y + target.vx2(1) * 0.6f) else dist(mx, my, siege.cx, siege.cy)) else 0f
      val f = world.shots.fire(shot, team, mx, my, angle, if(shot.lob) d else 0f, role.range * Tile * (if(shot.homing > 0f) 1.5f else 1.1f), role.air, role.ground)
      f.prey = target
      world.fx(FxKind.Muzzle, mx, my, aim, shot.color)
      world.sound(if(shot.lob) SoundKind.Lob else if(shot.homing > 0f) SoundKind.Rocket else if(shot.sprite == "shot_flame") SoundKind.Flame else if(size > 1) SoundKind.Bolt else SoundKind.Sling, mx, my)
    }
  }

  private def rail(dt: Float): Unit = {
    val share = powerShare
    ease(if(hasTarget && hasAmmo) share else 0f, dt)
    strength = scala.math.max(0f, strength - dt * 2.5f)
    if(!hasTarget || !hasAmmo || condition <= 0f || share < 0.2f) return
    val wanted = atan2(targetY - cy, targetX - cx)
    aim = turnTo(aim, wanted, dt * 2.4f)
    if(reload <= 0f && scala.math.abs(angleDiff(aim, wanted)) < 0.05f) {
      val shot = role.ammo(loaded)._2
      reload = role.reload / scala.math.max(0.3f, share)
      recoil = 1f
      strength = 1f
      rounds(loaded) -= 1
      val length = role.range * Tile
      beam = length
      world.beam(cx, cy, aim, length, shot.size * 0.5f, team, shot.damage, shot.burn)
      world.line(FxKind.Beam, cx + cos(aim) * 40f, cy + sin(aim) * 40f, cx + cos(aim) * length, cy + sin(aim) * length, 1f, shot.color)
      world.sound(SoundKind.Rail, cx, cy)
    }
  }

  private def reel(dt: Float): Unit = {
    val share = powerShare * condition
    strength = if(target != null && share > 0.2f) scala.math.min(1f, strength + dt * 4f) else scala.math.max(0f, strength - dt * 4f)
    ease(strength, dt)
    if(target != null && share > 0.2f) {
      aim = atan2(target.y - cy, target.x - cx)
      beam = dist(cx, cy, target.x, target.y)
      val pull = 260f * share / target.mass * dt
      target.push(-cos(aim) * pull * 6f, -sin(aim) * pull * 6f)
      target.hurt(22f * share * dt, pierceArmor = true)
    }
  }

  private def swat(dt: Float): Unit = {
    val share = powerShare * condition
    ease(share, dt)
    strength = scala.math.max(0f, strength - dt * 5f)
    if(reload <= 0f && share > 0.2f) {
      val f = world.shots.closest(cx, cy, role.range * Tile, 1 - team)
      if(f != null) {
        reload = role.reload / share
        strength = 1f
        aim = atan2(f.y - cy, f.x - cx)
        beam = dist(cx, cy, f.x, f.y)
        f.spent = true
        world.line(FxKind.Spark, cx, cy, f.x, f.y, 0.6f, 0xffe6a0)
        world.sound(SoundKind.Spark, cx, cy)
      }
    }
  }

  private def ring(dt: Float): Unit = {
    val share = powerShare * condition
    ease(if(target != null) share else share * 0.15f, dt)
    strength = scala.math.max(0f, strength - dt * 3f)
    if(target != null && share > 0.3f && reload <= 0f) {
      reload = role.reload / scala.math.max(0.3f, share)
      strength = 1f
      world.chain(cx, cy, target, team, 26f, 5, 100f)
      world.sound(SoundKind.Zap, cx, cy)
    }
  }

  private def burn(dt: Float): Unit = {
    val sun = scala.math.min(1.6f, world.sunNow)
    val share = powerShare * condition * sun
    if(hasTarget && share > 0.1f) {
      val wanted = atan2(targetY - cy, targetX - cx)
      aim = turnTo(aim, wanted, dt * 2.2f)
      strength = scala.math.min(share, strength + dt * 2f)
      val length = role.range * Tile
      beam = length
      world.beam(cx, cy, aim, length, 9f, team, 130f * strength * dt, 1.5f)
      reload -= dt
      if(reload <= 0f) { reload = 0.35f; world.sound(SoundKind.Beam, cx, cy) }
    } else strength = scala.math.max(0f, strength - dt * 2f)
    ease(if(hasTarget) scala.math.min(1f, share) else 0f, dt)
  }

  private def mist(dt: Float): Unit = {
    val share = waterShare * condition
    // Fires around it go out even with nobody to spray.
    if(share > 0.2f && world.fires.burning.nonEmpty) {
      douse -= dt
      if(douse <= 0f) {
        douse = 0.4f
        world.fires.burning.find(f => dist2(cx, cy, (f.x + 0.5f) * Tile, (f.y + 0.5f) * Tile) < role.range * role.range * Tile * Tile).foreach { f =>
          val fx = (f.x + 0.5f) * Tile
          val fy = (f.y + 0.5f) * Tile
          world.fires.douse(fx, fy, 30f)
          world.fx(FxKind.Mist, cx, cy, atan2(fy - cy, fx - cx), 0)
        }
      }
    }
    ease(if(target != null) share else 0f, dt)
    if(target != null && share > 0.2f) {
      aim = turnTo(aim, atan2(target.y - cy, target.x - cx), dt * 8f)
      if(reload <= 0f) {
        reload = role.reload / share
        val angle = aim + world.rng.range(-role.inaccuracy, role.inaccuracy)
        world.fx(FxKind.Mist, cx + cos(aim) * 14f, cy + sin(aim) * 14f, angle, 0)
        world.fires.douse(target.x, target.y, 40f)
        world.eachCreature(target.x, target.y, 40f, 1 - team) { c =>
          c.slow = scala.math.max(c.slow, 1.2f)
          c.burn = 0f
          if(world.season == Season.Winter) {
            c.chill += 0.13f / (1f + c.kind.hp / 300f)
            if(c.chill >= 1f) { c.chill = 0f; c.frozen = 1.6f; c.hurt(30f, pierceArmor = true) }
          } else c.hurt(1.2f, pierceArmor = true)
          val a = atan2(c.y - cy, c.x - cx)
          c.push(cos(a) * 7f / c.mass, sin(a) * 7f / c.mass)
        }
        if(world.rng.chance(0.1f)) world.sound(SoundKind.Spray, cx, cy)
      }
    }
  }

  override def write(out: Writer): Unit = { out.float(aim); rounds.foreach(out.int) }
  override def read(in: Reader): Unit = {
    aim = in.float()
    rounds.indices.foreach(i => rounds(i) = in.int())
    loaded = rounds.lastIndexWhere(_ > 0)
  }
}

final class MenderBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, radius: Float, seconds: Float, share: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var progress = 0f
  /** Runs from 1 to 0 after each mending, for the view. */
  var pulse = 0f
  override def wantsToWork: Boolean = true
  override def update(dt: Float): Unit = {
    val speed = condition * powerShare
    ease(speed, dt)
    pulse = scala.math.max(0f, pulse - dt * 1.2f)
    progress += dt * speed
    if(progress >= seconds) {
      progress = 0f
      var any = false
      val r = radius * Valley.Tile
      world.eachBuilding(cx, cy, r, team) { b =>
        if(b.hp < b.maxHp && !b.isInstanceOf[SiteBuilding]) {
          b.heal(b.maxHp * share)
          any = true
          world.fx(FxKind.Mend, b.cx, b.cy, b.size.toFloat, 0)
        }
      }
      if(any) pulse = 1f
    }
  }
}

// ---------------------------------------------------------- hearth and season

final class HearthBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock(if(size == 3) 4000 else if(size == 4) 9000 else 18000)
  val tier: Int = size - 3
  override def accepts(item: Item, from: Building): Boolean = true
  override def receive(item: Item, from: Building): Unit = {
    stock.add(item, 1)
    world.delivered(item, 1)
    spin += 0.2f
  }
  override def canGive(item: Item): Boolean = stock.has(item)
  override def give(item: Item): Unit = stock.remove(item, 1)
  override def update(dt: Float): Unit = {
    activity = 1f
    if(hitAgo > 6f && hp < maxHp) heal(dt * 12f)
  }
  override def write(out: Writer): Unit = out.stock(stock)
  override def read(in: Reader): Unit = in.stock(stock)
}

/** Holds a season back, or hurries it on, for a price. */
final class AlmanacBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  var rest = 0f
  override def wantsToWork: Boolean = true
  override def update(dt: Float): Unit = {
    ease(powerShare, dt)
    rest = scala.math.max(0f, rest - dt * powerShare * condition)
  }
  def ready: Boolean = rest <= 0f && powerShare > 0.3f
  def canAfford(action: AlmanacAction): Boolean = ready && world.canPay(action.cost)
  def perform(action: AlmanacAction): Boolean = {
    if(!canAfford(action)) return false
    world.pay(action.cost)
    rest = AlmanacBuilding.RestSeconds
    action match {
      case AlmanacAction.Hold => world.holdSeason(45f)
      case AlmanacAction.Hasten => world.hastenSeason()
    }
    world.sound(SoundKind.Season, cx, cy)
    true
  }
  override def write(out: Writer): Unit = out.float(rest)
  override def read(in: Reader): Unit = rest = in.float()
}
object AlmanacBuilding { val RestSeconds = 40f }

enum AlmanacAction(val label: String, val cost: Vector[Stack]) {
  case Hold extends AlmanacAction("Hold the season", Vector(Stack(Items.Glass, 20), Stack(Items.Gear, 10)))
  case Hasten extends AlmanacAction("Hurry it on", Vector(Stack(Items.Resin, 25), Stack(Items.Gear, 10)))
}

final class GreatWheelBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  /** What each spoke has been filledBy. */
  val filledBy: Array[Stock] = Array.fill(4)(new Stock)
  def need(spoke: Int, item: Item): Int =
    Blocks.spokes(spoke).find(_.item eq item).map(_.amount).getOrElse(0) - filledBy(spoke)(item)
  def filled(spoke: Int): Float = {
    val total = Blocks.spokes(spoke).map(_.amount).sum
    val have = Blocks.spokes(spoke).map(s => filledBy(spoke)(s.item)).sum
    have.toFloat / total
  }
  def done(spoke: Int): Boolean = Blocks.spokes(spoke).forall(s => filledBy(spoke)(s.item) >= s.amount)
  def complete: Boolean = done(0) && done(1) && done(2) && done(3)
  override def update(dt: Float): Unit = {
    val turning = (0 until 4).count(done) / 4f
    ease(turning, dt)
  }
  override def accepts(item: Item, from: Building): Boolean = need(world.season.ordinal, item) > 0
  override def receive(item: Item, from: Building): Unit = {
    val spoke = world.season.ordinal
    val before = done(spoke)
    filledBy(spoke).add(item, 1)
    if(!before && done(spoke)) world.spokeDone(this, spoke)
  }
  /** Takes what it can from the Hearth. */
  def fillFromHearth(): Int = {
    val spoke = world.season.ordinal
    var moved = 0
    Blocks.spokes(spoke).foreach { s =>
      val n = scala.math.min(need(spoke, s.item), world.hearthStock(s.item))
      if(n > 0) {
        world.pay(Vector(Stack(s.item, n)))
        val before = done(spoke)
        filledBy(spoke).add(s.item, n)
        moved += n
        if(!before && done(spoke)) world.spokeDone(this, spoke)
      }
    }
    moved
  }
  override def write(out: Writer): Unit = filledBy.foreach(out.stock)
  override def read(in: Reader): Unit = filledBy.foreach(in.stock)
}

// ------------------------------------------------------------------ automata

final class WorkshopBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val makes: Vector[UnitKind])
    extends Building(world, block, x, y, rotation, team) {
  import Valley.Tile
  stock = new Stock
  private var chosen = 0
  var progress = 0f
  def kind: UnitKind = makes(chosen)
  override def config: Int = chosen
  override def configure(value: Int): Unit = if(value >= 0 && value < makes.size && value != chosen) {
    chosen = value
    progress = 0f
  }
  def crew: Int = world.creatures.count(c => c.team == team && c.home == id)
  def maxCrew: Int = 6
  private def supplied: Boolean = kind.cost.forall(s => stock.has(s.item, s.amount))
  override def wantsToWork: Boolean = supplied && crew < maxCrew && world.crewCount < world.crewLimit
  override def update(dt: Float): Unit = {
    val speed = if(wantsToWork) condition * powerShare else 0f
    ease(speed, dt)
    progress += dt * speed / kind.seconds
    if(progress >= 1f) {
      progress = 0f
      kind.cost.foreach(s => stock.remove(s.item, s.amount))
      val (sx, sy) = world.standing(kind, cx, cy + (size / 2f + 0.6f) * Tile)
      val c = world.spawn(kind, team, sx, sy)
      c.home = id
      val (rx, ry) = world.standing(kind, cx, cy + (size / 2f + 1.5f) * Tile)
      c.rallyX = rx
      c.rallyY = ry
      world.fx(FxKind.Build, c.x, c.y, 1f, 0)
      world.sound(SoundKind.Built, cx, cy)
    }
  }
  override def accepts(item: Item, from: Building): Boolean = kind.cost.exists(s => (s.item eq item) && stock(item) < s.amount * 2)
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.int(chosen); out.float(progress); out.stock(stock) }
  override def read(in: Reader): Unit = { chosen = in.int(); progress = in.float(); in.stock(stock) }
}

// ---------------------------------------------------------------- the Bramble

final class NestBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  override def update(dt: Float): Unit = {
    ease(1f, dt)
    if(hitAgo > 12f && hp < maxHp) heal(dt * (if(world.season == Season.Spring) 24f else 6f))
  }
}

final class ThornBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, role: Role.Thorn)
    extends Building(world, block, x, y, rotation, team) {
  import Valley.Tile
  var aim = 0f
  private var reload = 0f
  private var search = world.rng.float() * 0.4f
  private var target: Creature = null
  private var targetBuilding: Building = null
  override def update(dt: Float): Unit = {
    reload -= dt
    search -= dt
    if(search <= 0f) {
      search = 0.4f
      target = world.closestCreature(cx, cy, role.range * Tile, 1 - team, air = true, ground = true)
      targetBuilding = if(target == null) world.closestBuilding(cx, cy, role.range * Tile, 1 - team) else null
    }
    val (tx, ty, has) =
      if(target != null && target.alive) (target.x, target.y, true)
      else if(targetBuilding != null && targetBuilding.alive) (targetBuilding.cx, targetBuilding.cy, true)
      else (0f, 0f, false)
    ease(if(has) 1f else 0.2f, dt)
    if(has) {
      aim = turnTo(aim, atan2(ty - cy, tx - cx), dt * 4f)
      if(reload <= 0f) {
        reload = role.reload
        val _ = world.shots.fire(role.shot, team, cx + cos(aim) * 22f, cy + sin(aim) * 22f, aim, 0f, role.range * Tile * 1.2f, air = true, ground = true)
        world.sound(SoundKind.Sting, cx, cy)
      }
    }
  }
}

// ------------------------------------------------------------------ the site

/** A building being put up or taken down. */
final class SiteBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val target: Block)
    extends Building(world, block, x, y, rotation, team) {
  var progress = 0f
  var removing = false
  var config0 = -1
  var note0 = ""
  override def note: String = note0
  override def renote(text: String): Unit = note0 = text
  /** What has been taken from the Hearth so far. */
  val spent = new Array[Int](target.cost.size)
  override def update(dt: Float): Unit = ()
  override def config: Int = config0
  override def configure(value: Int): Unit = config0 = value

  /** Advances the work; false when it is waiting for goods. */
  def work(amount: Float): Boolean = {
    if(removing) {
      val before = progress
      progress -= amount
      var i = 0
      while(i < target.cost.size) {
        val keep = scala.math.max(0, (target.cost(i).amount * progress).toInt)
        while(spent(i) > keep) { spent(i) -= 1; world.refund(target.cost(i).item, 1) }
        i += 1
      }
      if(progress <= 0f) world.finishRemoval(this)
      before > 0f
    } else {
      var reach = scala.math.min(1f, progress + amount)
      var ok = true
      if(!world.rules.free) {
        var i = 0
        while(i < target.cost.size) {
          val s = target.cost(i)
          val wanted = scala.math.ceil(s.amount * reach.toDouble).toInt
          while(spent(i) < wanted && world.hearthStock(s.item) > 0) {
            world.pay1(s.item)
            spent(i) += 1
          }
          if(spent(i) < wanted) {
            ok = false
            reach = scala.math.min(reach, scala.math.max(progress, spent(i).toFloat / s.amount))
          }
          i += 1
        }
      }
      progress = reach
      if(progress >= 1f) world.finishSite(this)
      ok
    }
  }
  override def write(out: Writer): Unit = {
    out.float(progress)
    out.int(if(removing) 1 else 0)
    out.int(config0)
    out.word(note0)
    spent.foreach(out.int)
  }
  override def read(in: Reader): Unit = {
    progress = in.float()
    removing = in.int() == 1
    config0 = in.int()
    note0 = in.word()
    spent.indices.foreach(i => spent(i) = in.int())
  }
}

// --------------------------------------------------------------- later works

/** Presses goods into a fluid. */
final class PressBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val recipe: Role.Press)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  private var left = 0f
  private var working = false
  override def isPlumbed: Boolean = true
  override def fluidKind: Fluid = recipe.out
  override def fluidMade: Float = if(working) recipe.rate * condition * powerShare else 0f
  private def supplied: Boolean = recipe.in.forall(s => stock.has(s.item, s.amount))
  private def full: Boolean = tanks.nonEmpty && tanks.forall(t => t.stored >= t.capacity - 0.5f && (t.fluid eq recipe.out))
  override def wantsToWork: Boolean = tanks.nonEmpty && !full && (left > 0f || supplied)
  override def update(dt: Float): Unit = {
    working = false
    if(wantsToWork && condition > 0f) {
      if(left <= 0f) {
        recipe.in.foreach(s => stock.remove(s.item, s.amount))
        left = recipe.seconds
      }
      working = true
      left -= dt * condition * powerShare
    }
    ease(if(working) powerShare else 0f, dt)
  }
  override def accepts(item: Item, from: Building): Boolean = recipe.in.exists(s => (s.item eq item) && stock(item) < s.amount * 8)
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = out.stock(stock)
  override def read(in: Reader): Unit = in.stock(stock)
}

/** Turns water into brine. */
final class ChillerBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, rate: Float)
    extends Building(world, block, x, y, rotation, team) {
  private def outFull: Boolean = tanks.filter(_.fluid eq Fluid.Brine).exists(t => t.stored >= t.capacity - 0.5f)
  override def isPlumbed: Boolean = true
  override def fluidKind: Fluid = Fluid.Brine
  override def powerDemand: Float = if(world.frozen) 0f else super.powerDemand
  override def powerShare: Float = if(world.frozen) 1f else super.powerShare
  override def wantsToWork: Boolean = tanks.size >= 2 && !outFull
  override def fluidMade: Float = if(wantsToWork) rate * condition * powerShare * fluidShare(Fluid.Water) else 0f
  override def update(dt: Float): Unit = ease(if(fluidMade > 0.1f) 1f else 0f, dt)
}

/** Grows goods, in season. */
final class GardenBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  var progress = 0f
  private var ready = 0
  var fertile = 0f
  override def onPlaced(): Unit = fertile = GardenBuilding.fertility(world, block, x, y)
  override def wantsToWork: Boolean = ready < 6 && world.season != Season.Winter
  override def update(dt: Float): Unit = {
    val season = world.season match {
      case Season.Spring => 1.6f
      case Season.Summer => 1.2f
      case Season.Autumn => 0.8f
      case Season.Winter => 0f
    }
    val speed = if(ready >= 6) 0f else season * fertile * condition * (1f + waterShare)
    ease(scala.math.min(1f, speed), dt)
    progress += dt * speed / seconds
    if(progress >= 1f) {
      progress = 0f
      ready += 1
      world.gathered(Items.Cloth, 1)
    }
    if(ready > 0 && dump(Items.Cloth)) ready -= 1
  }
  override def write(out: Writer): Unit = { out.float(progress); out.int(ready) }
  override def read(in: Reader): Unit = { progress = in.float(); ready = in.int() }
}
object GardenBuilding {
  /** The share of its ground that something grows on. */
  def fertility(world: Valley, block: Block, x: Int, y: Int): Float = {
    var n = 0
    for(ty <- y until y + block.size; tx <- x until x + block.size) {
      val f = world.floorAt(tx, ty)
      if(f == Floor.Meadow || f == Floor.Loam) n += 1
    }
    n.toFloat / (block.size * block.size)
  }
}

final class BoilerBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val full: Float, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  private var fuel = 0f
  var output = 0f
  override def isPowered: Boolean = true
  override def powerOutput: Float = output
  override def wantsToWork: Boolean = fuel > 0f || stock.total > 0
  override def update(dt: Float): Unit = {
    val water = waterShare
    if(fuel <= 0f && water > 0.2f && !stopped) {
      val i = stock.first
      if(i >= 0) { stock.remove(Items.all(i), 1); fuel = seconds * Items.all(i).burn }
    }
    if(fuel > 0f) { fuel -= dt; output = full * water } else output = 0f
    ease(output / full, dt)
  }
  override def accepts(item: Item, from: Building): Boolean = item.burn > 0f && (item ne Items.Powder) && stock.total < 10
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.stock(stock); out.float(fuel) }
  override def read(in: Reader): Unit = { in.stock(stock); fuel = in.float() }
}

final class VentBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val full: Float)
    extends Building(world, block, x, y, rotation, team) {
  var vents = 0
  override def isPowered: Boolean = true
  override def powerOutput: Float = if(stopped) 0f else full * vents
  override def onPlaced(): Unit = vents = world.ventsUnder(block, x, y)
  override def update(dt: Float): Unit = ease(if(stopped) 0f else 1f, dt)
}

/** Great power, and heat that has to go somewhere. */
final class ReactorBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val full: Float, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  var heat = 0f
  private var progress = 0f
  def load: Float = scala.math.min(1f, stock(Items.Ember) / 20f)
  override def isPowered: Boolean = true
  override def powerOutput: Float = if(stopped) 0f else full * load
  override def wantsToWork: Boolean = stock(Items.Ember) > 0
  override def update(dt: Float): Unit = {
    val l = if(stopped) 0f else load
    ease(l, dt)
    if(l > 0f) {
      progress += dt
      if(progress >= seconds / scala.math.max(0.2f, l)) { progress = 0f; stock.remove(Items.Ember, 1) }
    }
    heat += l * 0.11f * dt
    heat -= fluidShare(Fluid.Brine) * 0.17f * dt
    if(world.frozen) heat -= 0.02f * dt
    heat = clamp(heat - 0.008f * dt, 0f, 1.01f)
    if(heat > 0.7f && world.rng.chance(dt * 4f)) world.fx(FxKind.Puff, cx + world.rng.range(-40f, 40f), cy + world.rng.range(-40f, 40f), 0f, 0)
    if(heat >= 1f) {
      world.notices += Notice("An Ember Reactor has burst", 0xff7a5c)
      world.explode(cx, cy, 260f, 1400f, 1 - team, 8f, 0f, air = true, ground = true, 0xff8a3c)
      world.explode(cx, cy, 200f, 600f, team, 8f, 0f, air = true, ground = true, 0xff8a3c)
      if(alive) damage(1e9f)
    }
  }
  override def accepts(item: Item, from: Building): Boolean = (item eq Items.Ember) && stock(item) < 20
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.stock(stock); out.float(heat) }
  override def read(in: Reader): Unit = { in.stock(stock); heat = in.float() }
}

final class RatchetBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  var flow = 0f
  def back: Int = Dir.opposite(rotation)
  def front: Int = rotation
  /** The grid of what stands on that side. */
  def grid(side: Int): PowerGraph = ahead(side) match {
    case null => null
    case b if b.team == team => b.power
    case _ => null
  }
  override def update(dt: Float): Unit = ease(scala.math.min(1f, flow / 5f), dt)
}

/** A dome that stops shots. */
final class AegisBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val radius: Float, val full: Float)
    extends Building(world, block, x, y, rotation, team) {
  var strength: Float = full * 0.3f
  var broken = false
  /** Runs out after a hit, for the view. */
  var struck = 0f
  def open: Boolean = !broken && activity > 0.05f
  def reach: Float = radius * Valley.Tile * scala.math.min(1f, activity * 1.2f)
  override def wantsToWork: Boolean = true
  override def update(dt: Float): Unit = {
    val speed = condition * powerShare
    struck = scala.math.max(0f, struck - dt * 3f)
    strength = scala.math.min(full, strength + dt * full * (if(broken) 0.09f else 0.06f) * speed)
    if(broken && strength >= full * 0.9f) broken = false
    ease(if(broken) 0f else if(speed > 0.2f) 1f else 0f, dt)
  }
  def absorb(amount: Float, x: Float, y: Float): Unit = {
    strength -= amount
    struck = 1f
    world.fx(FxKind.Hit, x, y, atan2(y - cy, x - cx), 0x9fe8ff)
    if(strength <= 0f) {
      strength = 0f
      broken = true
      world.fx(FxKind.Ring, cx, cy, radius * 20f, 0x9fe8ff)
      world.sound(SoundKind.Ice, cx, cy)
    }
  }
  override def write(out: Writer): Unit = { out.float(strength); out.int(if(broken) 1 else 0) }
  override def read(in: Reader): Unit = { strength = in.float(); broken = in.int() == 1 }
}

/** Makes everything around it faster. */
final class MetronomeBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, radius: Float, faster: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var beat = 0f
  override def wantsToWork: Boolean = true
  override def update(dt: Float): Unit = {
    val speed = powerShare * (if(flooded || stopped) 0f else 1f)
    ease(speed, dt)
    beat -= dt
    if(beat <= 0f && speed > 0.3f) {
      beat = 0.5f
      world.eachBuilding(cx, cy, radius * Valley.Tile, team) { b =>
        if(!b.isInstanceOf[MetronomeBuilding] && !b.isInstanceOf[BeltBuilding]) {
          b.hurriedUntil = world.time + 0.7f
          b.hurry = 1f + (faster - 1f) * speed
        }
      }
    }
  }
}

final class CaltropBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, sharp: Float)
    extends Building(world, block, x, y, rotation, team) {
  private var rest = 0f
  override def update(dt: Float): Unit = {
    rest -= dt
    if(rest <= 0f) {
      rest = 0.4f
      var n = 0
      world.eachCreature(cx, cy, 16f, 1 - team) { c =>
        if(!c.kind.flies) {
          c.hurt(sharp, pierceArmor = true)
          c.slow = scala.math.max(c.slow, 0.8f)
          n += 1
        }
      }
      if(n > 0) damage(6f * n)
    }
  }
}

/** Stops what it touches, while it is off. */
final class SwitchBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  var on = true
  override def config: Int = if(on) 1 else 0
  override def configure(value: Int): Unit = on = value != 0
  override def update(dt: Float): Unit = {
    activity = if(on) 1f else 0f
    if(!on) neighbours.foreach(b => if(b.team == team) b.stoppedUntil = world.time + 0.1f)
  }
  override def write(out: Writer): Unit = out.int(config)
  override def read(in: Reader): Unit = configure(in.int())
}

/** Lets what it touches work only at times. */
final class GovernorBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  // The kind of watch, an item, a season, and an amount in hundreds.
  private var value = GovernorBuilding.encode(Watch.Below, 0, 0, 5)
  private var look = 0f
  var lets = true
  override def config: Int = value
  override def configure(v: Int): Unit = if(v >= 0) { value = v; look = 0f }
  def watch: Watch = Watch.fromOrdinal(clamp(value % 10, 0, Watch.values.length - 1))
  def item: Item = Items.all(clamp(value / 10 % 100, 0, Items.Count - 1))
  def season: Season = Season.fromOrdinal(clamp(value / 1000 % 10, 0, 3))
  def amount: Int = value / 10000 * 100
  def choose(watch: Watch = watch, item: Item = item, season: Season = season, amount: Int = amount): Unit =
    configure(GovernorBuilding.encode(watch, item.id, season.ordinal, clamp(amount / 100, 0, 90)))
  override def update(dt: Float): Unit = {
    look -= dt
    if(look <= 0f) {
      look = 0.5f
      lets = watch match {
        case Watch.InSeason => world.season == season
        case Watch.Below => world.hearthStock(item) < amount
        case Watch.Above => world.hearthStock(item) > amount
        case Watch.Foes => world.closestCreature(cx, cy, 20f * Valley.Tile, 1 - team, air = true, ground = true) != null
        case Watch.Calm => world.closestCreature(cx, cy, 20f * Valley.Tile, 1 - team, air = true, ground = true) == null
        case Watch.Surplus => neighbours.exists(b => b.power != null && b.power.capacity > 0f && b.power.stored > b.power.capacity * 0.5f)
      }
    }
    activity = if(lets) 1f else 0f
    if(!lets) neighbours.foreach(b => if(b.team == team && !b.isInstanceOf[GovernorBuilding]) b.stoppedUntil = world.time + 0.1f)
  }
  override def write(out: Writer): Unit = out.int(value)
  override def read(in: Reader): Unit = value = in.int()
}
object GovernorBuilding {
  def encode(watch: Watch, item: Int, season: Int, hundreds: Int): Int = watch.ordinal + item * 10 + season * 1000 + hundreds * 10000
}

/** Makes automata into better ones. They come to it by themselves once sent near. */
final class RefitBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, val role: Role.Refit)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  var inside: UnitKind = null
  var becomes: UnitKind = null
  var progress = 0f
  private def supplied: Boolean = role.cost.forall(s => stock.has(s.item, s.amount))
  override def wantsToWork: Boolean = inside != null
  override def update(dt: Float): Unit = {
    if(inside == null) {
      ease(0f, dt)
      if(supplied && !stopped) {
        val guest = world.closestWhere(cx, cy, (size / 2f + 3f) * Valley.Tile, team)(c => role.steps.exists(_._1 eq c.kind))
        if(guest != null && !guest.controlled) {
          inside = guest.kind
          becomes = role.steps.find(_._1 eq guest.kind).get._2
          role.cost.foreach(s => stock.remove(s.item, s.amount))
          guest.alive = false
          world.fx(FxKind.Build, guest.x, guest.y, 1f, 0)
          world.sound(SoundKind.Place, cx, cy)
        }
      }
    } else {
      val speed = condition * powerShare
      ease(speed, dt)
      progress += dt * speed / role.seconds
      if(progress >= 1f) {
        progress = 0f
        val (sx, sy) = world.standing(becomes, cx, cy + (size / 2f + 0.8f) * Valley.Tile)
        val c = world.spawn(becomes, team, sx, sy)
        val (rx, ry) = world.standing(becomes, c.x, cy + (size / 2f + 4f) * Valley.Tile)
        c.rallyX = rx
        c.rallyY = ry
        world.notices += Notice(s"A ${becomes.label} is ready", 0xa8e0c0)
        world.fx(FxKind.Build, c.x, c.y, 2f, 0)
        world.sound(SoundKind.Built, cx, cy)
        inside = null
        becomes = null
      }
    }
  }
  override def accepts(item: Item, from: Building): Boolean = role.cost.exists(s => (s.item eq item) && stock(item) < s.amount * 2)
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = {
    out.stock(stock); out.float(progress)
    out.int(if(inside == null) -1 else inside.id); out.int(if(becomes == null) -1 else becomes.id)
  }
  override def read(in: Reader): Unit = {
    in.stock(stock); progress = in.float()
    val a = in.int()
    val b = in.int()
    if(a >= 0 && b >= 0) { inside = UnitKinds.all(a); becomes = UnitKinds.all(b) }
  }
}

/** Sends goods out of the valley. */
final class WagonBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int)
    extends Building(world, block, x, y, rotation, team) {
  stock = new Stock
  var progress = 0f
  val PerWagon = 100
  override def wantsToWork: Boolean = stock.total >= PerWagon
  override def update(dt: Float): Unit = {
    ease(if(stock.total > 0) 1f else 0f, dt)
    if(stock.total >= PerWagon) progress += dt / 12f * condition
    if(progress >= 1f) {
      progress = 0f
      world.wagonsSent += stock.total
      stock.amounts.indices.foreach { i => world.sentOut(i) += stock.amounts(i); stock.amounts(i) = 0 }
      world.notices += Notice("A wagon has left", 0xe8c66a)
      world.fx(FxKind.Puff, cx, cy, 0f, 0)
      world.sound(SoundKind.Horn, cx, cy)
    }
  }
  override def accepts(item: Item, from: Building): Boolean = stock.total < PerWagon + 20
  override def receive(item: Item, from: Building): Unit = stock.add(item, 1)
  override def write(out: Writer): Unit = { out.stock(stock); out.float(progress) }
  override def read(in: Reader): Unit = { in.stock(stock); progress = in.float() }
}
