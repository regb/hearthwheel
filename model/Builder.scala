package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** Something the lamplighter is to build, or to take down. */
final case class Plan(block: Block, x: Int, y: Int, rotation: Int, config: Int = -1, remove: Boolean = false, note: String = "") {
  def cx: Float = (x + block.size / 2f) * Valley.Tile
  def cy: Float = (y + block.size / 2f) * Valley.Tile
  def overlaps(o: Plan): Boolean = x < o.x + o.block.size && x + block.size > o.x && y < o.y + o.block.size && y + block.size > o.y
}

/** The lamplighter's work: what is planned, and what is under way. */
final class Builder(world: Valley) {
  val plans = mutable.ArrayDeque.empty[Plan]
  /** What is being worked on right now, for the view. */
  var active: SiteBuilding = null
  var starved = false
  var paused = false
  var rebuild = true

  def add(p: Plan): Unit = {
    plans.filterInPlace(o => !o.overlaps(p) || (o.remove != p.remove))
    plans.append(p)
  }
  def cancel(x: Int, y: Int, w: Int, h: Int): Unit =
    plans.filterInPlace(p => !(p.x < x + w && p.x + p.block.size > x && p.y < y + h && p.y + p.block.size > y))
  def clear(): Unit = plans.clear()

  private def inReach(p: Plan): Boolean =
    world.lantern != null && dist2(world.lantern.x, world.lantern.y, p.cx, p.cy) <= Builder.Reach * Builder.Reach

  def update(dt: Float): Unit = {
    active = null
    starved = false
    if(world.lantern == null || world.hearth == null || paused) return
    if(plans.isEmpty && rebuild && world.ruins.nonEmpty) queueRuins()
    // A plan out of reach or starved lets the others go first.
    var tries = scala.math.min(plans.size, 24)
    var left = dt * (if(world.rules.free) 4f else 1f) * help * (1f + world.hearth.tier * 0.4f) * world.rules.buildSpeed
    while(tries > 0 && left > 0f && plans.nonEmpty) {
      val p = plans.head
      tries -= 1
      if(!inReach(p)) rotate()
      else {
        val site = siteFor(p)
        if(site == null) { val _ = plans.removeHead() }
        else {
          active = site
          val ok = site.work(left / p.block.buildSeconds)
          left = 0f
          if(!site.alive) { val _ = plans.removeHead() }
          else if(!ok) {
            starved = true
            rotate()
          }
        }
      }
    }
  }
  private def rotate(): Unit = plans.append(plans.removeHead())

  private def siteFor(p: Plan): SiteBuilding = {
    val there = world.buildingAt(p.x, p.y)
    if(p.remove) there match {
      case null => null
      case s: SiteBuilding => if(!s.removing) { val _ = world.startRemoval(s) }; s
      case b if b.team == Valley.Player && (b.block eq p.block) && !b.isInstanceOf[HearthBuilding] => world.startRemoval(b)
      case _ => null
    } else there match {
      case s: SiteBuilding if (s.target eq p.block) && s.x == p.x && s.y == p.y =>
        s.removing = false
        s
      case _ =>
        if(p.block.role eq Role.Hearth) {
          world.growHearth(p.block, p.x, p.y)
          null
        } else if(world.placementProblem(p.block, p.x, p.y, p.rotation) != null) null
        else world.startSite(p.block, p.x, p.y, p.rotation, p.config, p.note)
    }
  }
  /** How much faster it goes with masons about. */
  def help: Float = 1f + 0.5f * world.creatures.count(c => c.alive && c.team == Valley.Player && (c.kind.trade eq Trade.Builds))

  private def queueRuins(): Unit = {
    val ruin = world.ruins.remove(0)
    if(world.knows(ruin.block) && world.placementProblem(ruin.block, ruin.x, ruin.y, ruin.rotation) == null && world.canPay(ruin.block.cost))
      plans.append(Plan(ruin.block, ruin.x, ruin.y, ruin.rotation, ruin.config, note = ruin.note))
  }
}
object Builder {
  val Reach: Float = 13f * Valley.Tile

  /** A way for a belt around whatever is in between, or a plain line when there is none. */
  def path(world: Valley, block: Block, x0: Int, y0: Int, x1: Int, y1: Int): Vector[(Int, Int, Int)] = {
    val margin = 16
    val left = scala.math.max(0, scala.math.min(x0, x1) - margin)
    val top = scala.math.max(0, scala.math.min(y0, y1) - margin)
    val w = scala.math.min(world.width - 1, scala.math.max(x0, x1) + margin) - left + 1
    val h = scala.math.min(world.height - 1, scala.math.max(y0, y1) + margin) - top + 1
    if(w * h > 12000 || (x0 == x1 && y0 == y1)) return line(x0, y0, x1, y1, 0, horizontalFirst = true)
    // Cells by tile and the way one came, so that turns can cost.
    val cost = Array.fill(w * h * 4)(Int.MaxValue)
    val from = Array.fill(w * h * 4)(-1)
    val queue = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by[(Int, Int), Int](-_._1))
    def cell(x: Int, y: Int, d: Int) = ((y - top) * w + x - left) * 4 + d
    for(d <- 0 until 4) { cost(cell(x0, y0, d)) = 0; queue.enqueue((0, cell(x0, y0, d))) }
    var goal = -1
    while(queue.nonEmpty && goal < 0) {
      val (c, i) = queue.dequeue()
      if(c == cost(i)) {
        val d = i & 3
        val x = (i / 4) % w + left
        val y = (i / 4) / w + top
        if(x == x1 && y == y1) goal = i
        else for(nd <- 0 until 4) if((nd != Dir.opposite(d) || (x == x0 && y == y0)) && (nd == d || !crosses(world, x, y, d) || (x == x0 && y == y0))) {
          val nx = x + Dir.dx(nd)
          val ny = y + Dir.dy(nd)
          if(nx >= left && ny >= top && nx < left + w && ny < top + h) {
            val last = nx == x1 && ny == y1
            val over = crosses(world, nx, ny, nd)
            if(last || over || (world.buildingAt(nx, ny) == null && world.placementProblem(block, nx, ny, nd) == null)) {
              val nc = c + (if(over) 34 else 10) + (if(nd != d && !(x == x0 && y == y0)) 4 else 0)
              val ni = cell(nx, ny, nd)
              if(nc < cost(ni)) { cost(ni) = nc; from(ni) = i; queue.enqueue((nc, ni)) }
            }
          }
        }
      }
    }
    if(goal < 0) line(x0, y0, x1, y1, 0, horizontalFirst = true)
    else {
      val tiles = mutable.ArrayBuffer.empty[(Int, Int)]
      var i = goal
      while(i >= 0) { tiles += (((i / 4) % w + left, (i / 4) / w + top)); i = from(i) }
      val way = tiles.reverse.toVector.distinct
      way.indices.map { k =>
        val (a, b) = if(k + 1 < way.size) (way(k), way(k + 1)) else (way(k - 1), way(k))
        (way(k)._1, way(k)._2, ZiplineBuilding.towards(a._1, a._2, b._1, b._2))
      }.toVector
    }
  }

  /** Whether a belt can go straight over what is there, with a crossing. */
  private def crosses(world: Valley, x: Int, y: Int, d: Int): Boolean = world.knows(Blocks.Crossing) && (world.buildingAt(x, y) match {
    case b: BeltBuilding => b.team == Valley.Player && ((b.rotation ^ d) & 1) == 1
    case b: CrossingBuilding => b.team == Valley.Player
    case _ => false
  })

  /** Plans for a line of belts: where it goes straight over another line it
    * crosses, and where something is in the way it goes over on a zipline.
    */
  def belts(world: Valley, block: Block, tiles: Vector[(Int, Int, Int)]): Vector[Plan] = {
    val out = mutable.ArrayBuffer.empty[Plan]
    def straight(i: Int) = i > 0 && i < tiles.size - 1 && tiles(i - 1)._3 == tiles(i)._3 && tiles(i + 1)._3 == tiles(i)._3
    def blocked(i: Int): Boolean = {
      val (x, y, d) = tiles(i)
      world.buildingAt(x, y) match {
        case null => world.placementProblem(block, x, y, d) != null
        case b => !world.replaceable(b, block, x, y, d) && !((b.block eq block) && b.rotation == d)
      }
    }
    var i = 0
    while(i < tiles.size) {
      val (x, y, d) = tiles(i)
      val here = world.buildingAt(x, y)
      val crossing = here match {
        case b: BeltBuilding => straight(i) && ((b.rotation ^ d) & 1) == 1
        case _: CrossingBuilding => straight(i)
        case _ => false
      }
      if(crossing && world.knows(Blocks.Crossing)) {
        if(!here.isInstanceOf[CrossingBuilding]) out += Plan(Blocks.Crossing, x, y, 0)
        i += 1
      } else if(i > 0 && !blocked(i) && i + 1 < tiles.size && blocked(i + 1) && straight(i) && world.knows(Blocks.Zipline)) {
        // Over up to three tiles, straight ahead.
        var j = i + 1
        while(j < tiles.size && j - i <= 3 && blocked(j) && tiles(j)._3 == d) j += 1
        if(j < tiles.size && j - i <= 4 && !blocked(j) && tiles(j - 1)._3 == d) {
          out += Plan(Blocks.Zipline, x, y, 0, world.tileIndex(tiles(j)._1, tiles(j)._2))
          out += Plan(Blocks.Zipline, tiles(j)._1, tiles(j)._2, 0)
          i = j + 1
        } else { out += Plan(block, x, y, d); i += 1 }
      } else { out += Plan(block, x, y, d); i += 1 }
    }
    out.toVector
  }

  /** Tiles of a belt line dragged from one point to another, each with the way it faces. */
  def line(x0: Int, y0: Int, x1: Int, y1: Int, rotation: Int, horizontalFirst: Boolean): Vector[(Int, Int, Int)] = {
    val tiles = mutable.ArrayBuffer.empty[(Int, Int)]
    def walk(fromX: Int, fromY: Int, toX: Int, toY: Int): Unit = {
      var x = fromX
      var y = fromY
      val sx = Integer.signum(toX - fromX)
      val sy = Integer.signum(toY - fromY)
      while(x != toX || y != toY) {
        x += sx
        y += sy
        tiles += ((x, y))
      }
    }
    tiles += ((x0, y0))
    if(horizontalFirst) { walk(x0, y0, x1, y0); walk(x1, y0, x1, y1) }
    else { walk(x0, y0, x0, y1); walk(x0, y1, x1, y1) }
    tiles.indices.map { i =>
      val (x, y) = tiles(i)
      val d =
        if(tiles.size == 1) rotation
        else {
          val (ax, ay, bx, by) =
            if(i + 1 < tiles.size) (x, y, tiles(i + 1)._1, tiles(i + 1)._2)
            else (tiles(i - 1)._1, tiles(i - 1)._2, x, y)
          ZiplineBuilding.towards(ax, ay, bx, by)
        }
      (x, y, d)
    }.toVector
  }
}
