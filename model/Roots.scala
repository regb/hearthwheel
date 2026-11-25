package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** The Bramble underfoot. It creeps out of Briar Hearts and pods, gnaws at what
  * is built in its way, speeds the Bramble along, and burns.
  */
final class Roots(world: Valley) {
  private val n = world.width * world.height
  /** How thick it is on each tile, from 0 to 3. */
  val thick = new Array[Byte](n)
  /** Where there is some, in no order. */
  private val tiles = mutable.ArrayBuffer.empty[Int]
  private var pause = 0f
  private var growth = 30f
  private var drifted = 0

  def at(x: Int, y: Int): Boolean = world.inside(x, y) && thick(world.tileIndex(x, y)) > 0
  def size: Int = tiles.size
  def pods: Vector[PodBuilding] = world.buildings.iterator.collect { case p: PodBuilding if p.alive => p }.toVector
  /** How far roots go from a Briar Heart, in tiles. */
  def span: Float = scala.math.min(19f, 8f + 3.5f * world.year)
  val PodSpan = 6.5f

  def sow(x: Int, y: Int): Unit = if(bare(x, y)) { thick(world.tileIndex(x, y)) = 1; tiles += world.tileIndex(x, y) }
  private def bare(x: Int, y: Int): Boolean =
    world.inside(x, y) && thick(world.tileIndex(x, y)) == 0 && world.solidAt(x, y) == Solid.Open && !world.floorAt(x, y).water &&
      (world.buildingAt(x, y) match { case null => true; case b => b.team == Valley.Bramble })

  /** Takes them away around a point, as fire does. */
  def clear(x: Float, y: Float, r: Float): Unit = {
    var i = tiles.size - 1
    while(i >= 0) {
      val t = tiles(i)
      val tx = (t % world.width + 0.5f) * Valley.Tile
      val ty = (t / world.width + 0.5f) * Valley.Tile
      if(dist2(x, y, tx, ty) < r * r) drop(i)
      i -= 1
    }
  }
  private def drop(i: Int): Unit = {
    thick(tiles(i)) = 0
    tiles(i) = tiles(tiles.size - 1)
    val _ = tiles.remove(tiles.size - 1)
  }

  private def fed(x: Int, y: Int, nests: Vector[Building], pods: Vector[PodBuilding]): Boolean = {
    val px = (x + 0.5f) * Valley.Tile
    val py = (y + 0.5f) * Valley.Tile
    def near(b: Building, r: Float): Boolean = dist2(px, py, b.cx, b.cy) < r * r * Valley.Tile * Valley.Tile
    nests.exists(near(_, span)) || pods.exists(near(_, PodSpan))
  }

  def update(dt: Float): Unit = if(world.rules.wild && !world.rules.peaceful) {
    pause -= dt
    if(pause <= 0f) {
      pause = 0.25f
      val nests = world.nests
      val living = pods
      nests.foreach(b => if(world.rng.chance(0.2f)) sow(b.x - 1 + world.rng.int(b.size + 2), b.y - 1 + world.rng.int(b.size + 2)))
      living.foreach(b => if(world.rng.chance(0.2f)) sow(b.x - 1 + world.rng.int(b.size + 2), b.y - 1 + world.rng.int(b.size + 2)))
      val eager = (world.season match {
        case Season.Spring => 5f
        case Season.Summer => 2f
        case Season.Autumn => 3f
        case Season.Winter => 0f
      }) * world.rules.threat * (nests.size + living.size * 0.4f)
      var k = eager.toInt + (if(world.rng.chance(eager - eager.toInt)) 1 else 0)
      while(k > 0 && tiles.nonEmpty) { k -= 1; spread(nests, living) }
      wither(nests, living)
    }
    growth -= dt * world.rules.threat * (if(world.frozen) 0.3f else 1f)
    if(growth <= 0f) { growth = 55f; sprout() }
    if(world.season == Season.Autumn && world.year >= 2) drift() else drifted = 0
  }

  private def spread(nests: Vector[Building], living: Vector[PodBuilding]): Unit = {
    val i = world.rng.int(tiles.size)
    val t = tiles(i)
    val (x, y) = (t % world.width, t / world.width)
    if(world.rng.chance(0.3f) && thick(t) < 3) thick(t) = (thick(t) + 1).toByte
    val d = world.rng.int(4)
    val (nx, ny) = (x + Dir.dx(d), y + Dir.dy(d))
    if(world.inside(nx, ny)) {
      val b = world.buildingAt(nx, ny)
      if(b != null && b.team == Valley.Player) b.damage(14f)
      else if(fed(nx, ny, nests, living)) sow(nx, ny)
    }
  }
  // Fire eats it, winter thins it out, and it dies with what feeds it.
  private def wither(nests: Vector[Building], living: Vector[PodBuilding]): Unit = {
    var k = tiles.size / 24 + 1
    while(k > 0 && tiles.nonEmpty) {
      k -= 1
      val i = world.rng.int(tiles.size)
      val t = tiles(i)
      val (x, y) = (t % world.width, t / world.width)
      if(!fed(x, y, nests, living) || (world.frozen && world.rng.chance(0.04f))) drop(i)
    }
    var i = 0
    while(i < world.fires.burning.size) {
      val f = world.fires.burning(i)
      if(at(f.x, f.y) && f.age > 0.8f) {
        val at0 = tiles.indexOf(world.tileIndex(f.x, f.y))
        if(at0 >= 0) drop(at0)
        val d = world.rng.int(4)
        if(at(f.x + Dir.dx(d), f.y + Dir.dy(d)) && world.rng.chance(if(world.dry) 0.8f else 0.45f)) world.fires.start(f.x + Dir.dx(d), f.y + Dir.dy(d))
      }
      i += 1
    }
  }

  // A pod, some way along the roots.
  private def sprout(): Unit = {
    val living = pods
    val most = scala.math.min(9, world.nests.size * scala.math.min(3, world.year))
    if(living.size < most && tiles.nonEmpty) {
      var tries = 0
      var done = false
      while(!done && tries < 20) {
        tries += 1
        val t = tiles(world.rng.int(tiles.size))
        val (x, y) = (t % world.width, t / world.width)
        val px = (x + 1f) * Valley.Tile
        val py = (y + 1f) * Valley.Tile
        val far = world.nests.forall(b => dist(px, py, b.cx, b.cy) > 6f * Valley.Tile) && living.forall(b => dist(px, py, b.cx, b.cy) > 7f * Valley.Tile)
        if(far && plant(x, y)) done = true
      }
    }
  }
  /** Plants a pod, where there is room. */
  def plant(x: Int, y: Int): Boolean = {
    val room = (0 until 2).forall(j => (0 until 2).forall(i => world.inside(x + i, y + j) && world.solidAt(x + i, y + j) == Solid.Open && !world.floorAt(x + i, y + j).water && world.buildingAt(x + i, y + j) == null))
    if(room) {
      val b = world.place(Blocks.Pod, x, y, 0, Valley.Bramble)
      world.fx(FxKind.Puff, b.cx, b.cy, 0f, 0)
      sow(x - 1, y); sow(x + 2, y); sow(x, y - 1); sow(x, y + 2)
    }
    room
  }

  // In autumn thistledown rides the wind, to root closer to the Hearth.
  private def drift(): Unit = {
    val due = scala.math.min(3, world.year - 1)
    val h = world.hearth
    if(drifted < due && h != null && world.seasonProgress > 0.15f + 0.25f * drifted && pods.size < 12) {
      val from = world.nests
      if(from.nonEmpty) {
        drifted += 1
        val nest = from(world.rng.int(from.size))
        val c = world.spawn(UnitKinds.Thistledown, Valley.Bramble, nest.cx, nest.cy)
        // Between the two, some way out of the light.
        val a = atan2(nest.cy - h.cy, nest.cx - h.cx) + world.rng.range(-0.9f, 0.9f)
        val d = world.rng.range(20f, 27f) * Valley.Tile
        c.rallyX = clamp(h.cx + cos(a) * d, 3f * Valley.Tile, (world.width - 4f) * Valley.Tile)
        c.rallyY = clamp(h.cy + sin(a) * d, 3f * Valley.Tile, (world.height - 4f) * Valley.Tile)
        world.notices += Notice("Thistledown is on the wind. Where it lands, the Bramble takes root.", 0xd8c8f0)
      }
    }
  }

  def write(out: Writer): Unit = { out.bytes(thick); out.float(growth); out.int(drifted) }
  def read(in: Reader): Unit = {
    in.bytes(thick); growth = in.float(); drifted = in.int()
    tiles.clear()
    thick.indices.foreach(i => if(thick(i) > 0) tiles += i)
  }
}

/** Hatches creatures of the season, which keep watch around it until they march. */
final class PodBuilding(world: Valley, block: Block, x: Int, y: Int, rotation: Int, team: Int, seconds: Float)
    extends Building(world, block, x, y, rotation, team) {
  var progress = 0f
  def kind: UnitKind = world.season match {
    case Season.Spring => UnitKinds.Nettle
    case Season.Summer => if(world.year >= 2) UnitKinds.Stumper else UnitKinds.Nettle
    case Season.Autumn => if(world.year >= 2) UnitKinds.Hornwing else UnitKinds.Dandel
    case Season.Winter => UnitKinds.Frostling
  }
  def brood: Int = world.creatures.count(c => c.alive && c.home == id)
  def maxBrood: Int = scala.math.min(6, 2 + world.year)
  override def update(dt: Float): Unit = {
    ease(1f, dt)
    if(hitAgo > 10f && hp < maxHp) heal(dt * (if(world.season == Season.Spring) 10f else 4f))
    if(brood < maxBrood) progress += dt / seconds * world.rules.threat
    if(progress >= 1f) {
      progress = 0f
      val a = world.rng.float() * Tau
      val c = world.spawn(kind, team, cx + cos(a) * 50f, cy + sin(a) * 50f)
      c.home = id
      c.guarding = true
      c.rallyX = c.x
      c.rallyY = c.y
      world.fx(FxKind.Puff, c.x, c.y, 0f, 0)
    }
  }
  override def write(out: Writer): Unit = out.float(progress)
  override def read(in: Reader): Unit = progress = in.float()
}
