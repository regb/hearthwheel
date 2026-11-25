package hearthwheel

import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** A plain player: digs, spends on turrets towards the nests, feeds them from
  * the Hearth as belts would, and keeps braziers going in winter.
  */
final class Bot(w: Valley, skill: Float, expands: Boolean = false) {
  private var rest = 0f
  private var spot = 0
  var log = Vector.empty[String]

  private def free(x: Int, y: Int, size: Int): Boolean = (0 until size).forall(j => (0 until size).forall { i =>
    w.inside(x + i, y + j) && w.solidAt(x + i, y + j) == Solid.Open && !w.floorAt(x + i, y + j).water && w.buildingAt(x + i, y + j) == null && w.oreAt(x + i, y + j) < 0
  })
  // Where what walks from a heart comes close.
  private def outpost(nest: Building): (Int, Int) = {
    var x = nest.x + 1
    var y = nest.y + 4
    var n = 0
    while(n < 600 && dist(x.toFloat, y.toFloat, w.hearth.x.toFloat, w.hearth.y.toFloat) > 15f) {
      val t = w.groundField.next(clamp(x, 0, w.width - 1), clamp(y, 0, w.height - 1))
      if(t < 0) return (w.hearth.x + 1, w.hearth.y + 1)
      x = t % w.width
      y = t / w.width
      n += 1
    }
    (x, y)
  }
  private def build(b: Block): Boolean = w.canPay(b.cost) && {
    val h = w.hearth
    val nests = w.nests
    var done = false
    var tries = 0
    while(!done && tries < 40) {
      tries += 1
      spot += 1
      // Three in four towards a nest.
      val post = if(expands && b.isTurret && nests.nonEmpty && spot % 4 != 0) outpost(nests(spot % nests.size)) else null
      val a = if(post != null) w.rng.float() * Tau else if(nests.nonEmpty && spot % 4 != 0) { val n = nests(spot % nests.size); atan2(n.cy - h.cy, n.cx - h.cx) + w.rng.range(-0.7f, 0.7f) } else w.rng.float() * Tau
      val d = if(post != null) w.rng.range(1.5f, 6f) else w.rng.range(5f, 11f)
      val (ox, oy) = if(post != null) post else (h.x + 1, h.y + 1)
      val x = scala.math.round(ox + cos(a) * d)
      val y = scala.math.round(oy + sin(a) * d)
      if(free(x, y, b.size)) { w.pay(b.cost); w.place(b, x, y, 0, Valley.Player); done = true }
    }
    done
  }
  private var seams = 0
  /** A belt through a seam, drills on both sides of it, and the way to the Hearth. */
  def mineSeam(item: Item, block: Block, pays: Boolean): Boolean = {
    val h = w.hearth
    val n = block.size
    val tiles = for {
      y <- 2 until w.height - 2; x <- 2 until w.width - 2
      if w.oreAt(x, y) == item.id && dist(x.toFloat, y.toFloat, h.x.toFloat, h.y.toFloat) < 42f
    } yield (x, y)
    final case class Seam(score: Float, trunk: Vector[(Int, Int)], drills: Vector[(Int, Int)])
    def look(x: Int, y: Int, across: Boolean): Option[Seam] = {
      val trunk = (0 until 4 * n).toVector.map(i => if(across) (x + i, y) else (x, y + i)).takeWhile { case (a, b) => w.placementProblem(Blocks.Belt, a, b, 0) == null && w.buildingAt(a, b) == null }
      val spots = (0 until trunk.size / n).toVector.flatMap(k => if(across) Vector((x + k * n, y - n), (x + k * n, y + 1)) else Vector((x - n, y + k * n), (x + 1, y + k * n)))
      val drills = spots.filter { case (a, b) => w.placementProblem(block, a, b, 0) == null && w.buildingAt(a, b) == null && { val (o, c) = DrillBuilding.survey(w, block, a, b); (o eq item) && c >= n } }
      if(drills.isEmpty) None
      else Some(Seam(drills.map { case (a, b) => DrillBuilding.survey(w, block, a, b)._2 }.sum - dist(x.toFloat, y.toFloat, h.x.toFloat, h.y.toFloat) * 0.35f, trunk, drills))
    }
    val found = tiles.flatMap { case (x, y) => look(x, y, across = true) ++ look(x, y, across = false) }
    found.sortBy(-_.score).iterator.take(12).exists { seam =>
      // Flows towards the end that is closer to the Hearth.
      val ends = Vector(seam.trunk, seam.trunk.reverse).minBy { t => dist(t.last._1.toFloat, t.last._2.toFloat, h.x + 1f, h.y + 1f) }
      val rim = for(i <- 0 until h.size; p <- Vector((h.x + i, h.y - 1), (h.x + i, h.y + h.size), (h.x - 1, h.y + i), (h.x + h.size, h.y + i)) if w.buildingAt(p._1, p._2) == null) yield p
      rim.sortBy { case (x, y) => scala.math.abs(x - ends.last._1) + scala.math.abs(y - ends.last._2) }.iterator.take(5).flatMap { case (gx, gy) =>
        // Into the Hearth, from just beside it.
        val into = (0 until 4).find(d => w.buildingAt(gx + Dir.dx(d), gy + Dir.dy(d)) eq h).get
        val path = Builder.path(w, Blocks.Belt, ends.last._1, ends.last._2, gx, gy)
        val line = ends.zip(ends.tail).map { case ((x, y), (a, b)) => (x, y, (0 until 4).find(d => Dir.dx(d) == a - x && Dir.dy(d) == b - y).get) }
        val all = line ++ path.dropRight(1) :+ ((gx, gy, into))
        val plans = Builder.belts(w, Blocks.Belt, all)
        val drills = seam.drills.map { case (x, y) => Plan(block, x, y, 0) }.filterNot(d => plans.exists(_.overlaps(d)))
        val cost = (plans ++ drills).flatMap(_.block.cost).groupBy(_.item).map { case (i, s) => Stack(i, s.map(_.amount).sum) }.toVector
        val fits = path.nonEmpty && plans.forall(p => w.placementProblem(p.block, p.x, p.y, p.rotation) == null) && drills.size >= 2
        if(fits && (!pays || w.canPay(cost))) Some((plans, drills, cost)) else None
      }.take(1).toVector.headOption.exists { case (plans, drills, cost) =>
        if(pays) w.pay(cost)
        plans.foreach(p => w.place(p.block, p.x, p.y, p.rotation, Valley.Player, p.config))
        drills.foreach(p => w.place(p.block, p.x, p.y, 0, Valley.Player))
        seams += 1
        true
      }
    }
  }
  private val order = Vector(Items.Copper, Items.Flint, Items.Tin, Items.Copper, Items.Coal)
  private var next = 0
  private var mined = 0
  private var tryAt = 0f
  // As a player does who has caught up: more drills further out, better ones, menders, walls and a greater Hearth.
  private def expand(): Unit = {
    if(w.time > tryAt && seams < 16 && w.time > 45f * (seams - 2) && w.hearthStock(Items.Copper) > 110) {
      tryAt = w.time + 8f
      if(mineSeam(order(next % order.size), Blocks.Drill, pays = true)) next += 1 else if(w.hearthStock(Items.Copper) > 300) next += 1
    }
    if(w.time > tryAt && w.knows(Blocks.Auger) && mined < 3 && w.hearthStock(Items.Copper) > 200 && mineSeam(Items.Iron, Blocks.Auger, pays = true)) mined += 1
    def have(b: Block) = w.buildings.count(_.block eq b)
    if(w.knows(Blocks.Tinker) && have(Blocks.Tinker) < 10 && w.buildings.count(_.block.isTurret) > 14 + 4 * have(Blocks.Tinker)) { val _ = build(Blocks.Tinker) }
    if(w.knows(Blocks.BronzeWallLarge) && have(Blocks.BronzeWallLarge) < 40 && w.hearthStock(Items.Bronze) > 300) { val _ = build(Blocks.BronzeWallLarge) }
    if(w.knows(Blocks.GreatHearth) && w.hearth.tier == 0 && w.canPay(Blocks.GreatHearth.cost)) w.growHearth(Blocks.GreatHearth, w.hearth.x, w.hearth.y)
    if(w.knows(Blocks.Steelworks) && w.hearth.stock.has(Vector(Stack(Items.Iron, 20), Stack(Items.Coal, 60)))) {
      w.pay(Vector(Stack(Items.Iron, 4), Stack(Items.Coal, 4)))
      w.hearth.stock.add(Items.Steel, 2)
    }
    if(w.knows(Blocks.PowderMill) && w.hearth.stock.has(Vector(Stack(Items.Flint, 100), Stack(Items.Coal, 60)))) {
      w.pay(Vector(Stack(Items.Flint, 3), Stack(Items.Coal, 3)))
      w.hearth.stock.add(Items.Powder, 3)
    }
  }
  def update(dt: Float): Unit = {
    rest -= dt
    if(rest <= 0f) {
      rest = 4f / skill
      val turrets = w.buildings.count(_.block.isTurret)
      // Learns before it spends, once there is something to hold on with.
      val wanted = Vector(Blocks.Smelter, Blocks.Brazier, Blocks.Windmill, Blocks.GlassKiln, Blocks.Gearworks, Blocks.Emberpot, Blocks.Boltcaster, Blocks.Tinker, Blocks.Auger, Blocks.Steelworks, Blocks.PowderMill, Blocks.MortarRow,
        Blocks.WallLarge, Blocks.BronzeWall, Blocks.BronzeWallLarge, Blocks.GreatHearth).filterNot(w.knows).take(if(expands) 1 else if(Vector(Blocks.Smelter, Blocks.Brazier, Blocks.Emberpot, Blocks.Windmill, Blocks.GlassKiln, Blocks.Gearworks, Blocks.Boltcaster).forall(w.knows)) 0 else 1)
      Pages.all.find(p => wanted.contains(p.block) && w.canLearn(p)).foreach(w.learn)
      val saving = if(expands) turrets >= 5 + w.waves.number * 2 && w.hearthStock(Items.Copper) < 600 && (wanted.nonEmpty || seams < 14) else wanted.nonEmpty && turrets >= 8 + w.waves.number * 2
      if(w.season == Season.Autumn && w.knows(Blocks.Brazier) && w.buildings.count(_.block eq Blocks.Brazier) < 5) { val _ = build(Blocks.Brazier) }
      if(expands) {
        expand()
        // Ruins, as the lamplighter puts them up again.
        for(_ <- 0 until 6 if w.ruins.nonEmpty) {
          val r = w.ruins.remove(0)
          if(w.placementProblem(r.block, r.x, r.y, r.rotation) == null && w.canPay(r.block.cost)) { w.pay(r.block.cost); w.place(r.block, r.x, r.y, r.rotation, Valley.Player, r.config) }
        }
      }
      if(turrets < (if(expands) 160 else 60) && !saving) {
        val b = if(w.knows(Blocks.MortarRow) && turrets % 5 == 1 && w.canPay(Blocks.MortarRow.cost)) Blocks.MortarRow else if(w.knows(Blocks.Boltcaster) && turrets % 4 == 1 && (!expands || w.canPay(Blocks.Boltcaster.cost))) Blocks.Boltcaster else if(w.knows(Blocks.Emberpot) && turrets % 3 == 2 && (!expands || w.canPay(Blocks.Emberpot.cost) && (wanted.isEmpty || w.hearthStock(Items.Bronze) > 120))) Blocks.Emberpot else Blocks.Slinger
        val _ = build(b)
      }
      // Bronze comes out of the smelters there would be.
      if(w.knows(Blocks.Smelter) && w.hearth.stock.has(Vector(Stack(Items.Copper, 40), Stack(Items.Tin, 20), Stack(Items.Coal, 20)))) {
        val n = if(expands && w.hearthStock(Items.Copper) > 300) 3 else 1
        w.pay(Vector(Stack(Items.Copper, 4 * n), Stack(Items.Tin, 2 * n), Stack(Items.Coal, 2 * n)))
        w.hearth.stock.add(Items.Bronze, 4 * n)
      }
      if(w.knows(Blocks.GlassKiln) && w.hearth.stock.has(Vector(Stack(Items.Flint, 60), Stack(Items.Coal, 20)))) {
        w.pay(Vector(Stack(Items.Flint, 4), Stack(Items.Coal, 2)))
        w.hearth.stock.add(Items.Glass, 2)
      }
      if(w.knows(Blocks.Gearworks) && w.hearthStock(Items.Bronze) > 30) {
        w.pay(Vector(Stack(Items.Bronze, 4)))
        w.hearth.stock.add(Items.Gear, 2)
      }
      w.buildings.foreach {
        case t: TurretBuilding => t.role.ammo.reverse.foreach { case (item, _) => while(t.accepts(item, null) && w.hearthStock(item) > (if(item eq Items.Bronze) 250 else 30)) { w.pay1(item); t.receive(item, null) } }
        case z: BrazierBuilding => while(z.accepts(Items.Coal, null) && w.hearthStock(Items.Coal) > 5) { w.pay1(Items.Coal); z.receive(Items.Coal, null) }
        case _ => ()
      }
    }
  }
}

/** Plays valleys with bots and tells how it went. */
object Balance {
  def play(rules: Rules, skill: Float, limit: Int, expands: Boolean = false): (Valley, Bot) = {
    val w = MapGen.generate(rules)
    val bot = new Bot(w, skill, expands)
    if(expands) { w.learned += Blocks.Crossing; w.learned += Blocks.Zipline; bot.mineSeam(Items.Copper, Blocks.Drill, pays = false); bot.mineSeam(Items.Flint, Blocks.Drill, pays = false) } else core.Showcase.mine(w)
    // Not all at once: the drills of a first few minutes.
    var t = 0
    while(w.outcome == Outcome.Playing && t < limit * 60) {
      w.update(1f / 60f)
      bot.update(1f / 60f)
      w.effects.clear(); w.sounds.clear(); w.notices.clear()
      if(t % (60 * 45) == 0) bot.log :+= f"${t / 60}%4ds ${w.season.label}%-6s y${w.year} wave ${w.waves.number}%2d foes ${w.creatures.count(_.team == Valley.Bramble)}%3d turrets ${w.buildings.count(_.block.isTurret)}%2d lost ${w.lost}%3d hearth ${w.hearth.hp / w.hearth.maxHp * 100}%3.0f%% ${Items.all.map(i => i.key.take(2) + w.hearthStock(i)).mkString(" ")} | ${w.buildings.filter(_.block.isTurret).groupBy(_.block.key).map { case (k, v) => k + v.size }.mkString(" ")} empty ${w.buildings.count { case t: TurretBuilding => !t.hasAmmo; case _ => false }} | ${w.creatures.filter(_.team == Valley.Bramble).groupBy(_.kind.key).map { case (k, v) => k + v.size }.mkString(" ")} | knows ${w.learned.map(_.key).mkString(",")}"
      t += 1
    }
    (w, bot)
  }
  def main(args: Array[String]): Unit = {
    val which = args.headOption.map(_.toInt)
    for(i <- Valleys.all.indices if which.forall(_ == i)) {
      for((skill, expands) <- Vector((0.6f, true), (1f, true), (2f, true))) {
        val (w, bot) = play(Valleys.all(i).copy(goal = Goal.Survive(3)), skill, 2400, expands)
        println(s"== ${Valleys.all(i).name} skill $skill${if(expands) " expanding" else ""}: pods ${w.roots.pods.size} roots ${w.roots.size} ${w.outcome} after ${w.time.toInt}s, year ${w.year}, wave ${w.waves.number}, slain ${w.slain}, lost ${w.lost}")
        println("   drills " + w.buildings.collect { case d: DrillBuilding => d.ore.key }.groupBy(identity).map { case (k, v) => k + " " + v.size }.mkString(", ") + "; delivered " + Items.all.map(i => i.key + " " + w.deliveredCounts(i.id)).mkString(", "))
        if(args.length > 1) bot.log.foreach(println)
      }
    }
  }
}
