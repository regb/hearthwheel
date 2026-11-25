package hearthwheel
package core

import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** Puts a working base into a valley, to look at and to measure with. */
object Showcase {
  def build(w: Valley): Valley = {
    w.learned ++= Blocks.all.filter(_.category.nonEmpty)
    val hx = w.hearth.x
    val hy = w.hearth.y
    def free(x: Int, y: Int, size: Int): Boolean = (0 until size).forall(j => (0 until size).forall { i =>
      w.inside(x + i, y + j) && w.solidAt(x + i, y + j) == Solid.Open && !w.floorAt(x + i, y + j).water && w.buildingAt(x + i, y + j) == null
    })
    def put(b: Block, x: Int, y: Int, rotation: Int = 0): Building = if(free(x, y, b.size)) w.place(b, x, y, rotation, Valley.Player) else null
    def fill(b: Building): Unit = b match {
      case t: TurretBuilding => t.role.ammo.lastOption.foreach { case (item, _) => for(_ <- 0 until 12) t.receive(item, null) }
      case _ => ()
    }
    mine(w)
    // Turrets around the Hearth.
    val ring = Vector(Blocks.Slinger, Blocks.Emberpot, Blocks.Slinger, Blocks.Boltcaster, Blocks.Slinger, Blocks.Bell, Blocks.Slinger, Blocks.Emberpot)
    for(k <- 0 until 16) {
      val a = k * Tau / 16f
      val x = scala.math.round(hx + 1 + cos(a) * 9f)
      val y = scala.math.round(hy + 1 + sin(a) * 9f)
      val b = put(ring(k % ring.size), x, y)
      if(b != null) fill(b)
    }
    // Works, power and warmth.
    for((b, dx, dy) <- Vector[(Block, Int, Int)](
      (Blocks.Windmill, -5, -5), (Blocks.Windmill, -5, -2), (Blocks.Windmill, -5, 1), (Blocks.SunLens, 6, -5), (Blocks.SunLens, 6, -2), (Blocks.Flywheel, 6, 1),
      (Blocks.Pylon, -2, -2), (Blocks.Pylon, 4, -2), (Blocks.Pylon, 4, 5), (Blocks.Pylon, -2, 5), (Blocks.Smelter, -1, -5), (Blocks.GlassKiln, 2, -5), (Blocks.Gearworks, 6, 4),
      (Blocks.Tinker, -2, 0), (Blocks.Tinker, 4, 0), (Blocks.Brazier, -6, 5), (Blocks.Brazier, 9, -6), (Blocks.Brazier, 9, 8), (Blocks.Brazier, -6, -6), (Blocks.Almanac, -2, 6)
    )) {
      val made = put(b, hx + dx, hy + dy)
      made match {
        case c: CrafterBuilding => c.recipe.in.foreach(s => c.stock.add(s.item, 900))
        case z: BrazierBuilding => for(_ <- 0 until 10) z.receive(Items.Ember, null)
        case _ => ()
      }
    }
    w.builder.rebuild = true
    w
  }

  /** Drills on every seam around, with belts that lead to the Hearth. */
  def mine(w: Valley, block: Block = Blocks.Drill, most: Int = 26, from: Int = 4, reach: Int = 26, pays: Boolean = false, ore: Int = -1): Unit = {
    val h = w.hearth
    val field = new FlowField(w, Walk.Ground, forPlayer = true)
    field.setGoals((for(y <- h.y until h.y + 3; x <- h.x until h.x + 3) yield w.tileIndex(x, y)).toArray)
    field.finish()
    var drills = 0
    for(r <- from to reach; y <- h.y - r to h.y + r; x <- h.x - r to h.x + r) {
      if((scala.math.abs(x - h.x) == r || scala.math.abs(y - h.y) == r) && drills < most && w.inside(x, y) && w.oreAt(x, y) >= 0 && (ore < 0 || w.oreAt(x, y) == ore) && (!pays || w.canPay(block.cost)) && DrillBuilding.survey(w, block, x, y)._2 >= block.size + 1) {
        if(w.placementProblem(block, x, y, 0) == null) {
          if(pays) w.pay(block.cost)
          w.place(block, x, y, 0, Valley.Player)
          drills += 1
          lead(w, field, x, y, block.size)
        }
      }
    }
  }

  // Four-way steps down the field from beside a drill, joining any belt met.
  private def lead(w: Valley, field: FlowField, dx: Int, dy: Int, size: Int): Unit = {
    val starts = (0 until size).toVector.flatMap(k => Vector((dx - 1, dy + k), (dx + size, dy + k), (dx + k, dy - 1), (dx + k, dy + size)))
      .filter { case (x, y) => w.inside(x, y) && field.reaches(x, y) }
    if(!starts.exists { case (x, y) => w.buildingAt(x, y).isInstanceOf[BeltBuilding] }) starts.filter { case (x, y) => w.buildingAt(x, y) == null }.sortBy { case (x, y) => field.cost(x, y) }.headOption.foreach { case (sx, sy) =>
      var x = sx
      var y = sy
      var steps = 0
      var going = true
      while(going && steps < 80) {
        steps += 1
        val options = (0 until 4).filter { d =>
          val nx = x + Dir.dx(d)
          val ny = y + Dir.dy(d)
          w.inside(nx, ny) && (field.cost(nx, ny) < field.cost(x, y)) && {
            val b = w.buildingAt(nx, ny)
            b == null || b.isInstanceOf[HearthBuilding] || (b match { case o: BeltBuilding => o.rotation != Dir.opposite(d); case _ => false })
          }
        }
        if(options.isEmpty) going = false
        else {
          val d = options.minBy(d => field.cost(x + Dir.dx(d), y + Dir.dy(d)))
          if(w.placementProblem(Blocks.Belt, x, y, d) != null) going = false
          else {
            w.place(Blocks.Belt, x, y, d, Valley.Player)
            x += Dir.dx(d)
            y += Dir.dy(d)
            if(w.buildingAt(x, y) != null) going = false
          }
        }
      }
    }
  }
}
