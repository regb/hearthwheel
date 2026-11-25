package hearthwheel.model

import scala.collection.mutable
import MathUtil._

/** Makes a valley out of its rules. The same rules always give the same one. */
object MapGen {
  def generate(rules: Rules): Valley = {
    var attempt = 0
    while(attempt < 12) {
      val w = new Valley(rules.size, rules.size, rules)
      if(fill(w, rules, attempt)) return w
      attempt += 1
    }
    throw new IllegalStateException(s"No valley could be made for ${rules.name}")
  }

  private def fill(w: Valley, rules: Rules, attempt: Int): Boolean = {
    val n = rules.size
    val seed = (rules.seed * 31 + attempt * 1009).toInt
    val heights = new Noise(seed)
    val moisture = new Noise(seed + 71)
    val rough = new Noise(seed + 143)
    val detail = new Noise(seed + 211)
    val rng = new Rng(seed.toLong * 2654435761L + 5L)

    // The Hearth sits off center, opposite of where the Bramble grows.
    val angle = rng.float() * Tau
    val hx = clamp((n / 2f + cos(angle) * n * 0.18f).toInt, 24, n - 25)
    val hy = clamp((n / 2f + sin(angle) * n * 0.18f).toInt, 24, n - 25)

    // A river, as a wandering line through the map.
    val riverAngle = angle + Pi / 2f + rng.range(-0.4f, 0.4f)
    val riverOffset = rng.range(-0.10f, 0.10f) * n + n * 0.2f
    val riverWidth = 1.6f + rules.water * 3.2f

    var y = 0
    while(y < n) {
      var x = 0
      while(x < n) {
        val i = y * n + x
        val fx = x.toFloat
        val fy = y.toFloat
        val h = heights.fractal(fx / 26f, fy / 26f, 4)
        val m = moisture.fractal(fx / 20f, fy / 20f, 3)
        val r = rough.fractal(fx / 14f, fy / 14f, 4)
        val d = detail.fractal(fx / 7f, fy / 7f, 2)
        val home = distI(x, y, hx, hy)

        // Signed distance to the river.
        val rx = (fx - n / 2f) * cos(riverAngle) + (fy - n / 2f) * sin(riverAngle)
        val ry = -(fx - n / 2f) * sin(riverAngle) + (fy - n / 2f) * cos(riverAngle) - riverOffset
        val wander = sin(rx / 13f + seed % 7) * 6f + (heights.at(rx / 18f, 3.3f) - 0.5f) * 14f
        val river = scala.math.abs(ry - wander)

        val lake = h - (0.20f + rules.water * 0.17f) + scala.math.max(0f, (14f - home) * 0.03f)
        var floor =
          if(m > 0.6f) Floor.Loam
          else if(r > 0.62f) Floor.Gravel
          else Floor.Meadow
        if(lake < -0.035f || (rules.water > 0.3f && river < riverWidth - 1.4f && d > 0.36f)) floor = Floor.Deep
        else if(lake < 0.0f || river < riverWidth + d) floor = Floor.Shallow
        else if(lake < 0.03f || river < riverWidth + 1.1f + d * 2f) floor = Floor.Sand

        val edge = scala.math.min(scala.math.min(x, y), scala.math.min(n - 1 - x, n - 1 - y))
        var solid = Solid.Open
        val cliffy = r + (if(edge < 5) (5 - edge) * 0.12f else 0f)
        if(home > 13f && !floor.water && cliffy > 0.74f - rules.cliffs * 0.13f) solid = Solid.Cliff
        else if(home > 9f && !floor.water && m > 0.66f - rules.thickets * 0.12f && d > 0.36f && floor != Floor.Sand) solid = Solid.Thicket
        if(edge < 2) { solid = Solid.Cliff; if(floor.water) floor = Floor.Gravel }
        if(home < 8f) { solid = Solid.Open; if(floor.water) floor = Floor.Sand }

        w.floors(i) = floor.ordinal.toByte
        w.solids(i) = solid.ordinal.toByte
        w.grain(i) = (rng.nextInt() & 0x3f).toByte
        x += 1
      }
      y += 1
    }
    tidy(w)

    // The Hearth.
    w.place(Blocks.Hearth, hx - 1, hy - 1, 0, Valley.Player)
    rules.start.foreach(s => w.hearth.stock.add(s.item, s.amount))
    if(rules.free) Items.all.foreach(i => w.hearth.stock.add(i, 4000))
    w.groundField.finish()
    w.climbField.finish()

    // Briar Hearts, far by the way one walks in winter, when it is shortest.
    val far = farTiles(w, hx, hy, rng)
    val chosen = mutable.ArrayBuffer.empty[(Int, Int)]
    far.foreach { case (x, y) =>
      if(chosen.size < rules.nests && chosen.forall(c => distI(c._1, c._2, x, y) > n * 0.30f)) chosen += ((x, y))
    }
    if(chosen.size < rules.nests) return false
    chosen.foreach { case (x, y) =>
      for(dy <- -4 to 4; dx <- -4 to 4 if w.inside(x + dx, y + dy) && dx * dx + dy * dy <= 18) {
        val i = w.tileIndex(x + dx, y + dy)
        w.solids(i) = 0
        if(Floor.all(w.floors(i)).water) w.floors(i) = Floor.Loam.ordinal.toByte
        else w.floors(i) = Floor.Loam.ordinal.toByte
      }
      w.place(Blocks.Nest, x - 1, y - 1, 0, Valley.Bramble)
      val spin = rng.float() * Tau
      for(k <- 0 until 2) {
        val a = spin + k * Pi
        val tx = scala.math.round(x + cos(a) * 4f)
        val ty = scala.math.round(y + sin(a) * 4f)
        val open = (0 until 2).forall(j => (0 until 2).forall(i => w.inside(tx + i, ty + j) && w.solidAt(tx + i, ty + j) == Solid.Open && w.buildingAt(tx + i, ty + j) == null))
        if(open) { val _ = w.place(Blocks.Thorn, tx, ty, 0, Valley.Bramble) }
      }
    }
    w.groundField.invalidate()
    w.groundField.finish()
    if(chosen.exists { case (x, y) => !reachable(w, x, y) } && rules.goal != Goal.Cleanse) return false

    // Seams. The first of each kind that a start needs lie close to the Hearth.
    import Items._
    seam(w, rng, Copper, hx, hy, 9f, 13f, 4.2f)
    seam(w, rng, Flint, hx, hy, 9f, 15f, 3.2f)
    seam(w, rng, Tin, hx, hy, 11f, 17f, 3.4f)
    seam(w, rng, Coal, hx, hy, 12f, 19f, 3.4f)
    val extra = (n * n / 900f * rules.ore).toInt
    for(k <- 0 until extra) {
      val item = Vector(Copper, Tin, Coal, Flint, Copper, Coal)(k % 6)
      seam(w, rng, item, hx, hy, 20f, n * 0.62f, rng.range(2.6f, 4.6f))
    }
    // Iron lies further out, and vents wherever the ground is stony.
    seam(w, rng, Iron, hx, hy, 17f, 28f, 3.2f)
    for(_ <- 0 until (n / 26)) seam(w, rng, Iron, hx, hy, 26f, n * 0.6f, rng.range(2.6f, 4.2f))
    for(k <- 0 until (n / 24)) vents(w, rng, hx, hy, if(k == 0) 14f else 18f, if(k == 0) 26f else n * 0.55f)
    // Riverbeds hold the richest seams, for those who wait for summer.
    for(k <- 0 until (n / 18)) bedSeam(w, rng, Vector(Copper, Tin, Flint)(k % 3), hx, hy)

    // Groves near the start, for resin.
    grove(w, rng, hx, hy)
    if(Vector(Copper, Tin, Coal, Flint).exists(item => countNear(w, item, hx, hy, 20) < 10)) return false

    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, w.hearth.cx, w.hearth.cy + 60f)
    if(rules.free) w.learned ++= Blocks.all.filter(_.category.nonEmpty)
    else w.learned ++= Pages.basics
    w.groundField.invalidate()
    w.groundField.finish()
    w.climbField.finish()
    true
  }

  // Takes away single tiles that stick out, and the specks.
  private def tidy(w: Valley): Unit = for(_ <- 0 until 2) {
    val before = w.solids.clone()
    val floorsBefore = w.floors.clone()
    for(y <- 1 until w.height - 1; x <- 1 until w.width - 1) {
      val i = y * w.width + x
      val around = Array(i - 1, i + 1, i - w.width, i + w.width)
      val same = around.count(j => before(j) == before(i))
      if(before(i) != 0 && same <= 1) w.solids(i) = 0
      val f = floorsBefore(i)
      if(around.count(j => floorsBefore(j) == f) == 0) w.floors(i) = floorsBefore(i - 1)
    }
  }

  private def reachable(w: Valley, x: Int, y: Int): Boolean =
    (-4 to 4).exists(dy => (-4 to 4).exists(dx => w.inside(x + dx, y + dy) && w.groundField.reaches(x + dx, y + dy)))

  private def farTiles(w: Valley, hx: Int, hy: Int, rng: Rng): Vector[(Int, Int)] = {
    val n = w.width
    val found = mutable.ArrayBuffer.empty[(Int, Int, Float)]
    for(_ <- 0 until 900) {
      val x = 8 + rng.int(n - 16)
      val y = 8 + rng.int(n - 16)
      val d = distI(x, y, hx, hy)
      val ok = d > n * 0.38f && !w.floorAt(x, y).water && w.solidAt(x, y) == Solid.Open &&
        (w.groundField.reaches(x, y) || w.rules.goal == Goal.Cleanse)
      if(ok) found += ((x, y, d + rng.float() * n * 0.2f))
    }
    found.sortBy(-_._3).map(t => (t._1, t._2)).toVector
  }

  private def seam(w: Valley, rng: Rng, item: Item, hx: Int, hy: Int, near: Float, far: Float, radius: Float): Unit = {
    var tries = 0
    var done = false
    while(tries < 120 && !done) {
      tries += 1
      val a = rng.float() * Tau
      val d = rng.range(near, far)
      val x = (hx + cos(a) * d).toInt
      val y = (hy + sin(a) * d).toInt
      val r = radius.toInt + 1
      val clear = w.inside(x - r, y - r) && w.inside(x + r, y + r) && {
        var bad = 0
        for(dy <- -r to r; dx <- -r to r) {
          if(w.oreAt(x + dx, y + dy) >= 0) bad += 3
          if(w.solidAt(x + dx, y + dy) != Solid.Open) bad += 1
          if(w.floorAt(x + dx, y + dy).water) bad += 1
          if(w.buildingAt(x + dx, y + dy) != null) bad += 50
        }
        bad < (if(tries < 60) 4 else 22)
      }
      if(clear) { blob(w, rng, item, x, y, radius, onWater = false); done = true }
    }
  }
  private def blob(w: Valley, rng: Rng, item: Item, x: Int, y: Int, radius: Float, onWater: Boolean): Unit = {
    val shape = new Noise(rng.nextInt())
    val r = radius.toInt + 2
    for(dy <- -r to r; dx <- -r to r if w.inside(x + dx, y + dy)) {
      val d = sqrt((dx * dx + dy * dy).toFloat) + (shape.at(dx * 0.55f + 9f, dy * 0.55f + 9f) - 0.5f) * 3f
      val i = w.tileIndex(x + dx, y + dy)
      val f = Floor.all(w.floors(i))
      val fits = if(onWater) f == Floor.Shallow else !f.water
      if(d <= radius && fits && w.solids(i) == 0 && w.buildingAt(x + dx, y + dy) == null && !rng.chance(0.06f)) w.ores(i) = item.id.toByte
    }
  }
  private def bedSeam(w: Valley, rng: Rng, item: Item, hx: Int, hy: Int): Unit = {
    var tries = 0
    while(tries < 200) {
      tries += 1
      val x = 4 + rng.int(w.width - 8)
      val y = 4 + rng.int(w.height - 8)
      if(w.floorAt(x, y) == Floor.Shallow && w.oreAt(x, y) < 0 && distI(x, y, hx, hy) < w.width * 0.45f) {
        blob(w, rng, item, x, y, 3.6f, onWater = true)
        return
      }
    }
  }
  private def vents(w: Valley, rng: Rng, hx: Int, hy: Int, near: Float, far: Float): Unit = {
    var tries = 0
    var done = false
    while(!done && tries < 120) {
      tries += 1
      val a = rng.float() * Tau
      val d = rng.range(near, far)
      val x = (hx + cos(a) * d).toInt
      val y = (hy + sin(a) * d).toInt
      val open = (-1 to 2).forall(dy => (-1 to 2).forall(dx => w.inside(x + dx, y + dy) && w.oreAt(x + dx, y + dy) < 0 && w.solidAt(x + dx, y + dy) == Solid.Open && !w.floorAt(x + dx, y + dy).water && w.buildingAt(x + dx, y + dy) == null))
      if(open) {
        done = true
        for(dy <- 0 to 1; dx <- 0 to 1) {
          val i = w.tileIndex(x + dx, y + dy)
          w.ores(i) = Valley.Vent
        }
        for(dy <- -1 to 2; dx <- -1 to 2) w.floors(w.tileIndex(x + dx, y + dy)) = Floor.Gravel.ordinal.toByte
      }
    }
  }
  private def grove(w: Valley, rng: Rng, hx: Int, hy: Int): Unit = {
    var tries = 0
    while(tries < 200) {
      tries += 1
      val a = rng.float() * Tau
      val d = rng.range(11f, 16f)
      val x = (hx + cos(a) * d).toInt
      val y = (hy + sin(a) * d).toInt
      val ok = (-2 to 2).forall(dy => (-2 to 2).forall(dx => w.inside(x + dx, y + dy) && w.oreAt(x + dx, y + dy) < 0 && !w.floorAt(x + dx, y + dy).water && w.buildingAt(x + dx, y + dy) == null))
      if(ok) {
        for(dy <- -2 to 2; dx <- -2 to 2) {
          if(dx * dx + dy * dy <= 5 && !(dx == 0 && dy == 0) && rng.chance(0.85f)) w.solids(w.tileIndex(x + dx, y + dy)) = Solid.Thicket.ordinal.toByte
        }
        return
      }
    }
  }
  private def countNear(w: Valley, item: Item, hx: Int, hy: Int, r: Int): Int = {
    var c = 0
    for(y <- hy - r to hy + r; x <- hx - r to hx + r) if(w.oreAt(x, y) == item.id) c += 1
    c
  }
}
