package hearthwheel
package core

import sgl.engine.gameobject.render2d.{Baked2D, Painter2D}
import sgl.engine.gameobject.render3d.Renderer
import sgl.engine.gameobject.{Color, Scope}
import hearthwheel.model._
import hearthwheel.model.MathUtil._

/** The land, baked in chunks for each season, and blended from one to the next. */
final class Terrain(world: Valley, art: Gallery, renderer: Renderer)(using Scope) {
  private val T = Valley.Tile.toFloat
  private val Chunk = 16
  private val columns = (world.width + Chunk - 1) / Chunk
  private val rows = (world.height + Chunk - 1) / Chunk
  private val floors = Array.fill(4)(new Array[Baked2D](columns * rows))
  private val solids = Array.fill(4)(new Array[Baked2D](columns * rows))
  private val shadows = new Array[Baked2D](columns * rows)
  private val light = new Noise(world.rules.seed.toInt + 5)
  // How deep within cliffs a tile is, which is how dark.
  private val depth = {
    val d = new Array[Byte](world.width * world.height)
    for(_ <- 1 to 4) {
      val before = d.clone()
      for(y <- 0 until world.height; x <- 0 until world.width) {
        val i = y * world.width + x
        if(world.solidAt(x, y) == Solid.Cliff) {
          val around = Vector((1, 0), (-1, 0), (0, 1), (0, -1)).map { case (dx, dy) =>
            if(!world.inside(x + dx, y + dy)) 4 else if(world.solidAt(x + dx, y + dy) != Solid.Cliff) 0 else before((y + dy) * world.width + x + dx).toInt
          }.min
          d(i) = scala.math.min(4, around + 1).toByte
        }
      }
    }
    d
  }

  private var shown = world.seasonIndex
  private var previous = -1
  private var blend = 1f

  def update(dt: Float, speed: Float): Unit = {
    if(world.seasonIndex != shown) {
      if(previous >= 0 && previous != world.seasonIndex) drop(previous)
      previous = shown
      shown = world.seasonIndex
      blend = 0f
    }
    if(blend < 1f) {
      blend += dt / 3.5f * speed
      if(blend >= 1f) {
        blend = 1f
        if(previous >= 0 && previous != shown) drop(previous)
        previous = -1
      }
    }
  }
  /** How far the land has gone from the last season to this one. */
  def seasonBlend: Float = blend
  def lastSeason: Int = if(previous >= 0) previous else shown

  private def drop(season: Int): Unit = for(i <- floors(season).indices) {
    if(floors(season)(i) != null) { renderer.releaseBaked(floors(season)(i)); floors(season)(i) = null }
    if(solids(season)(i) != null) { renderer.releaseBaked(solids(season)(i)); solids(season)(i) = null }
  }

  private def floorChunk(season: Int, i: Int): Baked2D = {
    if(floors(season)(i) == null) floors(season)(i) = renderer.bake2D(paintFloor(_, season, i % columns, i / columns))
    floors(season)(i)
  }
  private def solidChunk(season: Int, i: Int): Baked2D = {
    if(solids(season)(i) == null) solids(season)(i) = renderer.bake2D(paintSolids(_, season, i % columns, i / columns))
    solids(season)(i)
  }

  private def eachVisible(x0: Float, y0: Float, x1: Float, y1: Float)(f: Int => Unit): Unit = {
    val size = Chunk * T
    val cx0 = clamp(floor(x0 / size), 0, columns - 1)
    val cx1 = clamp(floor(x1 / size), 0, columns - 1)
    val cy0 = clamp(floor(y0 / size), 0, rows - 1)
    val cy1 = clamp(floor(y1 / size), 0, rows - 1)
    var cy = cy0
    while(cy <= cy1) {
      var cx = cx0
      while(cx <= cx1) { f(cy * columns + cx); cx += 1 }
      cy += 1
    }
  }

  def drawFloors(p: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float): Unit = {
    if(blend < 1f && previous >= 0) {
      eachVisible(x0, y0, x1, y1)(i => p.baked(floorChunk(previous, i)))
      val tint = Color(1f, 1f, 1f, blend)
      eachVisible(x0, y0, x1, y1)(i => p.baked(floorChunk(shown, i), tint = tint))
    } else eachVisible(x0, y0, x1, y1)(i => p.baked(floorChunk(shown, i)))

    // Shortly before a season turns, the next one is baked a chunk at a time,
    // drawn where it cannot be seen.
    if(blend >= 1f && world.secondsLeft < 6f && !world.longWinter) {
      val next = (shown + 1) % 4
      var n = 0
      eachVisible(x0, y0, x1, y1) { i =>
        if(floors(next)(i) == null && n == 0) { n += 1; p.baked(floorChunk(next, i), tint = Color.Transparent) }
        else if(solids(next)(i) == null && n == 0) { n += 1; p.baked(solidChunk(next, i), tint = Color.Transparent) }
      }
    }
  }

  def drawSolids(p: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float): Unit = {
    eachVisible(x0, y0, x1, y1) { i =>
      if(shadows(i) == null) shadows(i) = renderer.bake2D(paintShadows(_, i % columns, i / columns))
      p.baked(shadows(i))
    }
    if(blend < 1f && previous >= 0) {
      eachVisible(x0, y0, x1, y1)(i => p.baked(solidChunk(previous, i)))
      val tint = Color(1f, 1f, 1f, blend)
      eachVisible(x0, y0, x1, y1)(i => p.baked(solidChunk(shown, i), tint = tint))
    } else eachVisible(x0, y0, x1, y1)(i => p.baked(solidChunk(shown, i)))
  }

  // ------------------------------------------------------------ painting

  private def grain(x: Int, y: Int): Int = if(world.inside(x, y)) world.grain(y * world.width + x).toInt & 0x3f else 0
  private def bright(x: Int, y: Int): Float = 0.94f + 0.12f * light.fractal(x / 9f, y / 9f, 2)

  // Whether the floor is drawn as water, rather than as ice or a dry bed.
  private def isWater(f: Floor, season: Int): Boolean = f == Floor.Deep && season != 3 || f == Floor.Shallow && (season == 0 || season == 2)

  private def wetColor(x: Int, y: Int, palette: Palette): Color = {
    // Corner (x, y) lies between four tiles.
    var deep = 0f
    var dy = -1
    while(dy <= 0) {
      var dx = -1
      while(dx <= 0) {
        if(world.floorAt(x + dx, y + dy) == Floor.Deep) deep += 0.25f
        dx += 1
      }
      dy += 1
    }
    Tint.shade(Tint.mix(palette.shallow, palette.deep, deep), bright(x, y))
  }

  private def floorColor(f: Floor, palette: Palette): Color = if(f == Floor.Deep) palette.deep else if(f == Floor.Shallow) palette.shallow else palette.floors(f.ordinal)

  private def paintFloor(p: Painter2D, season: Int, cx: Int, cy: Int): Unit = {
    val palette = Palette.all(season)
    val x0 = cx * Chunk
    val y0 = cy * Chunk
    val x1 = scala.math.min(world.width, x0 + Chunk)
    val y1 = scala.math.min(world.height, y0 + Chunk)
    for(y <- y0 until y1; x <- x0 until x1) {
      val f = world.floorAt(x, y)
      val g = grain(x, y)
      val v = g & 3
      if(f.water) {
        val texture = if(f == Floor.Shallow && season == 1) art.bed(v) else if(season == 3) art.ice(v) else art.floors(0)(v)
        // Dry beds do not blend into the lake next to them; everything else does.
        if(season == 1) {
          val c = floorColor(f, palette)
          p.shaded(texture, x * T, y * T, T, T, Tint.shade(c, bright(x, y)), Tint.shade(c, bright(x + 1, y)), Tint.shade(c, bright(x + 1, y + 1)), Tint.shade(c, bright(x, y + 1)))
        } else
          p.shaded(texture, x * T, y * T, T, T, wetColor(x, y, palette), wetColor(x + 1, y, palette), wetColor(x + 1, y + 1, palette), wetColor(x, y + 1, palette))
      } else {
        val c = palette.floors(f.ordinal)
        p.shaded(art.floors(f.ordinal)(v), x * T, y * T, T, T, Tint.shade(c, bright(x, y)), Tint.shade(c, bright(x + 1, y)), Tint.shade(c, bright(x + 1, y + 1)), Tint.shade(c, bright(x, y + 1)))
      }
    }
    // Higher floors reach over the edges of lower ones.
    for(y <- y0 until y1; x <- x0 until x1) {
      val f = world.floorAt(x, y)
      val g = grain(x, y)
      var d = 0
      while(d < 4) {
        val n = world.floorAt(x + Dir.dx(d), y + Dir.dy(d))
        val over = n.ordinal > f.ordinal && world.inside(x + Dir.dx(d), y + Dir.dy(d)) && !(f.water && n.water && season != 1)
        if(over) {
          val c = Tint.shade(floorColor(n, palette), bright(x, y))
          // The fringe lies along the top; turned to the side it belongs to.
          val turn = (d + 1) * Pi / 2f
          val mx = (x + 0.5f) * T + Dir.dx(d) * (T / 2f - 10f)
          val my = (y + 0.5f) * T + Dir.dy(d) * (T / 2f - 10f)
          p.sprite(art.fringeSide((g >> d) & 1), mx, my, T, 20f, turn, c)
          if(isWater(f, season) && !n.water) {
            val sx = (x + 0.5f) * T + Dir.dx(d) * (T / 2f - 14f)
            val sy = (y + 0.5f) * T + Dir.dy(d) * (T / 2f - 14f)
            p.sprite(art.shore((g >> d) & 1), sx, sy, T, 10f, turn, Color(1f, 1f, 1f, 0.55f))
          }
        }
        d += 1
      }
      d = 0
      while(d < 4) {
        val e = (d + 1) & 3
        val n = world.floorAt(x + Dir.dx(d) + Dir.dx(e), y + Dir.dy(d) + Dir.dy(e))
        val a = world.floorAt(x + Dir.dx(d), y + Dir.dy(d))
        val b = world.floorAt(x + Dir.dx(e), y + Dir.dy(e))
        if(n.ordinal > f.ordinal && a.ordinal < n.ordinal && b.ordinal < n.ordinal && !(f.water && n.water && season != 1)) {
          val c = Tint.shade(floorColor(n, palette), bright(x, y))
          val mx = (x + 0.5f) * T + (Dir.dx(d) + Dir.dx(e)) * (T / 2f - 10f)
          val my = (y + 0.5f) * T + (Dir.dy(d) + Dir.dy(e)) * (T / 2f - 10f)
          p.sprite(art.fringeCorner(g & 1), mx, my, 20f, 20f, (d + 2) * Pi / 2f, c)
        }
        d += 1
      }
    }
    // Seams, then what grows and lies about.
    for(y <- y0 until y1; x <- x0 until x1) {
      val f = world.floorAt(x, y)
      val g = grain(x, y)
      val ore = world.oreAt(x, y)
      if(world.ventAt(x, y)) p.sprite(art.vent, (x + 0.5f) * T, (y + 0.5f) * T, T, T, (g & 3) * Pi / 2f)
      val mx = (x + 0.5f) * T
      val my = (y + 0.5f) * T
      if(ore >= 0 && art.ores(ore) != null) {
        val under = f == Floor.Shallow && season != 1
        val color = if(under) (if(season == 3) Color(0.85f, 0.95f, 1f, 0.45f) else Color(0.6f, 0.85f, 0.95f, 0.55f)) else Color.White
        p.sprite(art.ores(ore)(g % 3), mx, my, T, T, ((g >> 2) & 3) * Pi / 2f, color)
      } else if(world.solidAt(x, y) == Solid.Open && !f.water) {
        val ox = mx + ((g & 7) - 3.5f) * 2f
        val oy = my + (((g >> 3) & 7) - 3.5f) * 2f
        val roll = (x * 7919 + y * 104729 + g * 31) & 127
        f match {
          case Floor.Meadow => season match {
            case 0 => if(roll < 16) p.sprite(art.flowers(roll & 3), ox, oy, 12f, 12f) else if(roll < 24) p.sprite(art.tufts(roll & 1), ox, oy, 16f, 16f, 0f, Tint(0x8fc04e)) else if(roll == 25) p.sprite(art.mushroom, ox, oy, 11f, 11f)
            case 1 => if(roll < 20) p.sprite(art.tufts(roll & 1), ox, oy, 17f, 17f, 0f, Tint(0xd2c24e)) else if(roll < 24) p.sprite(art.flowers(0), ox, oy, 11f, 11f)
            case 2 => if(roll < 22) p.sprite(art.fallen(roll & 1), ox, oy, 17f, 17f, g * 0.4f) else if(roll < 30) p.sprite(art.tufts(roll & 1), ox, oy, 15f, 15f, 0f, Tint(0xb08a3e)) else if(roll < 32) p.sprite(art.mushroom, ox, oy, 12f, 12f)
            case _ => if(roll < 12) p.sprite(art.drifts(roll & 1), ox, oy, 22f, 22f)
          }
          case Floor.Loam => if(season == 3) { if(roll < 9) p.sprite(art.drifts(roll & 1), ox, oy, 20f, 20f) } else if(season == 2 && roll < 10) p.sprite(art.fallen(roll & 1), ox, oy, 16f, 16f, g * 0.4f) else if(roll < 14) p.sprite(art.pebbles(roll & 1), ox, oy, 12f, 12f, 0f, Tint(0xc9b296))
          case Floor.Gravel => if(roll < 18) p.sprite(art.pebbles(roll & 1), ox, oy, 13f, 13f)
          case Floor.Sand => if(season != 3 && roll < 12 && touchesWater(x, y)) p.sprite(art.reeds(roll & 1), ox, oy, 18f, 18f) else if(roll < 18) p.sprite(art.pebbles(roll & 1), ox, oy, 10f, 10f, 0f, Tint(0xeadfc8))
          case _ => ()
        }
      }
    }
  }
  // The shadows of cliffs and trees, which reach over the edges of a chunk.
  private def paintShadows(p: Painter2D, cx: Int, cy: Int): Unit = {
    val x0 = cx * Chunk
    val y0 = cy * Chunk
    for(y <- y0 until scala.math.min(world.height, y0 + Chunk); x <- x0 until scala.math.min(world.width, x0 + Chunk) if world.solidAt(x, y) != Solid.Open)
      p.sprite(art.shadow, (x + 0.5f) * T + 6f, (y + 0.5f) * T + 8f, T * 1.75f, T * 1.75f, 0f, Color(1f, 1f, 1f, 0.22f))
  }
  private def touchesWater(x: Int, y: Int): Boolean = (0 until 4).exists(d => world.floorAt(x + Dir.dx(d), y + Dir.dy(d)).water)

  private def paintSolids(p: Painter2D, season: Int, cx: Int, cy: Int): Unit = {
    val palette = Palette.all(season)
    val x0 = cx * Chunk
    val y0 = cy * Chunk
    val x1 = scala.math.min(world.width, x0 + Chunk)
    val y1 = scala.math.min(world.height, y0 + Chunk)
    for(y <- y0 until y1; x <- x0 until x1) {
      val g = grain(x, y)
      // Off the grid, and of all sizes, as they grow.
      val roll = (x * 73856093) ^ (y * 19349663) ^ (g * 83492791)
      val mx = (x + 0.5f) * T + (((roll >> 3) & 15) - 7.5f) * 0.75f
      val my = (y + 0.5f) * T + (((roll >> 8) & 15) - 7.5f) * 0.75f
      val vary = ((roll >> 13) & 15) / 15f
      world.solidAt(x, y) match {
        case Solid.Cliff =>
          val dark = 1f - 0.17f * (depth(y * world.width + x) - 1)
          val c = Tint.shade(palette.cliff, dark * (0.95f + (g & 7) * 0.012f))
          p.sprite(art.cliffs(g & 3), mx, my, 43f + vary * 15f, 43f + vary * 15f, ((roll >> 17) & 3) * 0.12f - 0.18f, c)
          if(season == 3) p.sprite(art.snowcap, mx, my - 1f, 42f, 42f, (g & 1) * Pi, Color(1f, 1f, 1f, 0.35f + 0.6f * dark))
          else if(season == 2 && (g & 15) == 3) p.sprite(art.fallen(g & 1), mx, my, 16f, 16f)
        case Solid.Thicket =>
          val s = 42f + vary * 20f
          if(season == 3) p.sprite(art.bare(g % 3), mx, my, s, s, g * 0.7f)
          else p.sprite(art.trees(g % 3), mx, my, s, s, ((roll >> 17) & 7) * 0.1f - 0.35f, Tint.shade(palette.trees(if(season == 0 && g % 3 == 2 && (g & 12) != 0) 0 else g % 3), 0.92f + (g & 7) * 0.018f))
        case Solid.Open => ()
      }
    }
  }

  /** Open water, in plain white, for an effect to play on. */
  def drawWater(p: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float): Unit = {
    val tx0 = clamp(floor(x0 / T), 0, world.width - 1)
    val tx1 = clamp(floor(x1 / T), 0, world.width - 1)
    val ty0 = clamp(floor(y0 / T), 0, world.height - 1)
    val ty1 = clamp(floor(y1 / T), 0, world.height - 1)
    def wet(x: Int, y: Int): Boolean = { val f = world.floorAt(x, y); f == Floor.Deep || (f == Floor.Shallow && world.waterOpen) }
    var y = ty0
    while(y <= ty1) {
      var x = tx0
      while(x <= tx1) {
        if(wet(x, y)) {
          // As long as it goes on.
          var n = 1
          while(x + n <= tx1 && wet(x + n, y)) n += 1
          p.rect(x * T, y * T, n * T, T, Color.White)
          x += n
        } else x += 1
      }
      y += 1
    }
  }

  /** Has a part of the land painted again, after an edit. */
  def touched(x: Int, y: Int): Unit =
    for(cy <- (y - 2) / Chunk to (y + 2) / Chunk; cx <- (x - 2) / Chunk to (x + 2) / Chunk if cx >= 0 && cy >= 0 && cx < columns && cy < rows) {
      val i = cy * columns + cx
      for(s <- 0 until 4) {
        if(floors(s)(i) != null) floors(s)(i).invalidate()
        if(solids(s)(i) != null) solids(s)(i).invalidate()
      }
      if(shadows(i) != null) shadows(i).invalidate()
    }

  /** Glints that drift over open water. */
  def drawRipples(p: Painter2D, x0: Float, y0: Float, x1: Float, y1: Float, time: Float): Unit = {
    if(world.frozen) return
    val tx0 = clamp(floor(x0 / T), 0, world.width - 1)
    val tx1 = clamp(floor(x1 / T), 0, world.width - 1)
    val ty0 = clamp(floor(y0 / T), 0, world.height - 1)
    val ty1 = clamp(floor(y1 / T), 0, world.height - 1)
    var y = ty0
    while(y <= ty1) {
      var x = tx0
      while(x <= tx1) {
        val f = world.floorAt(x, y)
        if(f == Floor.Deep || (f == Floor.Shallow && world.waterOpen)) {
          val g = grain(x, y)
          if((g & 1) == 0) {
            val phase = (time * 0.35f + g * 0.137f) % 1f
            val a = sin(phase * Pi) * (if(f == Floor.Deep) 0.34f else 0.5f)
            p.sprite(art.ripples((g >> 1) & 3), (x + 0.5f) * T + (phase - 0.5f) * 10f, (y + 0.5f) * T + ((g >> 3) - 3.5f), T, T, 0f, Tint.alpha(a))
          }
        }
        x += 1
      }
      y += 1
    }
  }
}
