package hearthwheel

import java.nio.file.Paths

import org.lwjgl.opengl.GL11C

import hearthwheel.core.PlayScene
import hearthwheel.model._

/** Measures a valley with far more in it than a game has: `stress [options]`. */
object Stress {
  private def option(args: Array[String], name: String, default: Int): Int = {
    val i = args.indexOf("--" + name)
    if(i >= 0 && i + 1 < args.length) args(i + 1).toInt else default
  }

  /** A floor of loops of belts, full of goods, with drills, turrets and a horde. */
  def valley(belts: Int, creatures: Int, turrets: Int): Valley = {
    val size = 200
    val rules = Rules("Stress", "", size, 3L, Goal.Endless, nests = 0, free = true, peaceful = true, seasons = Vector(1e6f, 1e6f, 1e6f, 1e6f))
    val w = new Valley(size, size, rules)
    java.util.Arrays.fill(w.floors, Floor.Meadow.ordinal.toByte)
    val rng = new Rng(4L)
    for(i <- w.grain.indices) w.grain(i) = (rng.nextInt() & 0x3f).toByte
    for(y <- 0 until size; x <- 0 until size if rng.chance(0.03f) && (x < 60 || x > 140)) w.solids(y * size + x) = 2
    w.place(Blocks.Hearth, 99, 99, 0, Valley.Player)
    w.lantern = w.spawn(UnitKinds.Lantern, Valley.Player, w.hearth.cx, w.hearth.cy)
    w.learned ++= Blocks.all
    w.builder.rebuild = false
    // Rings of belts around the Hearth.
    var placed = 0
    var r = 4
    while(placed < belts && r < 90) {
      val cx = 100
      val cy = 100
      for(i <- -r until r) {
        val around = Vector((cx + i, cy - r, 0), (cx + r, cy + i, 1), (cx - i, cy + r, 2), (cx - r, cy - i, 3))
        around.foreach { case (x, y, d) =>
          if(w.inside(x, y)) {
            w.place(Blocks.SwiftBelt, x, y, d, Valley.Player) match {
              case b: BeltBuilding => for(k <- 0 until 3) { b.items(k) = rng.int(Items.Count); b.at(k) = 0.9f - k * 0.3f }; b.count = 3
              case _ => ()
            }
            placed += 1
          }
        }
      }
      r += 1
    }
    var n = 0
    while(n < turrets) {
      val x = 20 + rng.int(160)
      val y = 20 + rng.int(160)
      if(w.buildingAt(x, y) == null) {
        w.place(Blocks.Slinger, x, y, 0, Valley.Player) match {
          case t: TurretBuilding => t.rounds(0) = 100000; t.loaded = 0
          case _ => ()
        }
        n += 1
      }
    }
    for(_ <- 0 until creatures) {
      val a = rng.float() * MathUtil.Tau
      val d = rng.range(400f, 2800f)
      val kind = rng.pick(Vector(UnitKinds.Stumper, UnitKinds.Nettle, UnitKinds.Dandel, UnitKinds.Hornwing, UnitKinds.Creepvine))
      val c = w.spawn(kind, Valley.Bramble, 3200f + MathUtil.cos(a) * d, 3200f + MathUtil.sin(a) * d)
      c.hp = 1e9f
    }
    w.buildings.foreach(b => b.hp = 1e9f)
    w
  }

  def main(args: Array[String]): Unit = {
    val belts = option(args, "belts", 6000)
    val creatures = option(args, "creatures", 1500)
    val turrets = option(args, "turrets", 400)
    val seconds = option(args, "seconds", 8)
    val zoom = option(args, "zoom", 60) / 100f

    // The rules alone.
    val w = valley(belts, creatures, turrets)
    for(_ <- 0 until 400) { w.update(1f / 60f); w.effects.clear(); w.sounds.clear() }
    val t0 = System.nanoTime()
    for(_ <- 0 until 300) { w.update(1f / 60f); w.effects.clear(); w.sounds.clear() }
    println(f"rules: ${(System.nanoTime() - t0) / 300e6}%.2f ms a step, ${w.buildings.size} buildings, ${w.creatures.size} creatures, ${w.shots.flying.size} shots")

    val game = new Headless(new PlayScene(() => valley(belts, creatures, turrets), -1), 1920, 1080)
    game.start()
    println("renderer: " + GL11C.glGetString(GL11C.GL_RENDERER))
    game.play.zoomGoal = zoom
    game.play.profile.stats = true
    def measure(label: String): Unit = {
      val warm = System.nanoTime() + 4000000000L
      while(System.nanoTime() < warm) game.step()
      val times = scala.collection.mutable.ArrayBuffer.empty[Double]
      val bean = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
      val cpu = bean.getCurrentThreadCpuTime
      val bytes = bean.getThreadAllocatedBytes(Thread.currentThread.getId)
      val end = System.nanoTime() + seconds * 1000000000L
      while(System.nanoTime() < end) {
        val s = System.nanoTime()
        game.step()
        GL11C.glFinish()
        times += (System.nanoTime() - s) / 1e6
      }
      val allocated = (bean.getThreadAllocatedBytes(Thread.currentThread.getId) - bytes) / times.size / 1024
      val cpuMillis = (bean.getCurrentThreadCpuTime - cpu) / 1e6 / times.size
      val sorted = times.sorted
      println(f"$label%-10s ${1000 * times.size / times.sum}%6.1f fps   p50 ${sorted(sorted.size / 2)}%5.2f ms   p95 ${sorted(sorted.size * 95 / 100)}%5.2f ms   cpu ${cpuMillis}%5.2f ms   ${allocated} KB a frame   ${game.play.view.visibleBuildings} buildings seen, ${game.play.particles.count} particles")
    }
    measure("running")
    game.play.paused = true
    measure("paused")
    if(args.contains("--picture")) game.save(Paths.get(args(args.indexOf("--picture") + 1)), "stress")
    game.stop()
  }
}
