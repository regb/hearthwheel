package hearthwheel.model

/** The way to a goal from everywhere: each tile knows the next one.
  *
  * Costs are small integers, so the frontier is a ring of buckets. A field is
  * worked out in slices, spread over several updates on large maps; until a new
  * one is complete the previous one is followed.
  */
final class FlowField(world: Valley, walk: Walk, forPlayer: Boolean) {
  private val size = world.width * world.height
  private val width = world.width
  private val Unreached = Int.MaxValue / 2
  // Working copy, swapped with `current` when it is complete.
  private var work = new Array[Int](size)
  private var current = Array.fill(size)(Unreached)
  private val Buckets = 128
  private val buckets = Array.fill(Buckets)(new IntBuffer)
  private var pending = 0
  private var cursor = 0
  private var running = false
  private var goals: Array[Int] = Array.empty
  private var dirty = true
  var ready = false

  def setGoals(tiles: Array[Int]): Unit = { goals = tiles; dirty = true }
  def invalidate(): Unit = dirty = true

  /** Does up to `budget` tiles of work. */
  def update(budget: Int): Unit = {
    if(!running && dirty) start()
    if(running) advance(budget)
  }
  def finish(): Unit = update(Int.MaxValue)

  private def start(): Unit = {
    dirty = false
    running = true
    java.util.Arrays.fill(work, Unreached)
    buckets.foreach(_.clear())
    pending = 0
    cursor = 0
    goals.foreach { g =>
      work(g) = 0
      buckets(0).add(g)
      pending += 1
    }
  }

  private def advance(budget: Int): Unit = {
    var left = budget
    while(pending > 0 && left > 0) {
      val bucket = buckets(cursor & (Buckets - 1))
      if(bucket.size == 0) cursor += 1
      else {
        val tile = bucket.removeLast()
        pending -= 1
        left -= 1
        val here = work(tile)
        if(here == cursor) {
          val x = tile % width
          val y = tile / width
          var d = 0
          while(d < 4) {
            val nx = x + Dir.dx(d)
            val ny = y + Dir.dy(d)
            if(nx >= 0 && ny >= 0 && nx < width && ny < world.height) {
              val n = ny * width + nx
              val step = if(forPlayer) world.walkCost(n, walk, Valley.Player) else world.walkCost(n, walk, Valley.Bramble)
              if(step >= 0) {
                val total = here + step
                if(total < work(n)) {
                  work(n) = total
                  buckets(total & (Buckets - 1)).add(n)
                  pending += 1
                }
              }
            }
            d += 1
          }
        }
      }
    }
    if(pending == 0) {
      running = false
      ready = true
      val done = work
      work = current
      current = done
    }
  }

  def cost(x: Int, y: Int): Int = current(y * width + x)
  def reaches(x: Int, y: Int): Boolean = current(y * width + x) < Unreached

  /** The tile to go to next from there, or -1 when there is none. */
  def next(x: Int, y: Int): Int = {
    var best = current(y * width + x)
    var result = -1
    var dy = -1
    while(dy <= 1) {
      var dx = -1
      while(dx <= 1) {
        if(dx != 0 || dy != 0) {
          val nx = x + dx
          val ny = y + dy
          if(nx >= 0 && ny >= 0 && nx < width && ny < world.height) {
            val c = current(ny * width + nx)
            // Diagonals only when neither corner is in the way.
            if(c < best && (dx == 0 || dy == 0 || (open(x + dx, y) && open(x, y + dy)))) {
              best = c
              result = ny * width + nx
            }
          }
        }
        dx += 1
      }
      dy += 1
    }
    result
  }
  private def open(x: Int, y: Int): Boolean = {
    val step = world.walkCost(y * width + x, walk, if(forPlayer) Valley.Player else Valley.Bramble)
    step >= 0 && step < 20
  }
}

final class IntBuffer {
  private var values = new Array[Int](64)
  var size = 0
  def add(v: Int): Unit = {
    if(size == values.length) values = java.util.Arrays.copyOf(values, size * 2)
    values(size) = v
    size += 1
  }
  def removeLast(): Int = { size -= 1; values(size) }
  def apply(i: Int): Int = values(i)
  def clear(): Unit = size = 0
}
