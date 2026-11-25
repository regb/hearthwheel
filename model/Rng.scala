package hearthwheel.model

/** A small deterministic generator (xorshift), the same on every platform. */
final class Rng(seed: Long) {
  private var state: Long = {
    val mixed = (seed ^ 0x5DEECE66DL) * 0x2545F4914F6CDD1DL + 0x9E3779B97F4A7C15L
    if(mixed == 0L) 0x1234567L else mixed
  }

  def save: Long = state
  def restore(value: Long): Unit = state = if(value == 0L) 0x1234567L else value

  def nextInt(): Int = {
    state ^= state << 13
    state ^= state >>> 7
    state ^= state << 17
    (state >>> 24).toInt
  }
  def int(bound: Int): Int = if(bound <= 1) 0 else (nextInt() & 0x7fffffff) % bound
  def float(): Float = (nextInt() & 0xffffff) / 16777216f
  def range(min: Float, max: Float): Float = min + (max - min) * float()
  def chance(p: Float): Boolean = float() < p
  def pick[A](values: IndexedSeq[A]): A = values(int(values.size))
}

/** Smooth noise over the plane, between 0 and 1. */
final class Noise(seed: Int) {
  private def hash(x: Int, y: Int): Float = {
    var h = seed * 374761393 + x * 668265263 + y * 1274126177
    h = (h ^ (h >>> 13)) * 1103515245
    h = h ^ (h >>> 16)
    (h & 0xffff) / 65535f
  }
  def at(x: Float, y: Float): Float = {
    val xi = scala.math.floor(x.toDouble).toInt
    val yi = scala.math.floor(y.toDouble).toInt
    val fx = x - xi
    val fy = y - yi
    val sx = fx * fx * (3f - 2f * fx)
    val sy = fy * fy * (3f - 2f * fy)
    val top = hash(xi, yi) * (1f - sx) + hash(xi + 1, yi) * sx
    val bottom = hash(xi, yi + 1) * (1f - sx) + hash(xi + 1, yi + 1) * sx
    top * (1f - sy) + bottom * sy
  }
  def fractal(x: Float, y: Float, octaves: Int): Float = {
    var sum = 0f
    var weight = 0f
    var amplitude = 1f
    var frequency = 1f
    var i = 0
    while(i < octaves) {
      sum += at(x * frequency, y * frequency) * amplitude
      weight += amplitude
      amplitude *= 0.5f
      frequency *= 2f
      i += 1
    }
    sum / weight
  }
}

object MathUtil {
  val Pi: Float = scala.math.Pi.toFloat
  val Tau: Float = Pi * 2f
  def sin(a: Float): Float = scala.math.sin(a.toDouble).toFloat
  def cos(a: Float): Float = scala.math.cos(a.toDouble).toFloat
  def sqrt(a: Float): Float = scala.math.sqrt(a.toDouble).toFloat
  def atan2(y: Float, x: Float): Float = scala.math.atan2(y.toDouble, x.toDouble).toFloat
  def clamp(v: Float, lo: Float, hi: Float): Float = if(v < lo) lo else if(v > hi) hi else v
  def clamp(v: Int, lo: Int, hi: Int): Int = if(v < lo) lo else if(v > hi) hi else v
  def lerp(a: Float, b: Float, t: Float): Float = a + (b - a) * t
  def dist(x0: Float, y0: Float, x1: Float, y1: Float): Float = sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0))
  def distI(x0: Int, y0: Int, x1: Int, y1: Int): Float = dist(x0.toFloat, y0.toFloat, x1.toFloat, y1.toFloat)
  def dist2(x0: Float, y0: Float, x1: Float, y1: Float): Float = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)
  /** The signed difference between angles, in (-Pi, Pi]. */
  def angleDiff(from: Float, to: Float): Float = {
    var d = (to - from) % Tau
    if(d > Pi) d -= Tau
    if(d <= -Pi) d += Tau
    d
  }
  def turnTo(from: Float, to: Float, max: Float): Float = {
    val d = angleDiff(from, to)
    if(d > max) from + max else if(d < -max) from - max else to
  }
  def floor(v: Float): Int = scala.math.floor(v.toDouble).toInt
}
