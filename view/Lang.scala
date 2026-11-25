package hearthwheel
package core

import scala.collection.mutable

/** Says things in the language of the player.
  *
  * The game speaks English throughout; what it says is translated where it is
  * drawn. So saves, rules and tests know of no other language, and a text that
  * has no translation is shown as it is.
  */
object Lang {
  enum Language(val label: String, val code: String) {
    case English extends Language("English", "en")
    case French extends Language("Français", "fr")
  }
  private var now: Language = Language.English
  def language: Language = now
  def language_=(l: Language): Unit = if(l != now) { now = l; known.clear() }

  /** What could not be said, when a test wants to know. */
  var missing: mutable.LinkedHashSet[String] = null

  // A text with parts that change, cut where they are.
  private final class Pattern(english: String, french: String) {
    private val cut: Array[String] = english.split("\\{\\}", -1)
    private val said: Array[String] = french.split("\\{\\}", -1)
    val weight: Int = cut.map(_.length).sum
    def apply(s: String, depth: Int): String = {
      if(!s.startsWith(cut(0)) || !s.endsWith(cut.last) || s.length < weight) return null
      val parts = new Array[String](cut.length - 1)
      var at = cut(0).length
      var i = 1
      while(i < cut.length) {
        val end = if(i == cut.length - 1) s.length - cut(i).length else s.indexOf(cut(i), at)
        if(end < at || (end == at && cut(i).nonEmpty) || end > s.length) return null
        parts(i - 1) = s.substring(at, end)
        at = end + cut(i).length
        i += 1
      }
      val out = new java.lang.StringBuilder(said(0))
      i = 0
      while(i < parts.length) {
        val _ = out.append(translate(parts(i), depth + 1)).append(if(i + 1 < said.length) said(i + 1) else "")
        i += 1
      }
      out.toString
    }
  }
  private lazy val exact: Map[String, String] = French.all.filterNot(_._1.contains("{}")).toMap
  private lazy val patterns: Vector[Pattern] = French.all.filter(_._1.contains("{}")).map { case (a, b) => new Pattern(a, b) }.filter(_.weight > 0).sortBy(-_.weight)
  private val known = new mutable.HashMap[String, String]

  /** Marks what a player wrote, which is never translated. */
  def verbatim(s: String): String = if(now == Language.English) s else "\u200b" + s

  def apply(s: String): String =
    if(s.nonEmpty && s.charAt(0) == '\u200b') s.substring(1)
    else if(now == Language.English || s.isEmpty) s
    else known.getOrElse(s, {
      val t = translate(s, 0)
      // Numbers change all the time: forget rather than fill up.
      if(known.size > 4000) known.clear()
      known(s) = t
      t
    })

  private def wordless(s: String): Boolean = { var i = 0; var n = 0; while(i < s.length) { if(s.charAt(i).isLetter) n += 1; i += 1 }; n < 2 }

  private def translate(s: String, depth: Int): String = {
    if(wordless(s)) return s
    val said = exact.getOrElse(s, null)
    if(said != null) return said
    // "5 Copper", "12  Burr"
    var d = 0
    while(d < s.length && (s.charAt(d).isDigit || s.charAt(d) == '.' || s.charAt(d) == 'k')) d += 1
    if(d > 0 && d < s.length && s.charAt(d) == ' ') {
      val counted = exact.getOrElse(s.substring(d).trim, null)
      if(counted != null) return s.substring(0, d + 1) + counted
    }
    if(depth < 4) {
      // Lists, of what is known piece by piece.
      var i = 0
      while(i < separators.length) {
        val pieces = split(s, separators(i))
        if(pieces.size > 1 && pieces.forall(p => wordless(p) || simple(p))) return pieces.map(translate(_, depth + 1)).mkString(separators(i))
        i += 1
      }
      i = 0
      while(i < patterns.size) {
        val t = patterns(i)(s, depth)
        if(t != null) return t
        i += 1
      }
      i = 0
      while(i < separators.length && depth < 3) {
        if(s.contains(separators(i))) return split(s, separators(i)).map(translate(_, depth + 1)).mkString(separators(i))
        i += 1
      }
    }
    if(missing != null) missing += s
    s
  }
  private val separators = Array(" = ", " + ", ", ")
  private def simple(s: String): Boolean = exact.contains(s) || { val t = s.dropWhile(c => c.isDigit || c == '.' || c == ' '); exact.contains(t) }
  private def split(s: String, sep: String): Vector[String] = {
    val out = Vector.newBuilder[String]
    var at = 0
    var i = s.indexOf(sep)
    while(i >= 0) { out += s.substring(at, i); at = i + sep.length; i = s.indexOf(sep, at) }
    out += s.substring(at)
    out.result()
  }
}
