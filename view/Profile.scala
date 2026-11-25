package hearthwheel
package core

import scala.util.Try

import hearthwheel.model._

/** A saved game, in the list of them. */
final case class SavedGame(id: Int, name: String, valley: Int, year: Int, season: Int, wave: Int, rules: Rules)

/** What is kept from one valley to the next, and the settings. */
final class Profile(services: Services) {
  var musicVolume = 0.6f
  var soundVolume = 0.7f
  var airVolume = 0.5f
  var uiScale = 1f
  var shake = true
  var stats = false
  var weather = true
  var ripples = true
  var particles = true
  var bloom = true
  var clouds = true
  var signs = true
  var minimap = true
  var hints = true
  var autoFire = true
  var wires = 1f
  var difficulty: Difficulty = Difficulty.Even
  val bindings = new Bindings
  def language: Lang.Language = Lang.language

  /** The valleys of the region that have been won. */
  var won: Set[Int] = Set.empty
  /** Blocks learned in the campaign. */
  var learned: Set[Block] = Set.empty
  /** What has been met, for the codex. */
  var met: Set[UnitKind] = Set.empty
  /** The store outside of the valleys. */
  val vault = new Array[Int](Items.Count)
  var deeds: Set[String] = Set.empty
  /** Hints that have been given. */
  var hinted: Set[String] = Set.empty
  var layouts: Vector[Vector[Plan]] = Vector.empty
  var games: Vector[SavedGame] = Vector.empty
  /** Maps made by hand, by number. */
  var maps: Vector[Int] = Vector.empty

  def canFullscreen: Boolean = services.canFullscreen
  def isFullscreen: Boolean = services.fullscreen
  def toggleFullscreen(): Unit = services.fullscreen = !services.fullscreen
  def copy(text: String): Unit = services.copy(text)
  def paste(use: String => Unit): Unit = services.paste(use)

  load()
  games = services.read("games").map(_.split('/').toVector.filter(_.nonEmpty).flatMap(readGame)).getOrElse(Vector.empty)

  private def load(): Unit = services.read("profile").foreach { text =>
    val fields = text.split(';').iterator.map(_.split("=", 2)).collect { case Array(k, v) => k -> v }.toMap
    def float(k: String, default: Float): Float = fields.get(k).flatMap(v => Try(v.toFloat).toOption).getOrElse(default)
    def flag(k: String, default: Boolean): Boolean = fields.get(k).map(_ == "1").getOrElse(default)
    def list(k: String): Vector[String] = fields.get(k).map(_.split(',').toVector.filter(_.nonEmpty)).getOrElse(Vector.empty)
    musicVolume = float("music", musicVolume)
    soundVolume = float("sound", soundVolume)
    airVolume = float("air", airVolume)
    uiScale = float("ui", uiScale)
    wires = float("wires", wires)
    shake = flag("shake", shake); stats = flag("stats", stats); weather = flag("weather", weather); ripples = flag("ripples", ripples)
    particles = flag("particles", particles); bloom = flag("bloom", bloom); clouds = flag("clouds", clouds); signs = flag("signs", signs)
    minimap = flag("minimap", minimap); hints = flag("hints", hints); autoFire = flag("autofire", autoFire)
    difficulty = Difficulty.fromOrdinal(MathUtil.clamp(float("difficulty", 1f).toInt, 0, Difficulty.values.length - 1))
    Lang.language = Lang.Language.values.find(l => fields.get("lang").contains(l.code)).getOrElse(Lang.Language.English)
    fields.get("keys").foreach(bindings.load)
    won = list("won").flatMap(v => Try(v.toInt).toOption).toSet
    learned = list("learned").flatMap(Blocks.byKey.get).toSet
    met = list("met").flatMap(UnitKinds.byKey.get).toSet
    deeds = list("deeds").toSet
    hinted = list("hinted").toSet
    maps = list("maps").flatMap(v => Try(v.toInt).toOption)
    list("vault").zipWithIndex.foreach { case (v, i) => if(i < vault.length) vault(i) = Try(v.toInt).getOrElse(0) }
    layouts = fields.get("layouts").map(_.split('/').toVector.filter(_.nonEmpty).flatMap(LayoutCodec.load)).getOrElse(Vector.empty)
  }
  private def readGame(text: String): Option[SavedGame] = Try {
    val Array(id, name, valley, year, season, wave, rules) = text.split("#", 7): @unchecked
    SavedGame(id.toInt, name, valley.toInt, year.toInt, season.toInt, wave.toInt, RulesCodec.read(new Reader(rules)))
  }.toOption

  def save(): Unit = {
    def flag(b: Boolean) = if(b) 1 else 0
    services.write("profile", Vector(
      "music=" + musicVolume, "sound=" + soundVolume, "air=" + airVolume, "ui=" + uiScale, "wires=" + wires,
      "shake=" + flag(shake), "stats=" + flag(stats), "weather=" + flag(weather), "ripples=" + flag(ripples), "particles=" + flag(particles),
      "bloom=" + flag(bloom), "clouds=" + flag(clouds), "signs=" + flag(signs), "minimap=" + flag(minimap), "hints=" + flag(hints), "autofire=" + flag(autoFire),
      "difficulty=" + difficulty.ordinal, "lang=" + language.code, "keys=" + bindings.save,
      "won=" + won.toVector.sorted.mkString(","), "learned=" + learned.toVector.map(_.key).sorted.mkString(","), "met=" + met.toVector.map(_.key).sorted.mkString(","),
      "deeds=" + deeds.toVector.sorted.mkString(","), "hinted=" + hinted.toVector.sorted.mkString(","), "maps=" + maps.mkString(","),
      "vault=" + vault.mkString(","), "layouts=" + layouts.map(LayoutCodec.save).mkString("/")
    ).mkString(";"))
  }
  private def saveGames(): Unit = services.write("games", games.map { g =>
    val out = new Writer
    RulesCodec.write(g.rules, out)
    Vector(g.id, g.name.filterNot(c => c == '#' || c == '/'), g.valley, g.year, g.season, g.wave, out.result).mkString("#")
  }.mkString("/"))

  def open(valley: Int): Boolean = {
    val p = Valleys.places(valley)
    (p.after.isEmpty || p.after.exists(won.contains)) && p.needs.forall(learned.contains)
  }
  def unlocked(valley: Int): Boolean = open(valley)

  // The valley being played, by its name.
  private def slot(name: String): String = "world_" + name.map(c => if(c.isLetterOrDigit) c else '_')
  def savedWorld(name: String): Option[String] = services.read(slot(name))
  def storeWorld(name: String, text: String): Unit = services.write(slot(name), text)
  def dropWorld(name: String): Unit = services.write(slot(name), "")

  // Games saved by hand.
  def keepGame(w: Valley, valley: Int): Unit = {
    val id = games.map(_.id).maxOption.getOrElse(0) + 1
    services.write("game_" + id, Codec.save(w))
    games = games :+ SavedGame(id, w.rules.name, valley, w.year, w.seasonIndex, w.waves.number, w.rules)
    saveGames()
  }
  /** Keeps one that came from elsewhere. */
  def keepGame(rules: Rules, valley: Int, w: Valley, text: String): Unit = {
    val id = games.map(_.id).maxOption.getOrElse(0) + 1
    services.write("game_" + id, text)
    games = games :+ SavedGame(id, rules.name, valley, w.year, w.seasonIndex, w.waves.number, rules)
    saveGames()
  }
  def gameText(g: SavedGame): Option[String] = services.read("game_" + g.id)
  def dropGame(g: SavedGame): Unit = {
    services.write("game_" + g.id, "")
    games = games.filterNot(_.id == g.id)
    saveGames()
  }

  def mapText(n: Int): Option[HandMap] = services.read("map_" + n).flatMap(HandMap.load(_).toOption)
  private val mapNames = scala.collection.mutable.HashMap.empty[Int, String]
  /** What a map was called, or nothing. */
  def mapName(n: Int): String = mapNames.getOrElseUpdate(n, mapText(n).map(_.rules.name).getOrElse(""))
  def keepMap(n: Int, m: HandMap): Unit = {
    mapNames(n) = m.rules.name
    services.write("map_" + n, m.save)
    if(!maps.contains(n)) maps = maps :+ n
    save()
  }
  def dropMap(n: Int): Unit = {
    services.write("map_" + n, "")
    maps = maps.filterNot(_ == n)
    save()
  }
  def freeMap: Int = maps.maxOption.getOrElse(0) + 1

  /** Brings what was sent out of a valley to the vault. */
  def collect(w: Valley): Unit = w.sentOut.indices.foreach { i => vault(i) += w.sentOut(i); w.sentOut(i) = 0 }
}
