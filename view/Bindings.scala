package hearthwheel
package core

import sgl.Input.Keys

/** What keys do. Each can be given another key in the settings. */
enum Command(val label: String, val default: Keys.Key) {
  case North extends Command("Fly up", Keys.W)
  case West extends Command("Fly left", Keys.A)
  case South extends Command("Fly down", Keys.S)
  case East extends Command("Fly right", Keys.D)
  case Pause extends Command("Pause", Keys.Space)
  case Speed extends Command("Speed", Keys.V)
  case Turn extends Command("Turn", Keys.R)
  case Flip extends Command("Flip a layout", Keys.Z)
  case Pick extends Command("Pick up", Keys.Q)
  case Remove extends Command("Take down", Keys.X)
  case Copy extends Command("Copy a layout", Keys.C)
  case Layouts extends Command("Kept layouts", Keys.L)
  case Rebuild extends Command("Rebuild an area", Keys.U)
  case Crew extends Command("Command automata", Keys.F)
  case AllCrew extends Command("Choose all automata", Keys.Y)
  case Ride extends Command("Ride an automaton", Keys.E)
  case Learn extends Command("Almanac", Keys.T)
  case Codex extends Command("Codex", Keys.I)
  case Map extends Command("Map", Keys.M)
  case Ledger extends Command("Ledger", Keys.K)
  case Help extends Command("Help", Keys.H)
  case StopBuilding extends Command("Stop or go on building", Keys.B)
  case KeepRuins extends Command("Rebuild ruins by itself", Keys.G)
  case NextWave extends Command("Call the next wave", Keys.N)
  case DropPlans extends Command("Drop all plans", Keys.Backspace)
  case Hide extends Command("Hide the interface", Keys.J)
}

final class Bindings {
  private var keys: scala.collection.immutable.Map[Command, Keys.Key] = Command.values.map(c => c -> c.default).toMap
  def keyOf(c: Command): Keys.Key = keys(c)
  def commandOf(k: Keys.Key): Option[Command] = Command.values.find(c => keys(c) == k)
  /** Binds a key; a command that had it takes the old one. */
  def bind(c: Command, k: Keys.Key): Unit = {
    val old = keys(c)
    commandOf(k).foreach(other => keys = keys.updated(other, old))
    keys = keys.updated(c, k)
  }
  def reset(): Unit = keys = Command.values.map(c => c -> c.default).toMap
  def name(c: Command): String = Bindings.nameOf(keys(c))

  def save: String = Command.values.filter(c => keys(c) != c.default).map(c => c.toString + ":" + keys(c)).mkString(",")
  def load(text: String): Unit = text.split(',').foreach { pair =>
    pair.split(':') match {
      case Array(c, k) =>
        for { command <- Command.values.find(_.toString == c); key <- Bindings.all.find(_.toString == k) } keys = keys.updated(command, key)
      case _ => ()
    }
  }
}
object Bindings {
  val all: Vector[Keys.Key] = Vector(
    Keys.A, Keys.B, Keys.C, Keys.D, Keys.E, Keys.F, Keys.G, Keys.H, Keys.I, Keys.J, Keys.K, Keys.L, Keys.M, Keys.N, Keys.O, Keys.P, Keys.Q, Keys.R,
    Keys.S, Keys.T, Keys.U, Keys.V, Keys.W, Keys.X, Keys.Y, Keys.Z, Keys.Space, Keys.Backspace, Keys.Enter
  )
  def nameOf(k: Keys.Key): String = k match {
    case n: Keys.NumKey => n.toInt.toString
    case other => other.toString
  }
}
