package minecraft.runnables.typicalModels

import org.bukkit.event.Event
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import org.bukkit.event.world.ChunkPopulateEvent
import Players._
import org.bukkit.scheduler.BukkitTask

import scala.reflect.runtime.universe.TypeTag
object TerrainModel {

  case class Schematic(location:String,probability:Double)
  case class Schematics(value:Seq[Schematic]) extends ::[Schematics] with produces[Seq[Schematic]]

  type TerrainGenDeps = Schematics
  case class TerrainGenModel(frequency:Double,probability:Double,schematics:Schematics,value:Option[BukkitTask] = None) extends SpaceCraftPlayerEvent {
    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq()
    override val name: String = "TerrainEvent"
    val schemaLoadCmd = (s:String) => s"schem load $s"
    def apply(bukkitTask:BukkitTask):SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(player: SpaceCraftPlayer):SpaceCraftPlayer = player match {
      case _ if player.isGliding =>
        val schematic = schematics.collectFirst({case s if s.probability >= scala.math.random() => s})
        .getOrElse(schematics.head)
        player.performCommand(schemaLoadCmd(schematic.location))
        player.performCommand("paste")
        player
    }
  }
}
