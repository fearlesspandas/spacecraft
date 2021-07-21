package minecraft.runnables.typicalModels

import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import Typical.core.grammar._
import org.bukkit.scheduler.BukkitTask
import Players._
object PlayerGravityModel {
  case class PlayerGravityEvent(frequency:Double,value:Option[BukkitTask] = None) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
    override val name: String = "PlayerGravityEvent"
    override val probability: Double = 1

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = ???
  }
}
