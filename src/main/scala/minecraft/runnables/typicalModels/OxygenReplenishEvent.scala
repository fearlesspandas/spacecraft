package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import org.bukkit.scheduler.BukkitTask
import Players._
object OxygenReplenishEvent {
  case class OxygenReplenishEvent(amount:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    val frequency = 0d
    val probability = 0d
    override val name: String = "OxygenReplenishEvent"
    def apply(bukkitTask: BukkitTask):SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
      println("oxyReplenishHappening")
      player.copy(oxygenRemaining = player.oxygenRemaining + 1)
    }

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
  }
}
