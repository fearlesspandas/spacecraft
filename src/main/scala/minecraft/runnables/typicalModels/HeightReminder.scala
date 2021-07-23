package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import Players._
import org.bukkit.scheduler.BukkitTask
object HeightReminder {
case class HeightReminder(frequency:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
  val probability = 1
  override val name: String = "HeightReminder"
  def apply(bukkitTask:BukkitTask) = this.copy(value = Some(bukkitTask))
  override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
    if(player.getLocation.getY > 300) player.sendMessage("It may not look like it but ou're really high up")
    player
  }

  override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = HeightReminder(frequency,this.value)

  override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
}
}
