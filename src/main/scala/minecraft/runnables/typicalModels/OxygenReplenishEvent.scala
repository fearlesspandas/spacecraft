package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import org.bukkit.scheduler.BukkitTask
import Players._
import org.bukkit.ChatColor
object OxygenReplenishEvent {
  case class OxygenReplenishEvent(amount:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    val frequency = 0d
    val probability = 0d
    override val name: String = "OxygenReplenishEvent"
    def apply(bukkitTask: BukkitTask):SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
      if(amount < 0){player.sendMessage(s"${ChatColor.DARK_GRAY}$amount oxy")}
      else {player.sendMessage(s"${ChatColor.GREEN} +${amount} oxy")}
      player.copy(oxygenRemaining = player.oxygenRemaining + amount)
    }

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
  }
}
