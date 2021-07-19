package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import org.bukkit.scheduler.BukkitTask
import Players._
object OxygenReplenishEvent {
  case class OxygenReplenishEvent(frequency:Double,probability:Double,value:Option[BukkitTask]) extends SpaceCraftPlayerEvent {
    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq()
    override val name: String = "OxygenReplenishEvent"
    def apply(bukkitTask: BukkitTask):SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = player.copy(oxygenRemaining = player.oxygenRemaining + 1)
  }
}
