package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import org.bukkit.Material
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.event.block.Action._
import org.bukkit.scheduler.BukkitTask
object RocketExperience {
  case class GainRocketExperience(event:PlayerInteractEvent,value:Option[BukkitTask]) extends SpaceCraftPlayerEvent {
    override val name: String = "GainRocketExp"
    override val frequency: Double = 0
    override val probability: Double = 1

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(player: Players.SpaceCraftPlayer): Players.SpaceCraftPlayer = {
      event.getAction match {
        case RIGHT_CLICK_AIR  if
        (
          player.getInventory.getItemInMainHand == Material.FIREWORK_ROCKET ||
          player.getInventory.getItemInOffHand == Material.FIREWORK_ROCKET
        ) &&
          player.isGliding
      => player.giveExp(1)
      }
      player
    }

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
  }
}
