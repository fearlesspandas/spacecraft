package minecraft.runnables

import events.Events
import events.Events.Events
import org.bukkit.Material
import org.bukkit.entity.{Firework, Player}
import org.bukkit.inventory.ItemStack

class RocketCharge(override val player:Player) extends SpaceCraftRunnable {
  override val eventType: Events = Events.RocketChargeEvent
  override def runner(): Unit = if(player.isFlying){
      val inventory = player.getInventory
      inventory.addItem(new ItemStack(Material.FIREWORK_ROCKET,1))
  }
}

object RocketCharge{
  def apply(player: Player) = new RocketCharge(player)
}