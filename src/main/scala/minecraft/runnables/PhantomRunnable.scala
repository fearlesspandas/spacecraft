package minecraft.runnables

import events.{EventLoop, Events}
import org.bukkit.entity.{EntityType, Player}

class PhantomRunnable(override val player:Player) extends SpaceCraftRunnable {
  override val eventType = Events.PhantomEvent
  def runner():Unit = if (player.isFlying()){
      player.sendMessage("Phantoms are Attacking!")
      player.getWorld().spawnEntity(player.getLocation(), EntityType.PHANTOM)
  }
}
object PhantomRunnable{
  def apply(player:Player) = new PhantomRunnable(player)
}