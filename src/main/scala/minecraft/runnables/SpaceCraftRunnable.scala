package minecraft.runnables

import events.{EventLoop, Events}
import org.bukkit.entity.Player
import org.bukkit.scheduler.BukkitRunnable

trait SpaceCraftRunnable extends BukkitRunnable{
  val eventType:Events.Events
  val player:Player
  def runner():Unit
  override def run(): Unit = if(EventLoop.shouldRun(eventType,player)) runner()
}
