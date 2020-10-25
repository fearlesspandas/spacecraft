package minecraft.runnables

import events.{EventLoop, Events}
import org.bukkit.{ChatColor, Color, Material}
import org.bukkit.entity.{EntityType, Player}
import scala.collection.JavaConverters._
class PhantomRunnable(override val player:Player) extends SpaceCraftRunnable {
  override val eventType = Events.PhantomEvent
  def runner():Unit = if (player.isGliding){
      val entity = PhantomRunnable.entityPool((scala.math.random() * PhantomRunnable.entityPool.size).floor.toInt)
      player.sendMessage(s"${ChatColor.DARK_GREEN} ${entity.toString}'s are Attacking!")
      player.getWorld().spawnEntity(player.getTargetBlock(Set(Material.AIR).asJava,20).getLocation(), entity)
  }
}
object PhantomRunnable {
  val entityPool = Seq(EntityType.PHANTOM,EntityType.GHAST,EntityType.BLAZE)
  def apply(player:Player) = new PhantomRunnable(player)
}