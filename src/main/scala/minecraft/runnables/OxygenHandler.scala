package minecraft.runnables

import java.util.UUID

import events.Events
import events.Events.Events
import org.bukkit.Material
import org.bukkit.entity.Player

class OxygenHandler(override val player:Player) extends SpaceCraftRunnable {
  override val eventType: Events = Events.OxygenDiminishEvent
  val STARTING_MAX = 100
  override def runner(): Unit = if(player.getInventory.getHelmet != null && player.getInventory.getHelmet.getType == Material.DIAMOND_HELMET){
    val remaining = OxygenHandler.oxygenMap.getOrElse(player.getUniqueId,STARTING_MAX)
    if(remaining == 0) player.damage(player.getHealthScale/4)
    else {
      OxygenHandler.oxygenMap.update(player.getUniqueId, remaining - 1)
      player.sendMessage(s"${OxygenHandler.oxygenMap(player.getUniqueId)} O2 pods remaining. Consume blue ice to refill")
    }
  }else{
    player.damage(player.getHealthScale/2)
    player.sendMessage("Put on a diamond helmet or you'll suffocate")
  }
}
object OxygenHandler{
  val oxygenMap = scala.collection.mutable.Map[UUID,Int]()
  def apply(player:Player) = new OxygenHandler(player)

}