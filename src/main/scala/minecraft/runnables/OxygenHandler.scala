package minecraft.runnables

import java.util.UUID

import events.Events
import events.Events.Events
import org.bukkit.{ChatColor, Material}
import org.bukkit.entity.Player

class OxygenHandler(override val player:Player) extends SpaceCraftRunnable {
  override val eventType: Events = Events.OxygenDiminishEvent

  override def runner(): Unit = if(player.getInventory.getHelmet != null && player.getInventory.getHelmet.getType == Material.DIAMOND_HELMET && player.isOnline){
    val remaining = OxygenHandler.getOrElse(player,OxygenHandler.STARTING_MAX)
    if(remaining <= 0) {
      player.sendMessage(s"${ChatColor.LIGHT_PURPLE} No oxy remaining")
      player.damage(player.getHealthScale/4)
    }
    else {
      OxygenHandler.update(player, remaining - 1)
      player.sendMessage(s"${ChatColor.AQUA} oxy:${OxygenHandler.get(player)}")
    }
  }else{
    player.damage(player.getHealthScale/2)
    player.sendMessage("Put on a diamond helmet or you'll suffocate")
  }
}
object OxygenHandler{
  val STARTING_MAX = 100
  val SIPHON_AMT = 3
  val oxyconverters:Seq[Material] = Seq(
    Material.BLUE_ICE
  )
  val oxygenMap = scala.collection.mutable.Map[UUID,Int]()
  def apply(player:Player) = new OxygenHandler(player)
  def update(player:Player,value:Int) = oxygenMap.update(player.getUniqueId,value)
  def get(player:Player) = oxygenMap.getOrElse(player.getUniqueId,STARTING_MAX)
  def getOrElse(player:Player,default:Int) = oxygenMap.getOrElse(player.getUniqueId,default)
}