package minecraft.events

import org.bukkit.scheduler.BukkitRunnable
import EventLoop._
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.Bukkit
import org.bukkit.entity.Player
import minecraft.PlayerCommandProcessor._
import minecraft.runnables.typicalModels.ItemGravityModel.ItemGravityEvent
object EventLoopTaskHandler {
case class EventLoopTask() extends BukkitRunnable{
  override def run(): Unit = for{
    em <- eventManager
  }yield {
    em.eventStack.foreach(f => f())
    em.eventStack.foreach( _ =>  em.eventStack.pop())
    em
  }
}
  abstract class ActiveBoards(val name:String)
  case object Vitals extends ActiveBoards("Vitals")
  case object GravityMeter extends ActiveBoards("GravityMeter")
  case object ItemBeamCounter extends ActiveBoards("ItemBeamCounter")

  case class ScoreboardTask() extends BukkitRunnable{

    def getOxy(src:dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]):Double = {
      src.player.biMap(_ => 0d)(_.get.oxygenRemaining)
    }
    def getGravity(player:Player,taskname:String):org.bukkit.util.Vector = (for{
      em <- eventManager
      t <- em.getTask(player,taskname).<--[SpaceCraftPlayerEvent]
    }yield t ).biMap(_ => new org.bukkit.util.Vector(0,0,0))(_.get.asInstanceOf[PlayerGravityEvent].lastForceVector)
    def avgGravity(player:Player,src:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]):Double =
      (
        getGravity(player,gravityEvent.name) add getGravity(player,gravityEvent2.name) add getGravity(player,gravityEvent3.name)
      ).length()/3*1000

    def itemsTracked(player:Player,src:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]):Double = {
      val t1 = eventManager.getTask(player, itemGravityEvent.name).<--[SpaceCraftPlayerEvent]
        .biMap(_ => 0d)(
          d => d.get.asInstanceOf[ItemGravityEvent].items.size
        )
      val t2 = eventManager.getTask(player, itemGravityEvent2.name).<--[SpaceCraftPlayerEvent]
        .biMap(_ => 0d)(
          d => d.get.asInstanceOf[ItemGravityEvent].items.size
        )
      t1 + t2
    }
    override def run(): Unit = Bukkit.getServer.getOnlinePlayers.forEach(p => {
      handleScore(p,Vitals,oxyModel,getOxy(_))
      handleScore(p,GravityMeter,gravityEvent,avgGravity(p,_))
      handleScore(p,ItemBeamCounter,itemGravityEvent,itemsTracked(p,_))

      givePlayerGravCompass(p)
    })
  }
}
