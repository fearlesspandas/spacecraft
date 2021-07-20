package minecraft.events
import java.util.UUID

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.HeightReminder.HeightReminder
import minecraft.runnables.typicalModels.OxygenModel.OxygenDepletionModel
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import minecraft.runnables.typicalModels.RocketChargeModel.RocketChargeModel
import org.bukkit.entity.Player
import org.bukkit.event.entity.EntityDamageEvent
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin
object EventLoop {
  val oxyModel = OxygenDepletionModel(3,10000,None)
  val rocketChargeModel = RocketChargeModel(50000,None)
  val heightReminder = HeightReminder(5000)
  val oxyReplenishModel = OxygenReplenishEvent(1,1,None)

  val eventManager:dataset[EventManager] = EventManager(
    scala.collection.mutable.Map.empty[(String,UUID),dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]]
  )
  val baseTasks:Seq[SpaceCraftPlayerEvent] = Seq(
    oxyModel,
    rocketChargeModel,
    heightReminder
  )
  class EventLoopListener(plug:JavaPlugin) extends Listener {
    plug.getServer().getPluginManager().registerEvents(this,plug)


    @EventHandler
    def onPlayerJoin(event:PlayerJoinEvent):Unit = {
     event.getPlayer.sendMessage("Welcome to space")
      val newplayer = SpaceCraftPlayer(event.getPlayer,100)
      baseTasks.foreach(e => eventManager.updateEvent(e,newplayer,plug))
    }
    @EventHandler
    def replenishOxyOnDamage(event:EntityDamageEvent):Unit = {
      event.getEntity match {
        case p:Player => for{
          em <- eventManager
          spcplayer <- em.value.getOrElse((oxyModel.name,p.getUniqueId),throw new Error(s"No OxygenModel found for ${p.getDisplayName}")).player
        }yield
          eventManager.updateEvent(oxyModel,spcplayer,plug,oxyReplenishModel)
      }
    }


  }

}



