package minecraft.events
import java.util.UUID

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.{EntitySpawnModel, ScoreboardDisplay}
import minecraft.runnables.typicalModels.EntitySpawnModel.{BlazeSpawnEvent, DragonSpawnEvent, GhastSpawnEvent, PhantomSpawnEvent}
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.HeightReminder.HeightReminder
import minecraft.runnables.typicalModels.OxygenModel.OxygenDepletionModel
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import minecraft.runnables.typicalModels.RocketChargeModel.RocketChargeModel
import minecraft.runnables.typicalModels.ScoreboardDisplay.ScoreboardDisplayModel
import org.bukkit.{Bukkit, Material}
import org.bukkit.entity.Player
import org.bukkit.event.block.BlockBreakEvent
import org.bukkit.event.entity.{EntityDamageByEntityEvent, EntityDamageEvent}
import org.bukkit.event.player.{PlayerJoinEvent, PlayerRespawnEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scoreboard.Score
import org.spigotmc.event.player.PlayerSpawnLocationEvent

import scala.collection.concurrent.TrieMap
object EventLoop {
  val oxyModel = OxygenDepletionModel(3,10000,None)
  val rocketChargeModel = RocketChargeModel(50000,None)
  val heightReminder = HeightReminder(5000)
  val oxyReplenishModel = OxygenReplenishEvent(3,None)
  val entitySpawnRate = 50000000
  val dragonModel = DragonSpawnEvent(entitySpawnRate,0)
  val ghastModel = GhastSpawnEvent(entitySpawnRate,0)
  val phantomModel = PhantomSpawnEvent(entitySpawnRate,0)
  val blazeModel = BlazeSpawnEvent(entitySpawnRate,0)
  val gravityEvent = PlayerGravityEvent(1000,1,Set(),1,20,10,None)
  val scoreboardman = ScoreboardDisplayModel(5)
  val eventManager:dataset[EventManager] = EventManager(
    TrieMap()
  )
  val entitySpawnTasks = Seq(
      dragonModel,
    ghastModel,
    phantomModel,
    blazeModel
  )
  val baseTasks:Seq[SpaceCraftPlayerEvent] = Seq(
    oxyModel,
    rocketChargeModel,
    heightReminder,
    gravityEvent
  ) //++ entitySpawnTasks
  implicit val serializer:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Unit = src => src.multifetch[SpaceCraftPlayerEvent].fold(_ => throw new Error("issue updating event manager"))(
    d => {
     val event = d.asInstanceOf[SpaceCraftPlayerEvent]
      //if(event.isInstanceOf[PlayerGravityEvent]) println(s"serialized blocks:${event.asInstanceOf[PlayerGravityEvent].knownBlocks}")
      for{
        player <- src.player
        em <- eventManager
      }yield {
        em.eventStack.push(player.postProcessing)
        em.value.update((event.name,player.getUniqueId),src)
        em
      }
    }
  )


  class EventLoopListener(plug:JavaPlugin) extends Listener {
    plug.getServer().getPluginManager().registerEvents(this,plug)
    @EventHandler
    def onPlayerJoin(event:PlayerJoinEvent):Unit = {
     event.getPlayer.sendMessage("Welcome to space")
      event.getPlayer.setGravity(false)
      val newplayer = SpaceCraftPlayer(event.getPlayer,100)
      EntitySpawnModel.fixEntityFlight(newplayer)
      baseTasks.foreach(e => eventManager.updateEvent(e,newplayer,plug))
    }
    @EventHandler
    def onPlayerSpawn(event:PlayerRespawnEvent):Unit =
      EntitySpawnModel.fixEntityFlight(event.getPlayer)

    @EventHandler
    def replenishOxyOnDamage(event:EntityDamageByEntityEvent):Unit = {
      event.getDamager match {
        case p:Player => for{
          em <- eventManager
          spcplayer <- em.value.getOrElse((oxyModel.name,p.getUniqueId),throw new Error(s"No OxygenModel found for ${p.getDisplayName}")).player
        }yield
          eventManager.updateEvent(oxyModel,spcplayer,plug,oxyReplenishModel)
      }
    }
    @EventHandler
    def replenishOxygenOnBlockDestroy(event:BlockBreakEvent):Unit = {
      val player = event.getPlayer
      event.getBlock.getType match {
        case Material.BLUE_ICE | Material.ICE | Material.FROSTED_ICE =>
          for {
            em <- eventManager
            spcplayer <- em.value.getOrElse((oxyModel.name, player.getUniqueId), throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
          } yield eventManager.updateEvent(oxyModel, spcplayer, plug, oxyReplenishModel)
        case _ => ()
      }
    }

  }

}



