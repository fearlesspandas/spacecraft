package minecraft.events
import minecraft.IO.SettingsDecoder
import minecraft.runnables._
import org.bukkit.entity.{Item, Player}
import org.bukkit.event.block.Action
import org.bukkit.event.entity.{EntityDamageByEntityEvent, EntitySpawnEvent}
import org.bukkit.event.player.{PlayerDropItemEvent, PlayerInteractEvent, PlayerJoinEvent, PlayerMoveEvent}
import org.bukkit.event.{EventHandler, EventPriority, Listener}
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scheduler.{BukkitRunnable, BukkitTask}
import org.bukkit.{ChatColor, Material}
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.OxygenModel.OxygenDepletionModel
import minecraft.runnables.typicalModels.PlayerEvents.{PlayerEvents, SpaceCraftPlayerEvent}
import minecraft.runnables.typicalModels.Players.{Players, SpaceCraftPlayer}
import org.bukkit.event.world.ChunkLoadEvent
import minecraft.runnables.typicalModels.ListenerModel._
object EventLoop {
  type SpaceCraftDeps = OxygenHandler with Players with PlayerEvents with FrequencyMap with ProbabilityMap
  var dat:dataset[SpaceCraftDeps] = data[SpaceCraftDeps]()

  type EventId = Int
  var disabled:Boolean = false
  val frequencyMap = SettingsDecoder.getFreqSettings(SettingsDecoder.freqfileloc)
  val probabilityMap:scala.collection.mutable.Map[Events.Events,Double] = SettingsDecoder.getProbSettings(SettingsDecoder.probfileloc)
  val disabledMap:scala.collection.mutable.Map[(Events.Events,Player),Boolean] = scala.collection.mutable.Map()
  val cmdmap:scala.collection.mutable.Map[(Material,Player),String] = scala.collection.mutable.Map()
  def shouldRun(id:Events.Events,player:Player):Boolean = if (scala.math.random() <= probabilityMap(id)  && !disabled & !disabledMap.getOrElse((id,player),false)) true else false
  def runFromTimer(b:BukkitRunnable,plug:JavaPlugin,eventType:Events.Events):BukkitTask = b.runTaskTimer(plug,frequencyMap(eventType),frequencyMap(eventType))
  val runnerMap:Map[Events.Events,Player => SpaceCraftRunnable] = Map(
    Events.SpawnEvent -> (EntitySpawnRunnable(_:Player)),
    Events.RocketChargeEvent -> (RocketCharge(_:Player)),
    Events.OxygenDiminishEvent -> (OxygenHandler(_:Player))
  )
  class EventLoopListener(plug:JavaPlugin) extends Listener {
    plug.getServer().getPluginManager().registerEvents(this,plug)

    val oxyModel = OxygenDepletionModel(1000,3,Seq(Material.BLUE_ICE),1000)

    @EventHandler
    def onPlayerJoin(event:PlayerJoinEvent):Unit = {
     event.getPlayer.sendMessage("Welcome to space")
      val t = data[SpaceCraftPlayer]().++(SpaceCraftPlayer(event.getPlayer,100)).++[SpaceCraftPlayerEvent,OxygenDepletionModel](oxyModel)
      t.runTaskAsynchronously(plug)
    }
  }

  object Events extends Enumeration{
    type Events = Value
    val SpawnEvent,RocketChargeEvent,OxygenDiminishEvent,TerrainEvent = Value
    def addValue(str:String) = Value(str)
  }
}



