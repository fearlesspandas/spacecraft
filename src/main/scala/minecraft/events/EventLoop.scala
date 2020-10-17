package events
import events.EventLoop.{runFromTimer, shouldRun}
import minecraft.IO.SettingsDecoder
import minecraft.runnables._
import net.md_5.bungee.api.chat.ClickEvent
import org.bukkit.{Bukkit, Material}
import org.bukkit.entity.Player
import org.bukkit.event.block.Action
import org.bukkit.event.entity.EntityAirChangeEvent
import org.bukkit.event.player.{PlayerInteractEntityEvent, PlayerInteractEvent, PlayerJoinEvent, PlayerToggleFlightEvent}
import org.bukkit.event.{EventHandler, EventPriority, Listener}
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scheduler.BukkitRunnable
object EventLoop {
  type EventId = Int
  var disabled:Boolean = false
  val frequencyMap = SettingsDecoder.getFreqSettings(SettingsDecoder.freqfileloc)
  val probabilityMap:scala.collection.mutable.Map[Events.Events,Double] = SettingsDecoder.getProbSettings(SettingsDecoder.probfileloc)
  val disabledMap:scala.collection.mutable.Map[(Events.Events,Player),Boolean] = scala.collection.mutable.Map()
  val cmdmap:scala.collection.mutable.Map[(Material,Player),String] = scala.collection.mutable.Map()
  def shouldRun(id:Events.Events,player:Player):Boolean = if (scala.math.random() <= probabilityMap(id)  && !disabled & !disabledMap.getOrElse((id,player),false)) true else false
  def runFromTimer(b:BukkitRunnable,plug:JavaPlugin,eventType:Events.Events) = b.runTaskTimer(plug,0,frequencyMap(eventType))
}

class EventLoopListener(plug:JavaPlugin) extends Listener {
  plug.getServer().getPluginManager().registerEvents(this,plug)

  val runnerMap:Map[Events.Events,Player => BukkitRunnable] = Map(
    Events.PhantomEvent -> (PhantomRunnable(_:Player)),
    Events.RocketChargeEvent -> (RocketCharge(_:Player)),
    Events.OxygenDiminishEvent -> (OxygenHandler(_:Player))
  )
  @EventHandler
  def onPlayerJoin(event:PlayerJoinEvent):Unit = {
    Events.values.foreach(e => runFromTimer(runnerMap(e)(event.getPlayer),plug,e))
  }
  @EventHandler
  def onClickEvent(event:PlayerInteractEvent):Unit = {
    val player = event.getPlayer
    val item = player.getInventory.getItemInMainHand
    val cmd = EventLoop.cmdmap.getOrElse((item.getType,player),"")
    if(cmd.size>0){
      player.sendMessage(s"Executing: $cmd")
      player.performCommand(cmd)
    }
  }
  @EventHandler(priority = EventPriority.HIGH)
  def oxygenReplenish(event:PlayerInteractEvent): Unit = if (event.getClickedBlock.getType == Material.BLUE_ICE && event.getAction ==Action.RIGHT_CLICK_BLOCK){
    OxygenHandler.oxygenMap.update(event.getPlayer.getUniqueId,OxygenHandler.oxygenMap.getOrElse(event.getPlayer.getUniqueId,0)+3)
    event.getPlayer.sendMessage(s"${OxygenHandler.oxygenMap(event.getPlayer.getUniqueId)} O2 pods remaining")
    event.getClickedBlock.breakNaturally()
  }
}

object Events extends Enumeration{
  type Events = Value
  val PhantomEvent,RocketChargeEvent,OxygenDiminishEvent = Value
}

