package events
import events.EventLoop.{runFromTimer, shouldRun}
import minecraft.IO.SettingsDecoder
import minecraft.runnables._
import net.md_5.bungee.api.chat.ClickEvent
import org.bukkit.{Bukkit, ChatColor, Material}
import org.bukkit.entity.Player
import org.bukkit.event.block.Action
import org.bukkit.event.entity.{EntityAirChangeEvent, EntityDamageByEntityEvent, EntityDamageEvent, EntityDeathEvent, EntitySpawnEvent}
import org.bukkit.event.player.{PlayerDropItemEvent, PlayerInteractEntityEvent, PlayerInteractEvent, PlayerJoinEvent, PlayerRespawnEvent, PlayerToggleFlightEvent}
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
  def runFromTimer(b:BukkitRunnable,plug:JavaPlugin,eventType:Events.Events) = b.runTaskTimer(plug,frequencyMap(eventType),frequencyMap(eventType))
  val runnerMap:Map[Events.Events,Player => SpaceCraftRunnable] = Map(
    Events.SpawnEvent -> (EntitySpawnRunnable(_:Player)),
    Events.RocketChargeEvent -> (RocketCharge(_:Player)),
    Events.OxygenDiminishEvent -> (OxygenHandler(_:Player))
  )
}

class EventLoopListener(plug:JavaPlugin) extends Listener {
  import EventLoop.runnerMap
  plug.getServer().getPluginManager().registerEvents(this,plug)


  @EventHandler
  def onPlayerJoin(event:PlayerJoinEvent):Unit = {
    Events.values.foreach(e => try {runFromTimer(runnerMap(e)(event.getPlayer),plug,e)}catch{case err:Exception => println(s"Event ${e} Not yet implemented")})
  }
  @EventHandler
  def executeSavedComand(event:PlayerInteractEvent):Unit = try {
    val player = event.getPlayer
    val item = player.getInventory.getItemInMainHand
    val cmd = EventLoop.cmdmap.getOrElse((item.getType,player),"")
    if(cmd.size>0){
      player.sendMessage(s"Executing: $cmd")
      player.performCommand(cmd)
    }
  }catch {
    case e:Exception =>
      println(s"OnClick Exception ${event.getItem}")
  }
  @EventHandler(priority = EventPriority.LOW)
  def rocketExp(event:PlayerInteractEvent):Unit = try{
    if (
      event.getItem != null &&
        event.getItem.getType == Material.FIREWORK_ROCKET && event.getAction == Action.RIGHT_CLICK_AIR
    ) {
      event.getPlayer.giveExp(1)
    }
  }catch{
    case e:Exception =>
      println(s"Rocket Exception")
  }
  @EventHandler(priority = EventPriority.HIGH)
  def oxygenReplenish(event:PlayerInteractEvent): Unit = try{
    if (
      OxygenHandler.oxyconverters.contains(event.getClickedBlock.getType) &&
        event.getAction == Action.RIGHT_CLICK_BLOCK
    ) {
      OxygenHandler
        .update(
          event.getPlayer,
          OxygenHandler
            .getOrElse(event.getPlayer, 0) + OxygenHandler.SIPHON_AMT
        )
      event.getPlayer.sendMessage(s"${ChatColor.GREEN} + ${OxygenHandler.SIPHON_AMT} oxy")
      event.getClickedBlock.breakNaturally()
    }
  }catch{
    case e:Exception =>
      println(s"Oxygen Exception for ${event.getPlayer}")
  }
  @EventHandler
  def oxygenOnDamage(event:EntityDamageByEntityEvent):Unit = try {
    event.getDamager match {
      case player:Player =>
        player.sendMessage(s"${ChatColor.GREEN} +1 oxy")
        OxygenHandler.update(player,OxygenHandler.get(player) + 1)
    }
  }catch{
    case e:Exception =>
      println("Error while handling Oxygen replenish")
  }
  @EventHandler
  def removeItemGravity(event:PlayerDropItemEvent):Unit = try{
    event.getItemDrop.setGravity(false)
  }catch {
    case e:Exception => println("Error while setting item gravity")
  }
}

object Events extends Enumeration{
  type Events = Value
  val SpawnEvent,RocketChargeEvent,OxygenDiminishEvent,TerrainEvent = Value
  def addValue(str:String) = Value(str)
}

