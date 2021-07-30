package minecraft.events
import java.util.UUID

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.events.EventLoopTaskHandler.ActiveBoards
import minecraft.runnables.typicalModels.EntitySpawnModel
import minecraft.runnables.typicalModels.EntitySpawnModel.{BlazeSpawnEvent, DragonSpawnEvent, GhastSpawnEvent, PhantomSpawnEvent}
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.HeightReminder.HeightReminder
import minecraft.runnables.typicalModels.ItemGravityModel.ItemGravityEvent
import minecraft.runnables.typicalModels.OxygenModel.OxygenDepletionModel
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import minecraft.runnables.typicalModels.RocketChargeModel.RocketChargeModel
import org.bukkit.{Bukkit, Material}
import org.bukkit.entity.{EnderSignal, Phantom, Player, Vehicle}
import org.bukkit.event.block.{Action, BlockBreakEvent}
import org.bukkit.event.entity.{EntityDamageByEntityEvent, EntityDamageEvent, EntitySpawnEvent}
import org.bukkit.event.inventory.InventoryClickEvent
import org.bukkit.event.player.{PlayerDropItemEvent, PlayerInteractEntityEvent, PlayerInteractEvent, PlayerJoinEvent, PlayerRespawnEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scoreboard.{DisplaySlot, Score}
import org.spigotmc.event.player.PlayerSpawnLocationEvent
import minecraft.runnables.typicalModels.ListenerModel._
import minecraft.utils.MinecartController.MinecartControlModel
import minecraft.utils.{ItemCommands, MinecartController, Surroundings}
import org.bukkit.event.entity.EntityDamageEvent.DamageCause
import org.bukkit.event.vehicle.{VehicleDamageEvent, VehicleEnterEvent, VehicleExitEvent}
import org.bukkit.event.world.ChunkLoadEvent

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
object EventLoop {
  val oxyModel = OxygenDepletionModel(3,10000,None)
  val rocketChargeModel = RocketChargeModel(50000,None)
  val heightReminder = HeightReminder(5000)
  val oxyReplenishModel = OxygenReplenishEvent(3,None)
  val entitySpawnRate = 60000 * 5
  val dragonModel = DragonSpawnEvent(entitySpawnRate,0.000001)
  val ghastModel = GhastSpawnEvent(entitySpawnRate,0.002)
  val phantomModel = PhantomSpawnEvent(entitySpawnRate,0.05)
  val blazeModel = BlazeSpawnEvent(entitySpawnRate,0.07)
  val itemGravityEvent = ItemGravityEvent("ItemGravityEvent1",100,1,2,Seq())
  val itemGravityEvent2 = ItemGravityEvent("ItemGravityEvent2",100,1,2,Seq())
  val minecartController = MinecartControlModel(
    name = "MinecartControlModel",
    frequency = 200,
    probability = 1,
    speed = 0,
    inventorySnapshot = Seq(),
    waypoints = Seq()
  )
  val minecartController2 = MinecartControlModel(
    name = "MinecartControlModel2",
    frequency = 200,
    probability = 1,
    speed = 0,
    inventorySnapshot = Seq(),
    waypoints = Seq()
  )
  val gravityEvent = PlayerGravityEvent(
    frequency = 300,
    probability = 1,
    "PlayerGravityEvent1",
    knownBlocks = Set(),
    gravity = 2,
    maxBlocks = 300,
    value = None
  )
  val gravityEvent2 = PlayerGravityEvent(
    frequency = 300,
    probability = 1,
    "PlayerGravityEvent2",
    knownBlocks = Set(),
    gravity = 2,
    maxBlocks = 300,
    value = None
  )
  val gravityEvent3 = PlayerGravityEvent(
    frequency = 300,
    probability = 1,
    "PlayerGravityEvent3",
    knownBlocks = Set(),
    gravity = 2,
    maxBlocks = 300,
    value = None
  )
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
    gravityEvent,
    gravityEvent2,
    gravityEvent3,
    itemGravityEvent,
    itemGravityEvent2,
    minecartController
  ) //++ entitySpawnTasks



  def handleScore(player:Player, objectiveName:ActiveBoards, model:SpaceCraftPlayerEvent, deriveScore:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Double):Unit = {
    val scoreboard =
      Bukkit.getScoreboardManager.getMainScoreboard
    val maybeObjective = scoreboard.getObjective(objectiveName.name)
    val objective = if(maybeObjective == null) scoreboard.registerNewObjective(s"${objectiveName.name}","dummy",s"${objectiveName.name}") else maybeObjective
    //println(s"registeringObjective:${objective != null}")
    for{
      em <- eventManager
    }yield{
      val dat = em.getTask(player,model.name)
      //println(s"taskplayer:${datplayer}")
      val oxyremaining = deriveScore(dat)

      //println(s"data intact:${if(dat.isEmpty) dat else !dat.isEmpty}")
      objective.getScore(player.getDisplayName).setScore((oxyremaining).floor.toInt)
      em
    }

  }
  implicit val serializer:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Unit = src =>
      //if(event.isInstanceOf[PlayerGravityEvent]) println(s"serialized blocks:${event.asInstanceOf[PlayerGravityEvent].knownBlocks}")
      for{
        player <- src.player
        event <- src.<--[SpaceCraftPlayerEvent]
        em <- eventManager
      }yield {
        em.eventStack.push(player.postProcessing)
        em.value.update((event.name,player.getUniqueId),src)
        em
      }




  class EventLoopListener(plug:JavaPlugin) extends Listener {
    plug.getServer().getPluginManager().registerEvents(this,plug)
    @EventHandler
    def onPlayerJoin(event:PlayerJoinEvent):Unit = {
     event.getPlayer.sendMessage("Welcome to space")
      event.getPlayer.setGravity(false)
      val newplayer = SpaceCraftPlayer(event.getPlayer,100)
      EntitySpawnModel.fixEntityFlight(newplayer)
      baseTasks.foreach(e => {
        val spcplayer = eventManager.getTask(event.getPlayer,e.name).player.fold(_ => newplayer)(d => d).get
        val spcevent = eventManager.getTask(event.getPlayer,e.name).<--[SpaceCraftPlayerEvent].fold[SpaceCraftPlayerEvent](_ => e)(ev => ev).get
        eventManager.updateEvent(spcevent,spcplayer,plug);
        Thread.sleep(150)
      })
    }

    @EventHandler
    def crashDamageHandler(event:EntityDamageEvent):Unit = event.getCause match {
      case DamageCause.FLY_INTO_WALL =>
        event.getEntity match {
          case p:Player if(p.getInventory.getBoots.getType == Material.DIAMOND_BOOTS) => event.setCancelled(true)
          case _ => ()
        }
      case DamageCause.VOID => event.setCancelled(true)
      case _ => ()

    }
    @EventHandler
    def onPlayerSpawn(event:PlayerRespawnEvent):Unit = {
      EntitySpawnModel.fixEntityFlight(event.getPlayer)
      for{
        em <- eventManager
        spcevent <- eventManager.getTask(event.getPlayer,oxyModel.name).<--[SpaceCraftPlayerEvent]
        spcplayer <- em.getTask(event.getPlayer,oxyModel.name).player
      }yield {
        val oxym = spcevent.asInstanceOf[OxygenDepletionModel]
        eventManager.updateEvent(oxym,spcplayer,plug,OxygenReplenishEvent(100 - spcplayer.oxygenRemaining))
      }
    }
    @EventHandler
    def floatItems(event:PlayerDropItemEvent):Unit = {
      val player = event.getPlayer
      if (!player.isOnGround){
        event.getItemDrop.setGravity(false);
        event.getItemDrop.setGlowing(true)
      val ievent = Seq(itemGravityEvent,itemGravityEvent2)((math.random * 2).floor.toInt)
      for{
        em <- eventManager
        task <- em.getTask(player,ievent.name).<--[SpaceCraftPlayerEvent]
        spcPlayer <- em.getTask(player,ievent.name).player
      }yield {
        val gravTask = task.asInstanceOf[ItemGravityEvent]
        val updatedTask = task.asInstanceOf[ItemGravityEvent].copy(items = event.getItemDrop +: gravTask.items )
        em.updateEvent(updatedTask,spcPlayer,plug)
      }
      }
    }
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
        case m if(m.isFuel) =>
          for {
            em <- eventManager
            spcplayer <- em.value.getOrElse((oxyModel.name, player.getUniqueId), throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
          } yield eventManager.updateEvent(oxyModel, spcplayer, plug, oxyReplenishModel)
        case Material.BLUE_ICE | Material.ICE | Material.FROSTED_ICE =>
          for {
            em <- eventManager
            spcplayer <- em.value.getOrElse((oxyModel.name, player.getUniqueId), throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
          } yield eventManager.updateEvent(oxyModel, spcplayer, plug, oxyReplenishModel)
        case _ => ()
      }
    }
    @EventHandler
    def itemCommandHandler(event:PlayerInteractEvent) = event.getAction match {
      case Action.RIGHT_CLICK_AIR if(!event.getPlayer.isInsideVehicle) =>
        val item = event.getItem.getType
        ItemCommands.runCommand(event.getPlayer,item)

      case _ => ()
    }
    @EventHandler
    def enterMinecart(event:VehicleEnterEvent):Unit = event.getEntered match {
      case player: Player =>
        for {
          spcplayer <- eventManager.getTask(player,minecartController.name).player
          task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
        }yield {
          val mctask = task.asInstanceOf[MinecartControlModel]
          val inventory = player.getInventory.getContents
          MinecartController.setInventoryControls(player)
          eventManager.updateEvent(mctask.copy(inventorySnapshot = inventory),spcplayer,plug)
        }
      case _ => ()
    }
    @EventHandler
    def exitMinecart(event:VehicleExitEvent):Unit = event.getExited match {
      case player: Player =>
        for {
          spcplayer <- eventManager.getTask(player,minecartController.name).player
          task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
        }yield {
          val mctask = task.asInstanceOf[MinecartControlModel]
          player.getInventory.setContents(mctask.inventorySnapshot.toArray)
          player.setCanPickupItems(true)
          eventManager.updateEvent(mctask.copy(probability = 0),spcplayer,plug)
        }
      case _ => ()
    }
    @EventHandler
    def addMinecartToConvoy(event:VehicleDamageEvent):Unit = event.getVehicle match {
      case v:Vehicle =>
        val player = event.getAttacker.asInstanceOf[Player]
        if (!player.isOnGround){
          val ievent = Seq(itemGravityEvent,itemGravityEvent2)((math.random * 2).floor.toInt)
          for{
            em <- eventManager
            task <- em.getTask(player,ievent.name).<--[SpaceCraftPlayerEvent]
            spcPlayer <- em.getTask(player,ievent.name).player
          }yield {
            val gravTask = task.asInstanceOf[ItemGravityEvent]
            val updatedTask = task.asInstanceOf[ItemGravityEvent].copy(items = v +: gravTask.items )
            em.updateEvent(updatedTask,spcPlayer,plug)
          }
    }
    }
    @EventHandler
    def fillShip(event:PlayerInteractEntityEvent):Unit = {
      val player = event.getPlayer
      event.getRightClicked match {
        case v:Vehicle =>
          player.getInventory.getItemInMainHand.getType match {
            case Material.COAL_BLOCK =>
              val updatedInv = player.getInventory.getItemInMainHand
              updatedInv.setAmount(1)
              player.getInventory.remove(updatedInv)
              for {
                spcplayer <- eventManager.getTask(player,minecartController.name).player
                task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
              }yield {
                val mctask = task.asInstanceOf[MinecartControlModel]
                eventManager.updateEvent(mctask.addFuel(v,100),spcplayer,plug)
              }
          }

      }
    }
    //every time a chunk loads all nearby players will be potentially affected
//    def loadGravityFromChunkLoad(event:ChunkLoadEvent):Unit = {
//      val chunk = event.getChunk
//      val pseudoOrigin = new org.bukkit.util.Vector(chunk.getX,0,chunk.getZ)
//      val prob = 0.3
//      val max = 10
//      val chunkblocks =
//        (for{
//          _ <- Seq()
//          if math.random() < prob
//        dx <- 0 to 16
//        if math.random() < prob
//        dz <- 0 to 16
//        if math.random() < prob
//        y <- 0 to 255
//        block = chunk.getBlock(chunk.getX + dx,y,chunk.getZ + dz)
//        if math.random() < prob
//          if !block.getType.isAir
//      }yield {
//        block
//      }).sortWith(
//          (a,b) => gravityEvent.materialDensityMap(a.getType) < gravityEvent.materialDensityMap(b.getType)
//        ).toSet.take(10)
//      Bukkit
//        .getServer
//        .getOnlinePlayers
//        .asScala
//        .filter(p => p.getLocation().distance(pseudoOrigin.toLocation(chunk.getWorld)) < 100)
//        .foreach(p => for{
//          em <- eventManager
//          task <- em.getTask(p,gravityEvent.name)
//          spcplayer <- em.getTask(p,gravityEvent.name).player
//        }yield{
//          val gravTask = task.asInstanceOf[PlayerGravityEvent]
//          //potentially check for overall mass in comparison to already known blocks
//          //so that we can avoid canceling and restarting the tasks when there's a negligible
//          //affect. Although this is why we have multiple threads to be fair
//          em.updateEvent(gravTask.copy(knownBlocks = chunkblocks ++ gravTask.knownBlocks),spcplayer,plug)
//        })
    //}
  }

}



