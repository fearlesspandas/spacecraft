package minecraft.runnables.typicalModels

import java.time.LocalTime

import minecraft.runnables.typicalModels.PlayerEvents.{MonadicEvent, SpaceCraftPlayerEvent}
import Typical.core.grammar._
import org.bukkit.scheduler.BukkitTask
import Players._
import org.bukkit.{Bukkit, Location, Material}
import org.bukkit.block.{Block, BlockFace}
import org.bukkit.event.player.PlayerVelocityEvent
import org.bukkit.util.{Vector => LVector}
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.events.EventLoop._
import EventManager._
import org.bukkit.entity.Player
import org.bukkit.inventory.StonecutterInventory
import org.bukkit.potion.PotionEffectType
import minecraft.utils.Surroundings._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
object PlayerGravityModel {
  type Ticks = LocalTime
  type SeedBlocks = Seq[Block]
  case class PlayerGravityEvent(
                                 frequency:Double,
                                 probability:Double,
                                 name:String,
                                 knownBlocks:Set[Block],
                                 gravity:Double = 1,
                                 maxBlocks:Int = 1000,
                                 lastForceVector:LVector = new LVector(0,0,0),
                                 materialDensityMap:Map[Material,Double] = Material.values().map(m => m -> materialDensity(m)).toMap,
                                 value:Option[BukkitTask] = None
                               ) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      if(isStanding(player) || (!player.isOnline) || blocksBelow(knownBlocks)(player.getLocation(),15).nonEmpty || blocksAbove(knownBlocks)(player.getLocation(),10).nonEmpty){
        if(player.isOnline) {
          player.value.setGravity(true);
          val gravevent = event.asInstanceOf[PlayerGravityEvent]
          val newKNown = (gravevent.knownBlocks ++ expandBlockBlob(gravevent.knownBlocks)(blocksBelow(gravevent.knownBlocks)(player.getLocation,10).toSet)).filterNot(_==null).toSeq.sortWith((a,b) =>
              materialDensityMap(a.getType)/ a.getLocation().distance(player.getLocation) >
                materialDensityMap(b.getType) / b.getLocation().distance(player.getLocation)
          ).take(maxBlocks).toSet
          src.++[SpaceCraftPlayerEvent,PlayerGravityEvent](gravevent.copy(knownBlocks = newKNown))
        }else
          src
      }
      else {
        player.value.setGravity(false)
        if(player.isInsideVehicle){
          player.getVehicle.setGravity(false)
        }
        handleGravity(event,player,src)
      }

    }


    def handleGravity(event:SpaceCraftPlayerEvent, player:SpaceCraftPlayer,  src:dataset[SpaceCraftPlayer with SpaceCraftPlayer]):dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] = {
      val gravityEvent = event.asInstanceOf[PlayerGravityEvent]
      val gravityScaler = gravityEvent.gravity
      val searchCap = 50
      val prorata = 5
      val blocksInSight =
        player
        .getLineOfSight(Material.values().toSet.asJava,searchCap)
          .asScala
          .filterNot(
            b => b.getType.isAir || gravityEvent.knownBlocks.contains(b)
          )
      val onlinePlayers = Bukkit.getServer.getOnlinePlayers.asScala.toSeq.filterNot(_.getUniqueId == player.getUniqueId).sortWith(
        (a,b) =>
          a
          .getLocation()
            .distance(player.getLocation()) <
            b
              .getLocation()
              .distance(player.getLocation())
      ).take(10)
      val potentialBlocks = onlinePlayers.flatMap( p => {
        eventManager
          .getTask(p,this.name)
          .<--[SpaceCraftPlayerEvent]
          .biMap(
            _ => Set.empty[Block]
          )(
            d => d.get.asInstanceOf[PlayerGravityEvent].knownBlocks.take(4)
          )
      })
      //println(s"knownBlocks = ${gravityEvent.knownBlocks.size}")
      val playerlocation = player.getLocation.toVector

      val filteredBlocks = (
          potentialBlocks ++
          gravityEvent.knownBlocks ++
          expandBlockBlob(gravityEvent.knownBlocks)(gravityEvent.knownBlocks) ++
          expandBlockBlobDepthFirstSet(gravityEvent.knownBlocks)(gravityEvent.knownBlocks,20) ++
          expandBlockBlob(gravityEvent.knownBlocks)(blocksInSight.toSet)  ++
            //expandBlockBlob(
              getSurroundingBlocks(gravityEvent.knownBlocks)(player,searchCap,prorata).toSet
            //)
        )
      .toSeq.filterNot(_ == null).sortWith(sortByDensityAndDistance(materialDensityMap)(player.getLocation())
      ).take(maxBlocks).toSet

      //println(s"newKNownBocks:${filteredBlocks.size}")
      val forcevecs = filteredBlocks.foldLeft(new LVector(0,0,0))((accumVec,vec) => {
        val origBlockLoc = vec.getLocation().toVector
        val distance = origBlockLoc.distance(playerlocation)
        //println(s"distance:${distance}")
        val v  = origBlockLoc.subtract(playerlocation)
        val normal = v.normalize()
        //println(s"normal:${normal}")
        val scaler = -2*gravityScaler*(materialDensityMap(vec.getType))/(distance * distance)
        val res =
          if(distance< 10) new LVector(0,0,0) else
          normal.multiply(scaler)
        //println(s"res:${res}")
        res.add(accumVec)
      })

      val currentVelocity = player.getVelocity
      if(forcevecs.length() > 0){
        if(player.isInsideVehicle){
          val vehicle = player.getVehicle
          val vehicleVel = vehicle.getVelocity
          vehicle.setVelocity(vehicleVel.subtract(forcevecs))
        }else
          player.setVelocity(currentVelocity.subtract(forcevecs))
      }
     // println(s"updating gravity${forcevecs}")
      src ++[SpaceCraftPlayerEvent,PlayerGravityEvent] gravityEvent.copy(knownBlocks = filteredBlocks,lastForceVector = forcevecs) //++ player//.copy(postProcessing = afterEffects)
    }

  }

}
