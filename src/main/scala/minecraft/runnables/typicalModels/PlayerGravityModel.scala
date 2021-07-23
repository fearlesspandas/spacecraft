package minecraft.runnables.typicalModels

import java.time.LocalTime

import minecraft.runnables.typicalModels.PlayerEvents.{MonadicEvent, SpaceCraftPlayerEvent}
import Typical.core.grammar._
import org.bukkit.scheduler.BukkitTask
import Players._
import org.bukkit.{Location, Material}
import org.bukkit.block.Block
import org.bukkit.event.player.PlayerVelocityEvent
import org.bukkit.util.{Vector => LVector}
import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.entity.Player

import scala.collection.JavaConverters._
object PlayerGravityModel {
  type Ticks = LocalTime
  case class PlayerGravityEvent(
                                 frequency:Double,
                                 probability:Double  = 1,
                                 knownBlocks:Set[(Block,Ticks)],
                                 gravity:Double = 1,
                                 maxBlocks:Int = 1000,
                                 maxTime:Int = 3,
                                 value:Option[BukkitTask] = None
                               ) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
    override val name: String = "PlayerGravityEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    val materials = Seq(Material.GLASS,Material.STONE,Material.DARK_OAK_WOOD,Material.BLACK_WOOL)

    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      if(isStanding(player) || (!player.isOnline)) {
        if(player.isOnline) {player.value.setGravity(true)}
        src
      }
      else {
        player.value.setGravity(false)
        handleGravity(event,player,src)
      }

    }

    def isStanding(player:Player):Boolean = {
      !(player.getLocation().subtract(0,1,0).getBlock.getType.isAir &&
        player.getLocation().subtract(1,1,0).getBlock.getType.isAir &&
        player.getLocation().subtract(0,1,1).getBlock.getType.isAir &&
        player.getLocation().subtract(1,1,1).getBlock.getType.isAir &&
        player.getLocation().subtract(-1,1,-1).getBlock.getType.isAir &&
        player.getLocation().subtract(0,1,-1).getBlock.getType.isAir &&
        player.getLocation().subtract(-1,1,0).getBlock.getType.isAir &&
        player.getLocation().subtract(-1,1,1).getBlock.getType.isAir &&
        player.getLocation().subtract(1,1,-1).getBlock.getType.isAir
        )
    }
    def handleGravity(event:SpaceCraftPlayerEvent,player:SpaceCraftPlayer,src:dataset[SpaceCraftPlayer with SpaceCraftPlayer]):dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] = {
      val gravityEvent = event.asInstanceOf[PlayerGravityEvent]
      //player.getWorld.getChunkAt(0,0).get
      val blocksInSight =
        player
        .getLineOfSight(Material.values().toSet.asJava,100)
          .asScala
          .filterNot(
            b => b.getType.isAir || knownBlocks.exists(b2 => b == b2._1)
          )
          .map((_,LocalTime.now()))
      //println(s"blocksInSight:${blocksInSight},${blocksInSight.size}")
      println(s"knownBlocks = ${gravityEvent.knownBlocks}")
      val filteredBlocks = gravityEvent.knownBlocks.filterNot(p => p._2.plusSeconds(maxTime).isBefore(LocalTime.now()))
      //update known blocks with blocks in sight
      val newKnownBlocks =
        if(filteredBlocks.size < maxBlocks)
          filteredBlocks  ++ (
            blocksInSight
              .dropRight(
                math.max(blocksInSight.size - (maxBlocks - filteredBlocks.size),0)
              ).toSet
            )
        else filteredBlocks

      println(s"newKNownBocks:${newKnownBlocks.size}")
      //add vectors for all known blocks
      val diffvectors:Set[Block] = knownBlocks.map(b => b._1)
      println(s"diffVectors:${diffvectors.size}")
      val playerlocation = player.getLocation.toVector
      val forcevecs = diffvectors.map(vec => {
        val origBlockLoc = vec.getLocation().toVector
        val distance = origBlockLoc.distance(playerlocation)
        player.sendMessage(s"Moving towards:${origBlockLoc}")
        println(s"distance:${distance}")
        val v  = origBlockLoc.subtract(playerlocation)
        val normal = v.normalize()
        println(s"normal:${normal}")
        val scaler = -2*gravity/(distance * distance)
        val res = if(distance< 3) new LVector(0,0,0) else normal.multiply(scaler)
        println(s"res:${res}")
        res
      })
      println(s"forcevectors:${forcevecs.size}")
      val forcevec = if(forcevecs.size > 0) forcevecs.tail.foldLeft(forcevecs.head)((accumvec,v) => accumvec.add(v)) else new LVector(0,0,0)
      val currentVelocity = player.getVelocity
      player.setVelocity(currentVelocity.subtract(forcevec))
      println(s"updating gravity${forcevec}")
      src ++[SpaceCraftPlayerEvent,PlayerGravityEvent] gravityEvent.copy(knownBlocks = newKnownBlocks) ++ player//.copy(postProcessing = afterEffects)
    }

  }

}
