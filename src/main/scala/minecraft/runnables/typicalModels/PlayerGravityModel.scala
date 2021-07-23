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
                                 knownBlocks:Set[Block],
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
    val normals = Set(
      new LVector(1,0,0),
      new LVector(1,1,0),
      new LVector(1,0,1),
      new LVector(1,1,1),
      new LVector(1,-1,0),
      new LVector(1,0,-1),
      new LVector(0,-1,1)
    )
    def expandBlockBlob(blocks:Set[Block]):Set[Block] = {
      blocks.flatMap(block => {
        (normals ++ (normals.map(_.multiply(-1))))
          .map(v => block.getLocation().add(v).getBlock)
          .filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
      })
    }
    def handleGravity(event:SpaceCraftPlayerEvent,player:SpaceCraftPlayer,src:dataset[SpaceCraftPlayer with SpaceCraftPlayer]):dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] = {
      val gravityEvent = event.asInstanceOf[PlayerGravityEvent]
      player.sendMessage("~~~")
      val blocksInSight =
        player
        .getLineOfSight(Material.values().toSet.asJava,maxBlocks)
          .asScala
          .filterNot(
            b => b.getType.isAir || knownBlocks.contains(b)
          )

      //println(s"blocksInSight:${blocksInSight},${blocksInSight.size}")
      println(s"knownBlocks = ${gravityEvent.knownBlocks.size}")
      val playerlocation = player.getLocation.toVector
      val filteredBlocks = (gravityEvent.knownBlocks.filterNot(p => {
        val block = p
        val distance = block.getLocation().toVector.distance(playerlocation)
        distance > maxBlocks
      }) ++ expandBlockBlob(knownBlocks) ++ blocksInSight ).toSeq.sortWith(_.getLocation().distance(player.getLocation) < _.getLocation().distance(player.getLocation)).take(maxBlocks).toSet
      //update known blocks with blocks in sight

      val newKnownBlocks =
        filteredBlocks

      println(s"newKNownBocks:${newKnownBlocks.size}")
      val forcevecs = knownBlocks.map(vec => {
        val origBlockLoc = vec.getLocation().toVector
        val distance = origBlockLoc.distance(playerlocation)
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
