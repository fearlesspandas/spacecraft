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
      if(isStanding(player) || (!player.isOnline) || blocksBelow(player.getLocation(),15).nonEmpty || blocksAbove(player.getLocation(),10).nonEmpty){
        if(player.isOnline) {
          player.value.setGravity(true);
          val gravevent = event.asInstanceOf[PlayerGravityEvent]
          val newKNown = (gravevent.knownBlocks ++ expandBlockBlob(blocksBelow(player.getLocation,10).toSet)).filterNot(_==null).toSeq.sortWith((a,b) =>
              materialDensityMap(a.getType)/ a.getLocation().distance(player.getLocation) >
                materialDensityMap(b.getType) / b.getLocation().distance(player.getLocation)
          ).take(maxBlocks).toSet
          src.++[SpaceCraftPlayerEvent,PlayerGravityEvent](gravevent.copy(knownBlocks = newKNown))
        }else
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
    val normals = BlockFace.values().map(_.getDirection)
    def expandBlockBlob(blocks:Set[Block],iterations:Int = 1):Set[Block] = {
      val foundBlocks = //(0 to iterations).foldLeft(blocks)((accumBlocks,_) => {
          blocks.flatMap(block => {
          (normals)
            .map(v => block.getLocation().add(v).getBlock)
            .filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
        })
      foundBlocks
    }
    def rotateVec(vec:LVector,angle:Double):LVector = {
      vec.rotateAroundX(angle).rotateAroundY(angle).rotateAroundZ(angle)
    }
    def expandBlockBlobDepthFirst(startingLoc:Block,dir:LVector,depth:Int = maxBlocks):Set[Block] = {
      val normal = dir
      //println(s"normal:${normal}")
       val foundBlocks = (0 to depth/2).map(x =>
         startingLoc.getLocation().add(normal.multiply(x*2)).getBlock
       )
      //println(s"foundBlocks:${foundBlocks.size},${foundBlocks.count(b => b.getType.isAir)}")
      foundBlocks.toSet.filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
    }
    def expandBlockBlobDepthFirstSet(blocks:Set[Block],maxDist:Int = maxBlocks):Set[Block] = {
      blocks.flatMap(b => normals.flatMap( n => expandBlockBlobDepthFirst(b,n,maxDist)))
    }

    def randomSearch(block:Location,radius:Int,samples:Int):Set[Block] = {
      val loc = block
      val randomNormals = () => (math.random*radius).floor.toInt
      (0 to samples).map(ind => {
        val (n1,n2,n3) = (randomNormals(),randomNormals(),randomNormals())
        loc.add(n1,n2,n3).getBlock
      }).toSet.filterNot(b => b.getType.isAir)
    }
    val fudge = (r:Int) => (math.random*r/2) * (if (math.random() > 0.5) 1 else -1).floor.toInt
    def blocksBelow(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(0,prorata*x + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))//.take(num)
    }
    def blocksAbove(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(0,prorata*x + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))//.take(num)
    }
    def blocksX(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(x/prorata + fudge(prorata),0,0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksMinusX(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(x* prorata + fudge(prorata),0,0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksZ(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(0,0,x* prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksMinusZ(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(0,0,x*prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksTopRight(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(x*prorata + fudge(prorata),x * prorata + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksBottomLeft(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(x*prorata+ fudge(prorata),x*prorata+ fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksBottomRight(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(0,x*prorata+ fudge(prorata),x*prorata+ fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksTopLeft(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(0,x*prorata+ fudge(prorata),x*prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksBackRight(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(x*prorata + fudge(prorata),0,x* prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksBackLeft(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.subtract(x * prorata+ fudge(prorata),0,x * prorata+ fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksUPUPUP(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap/prorata).map(x => loc.add(x,x,x).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def blocksDOWNDOWNDOWN(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
      (0 to cap).map(x => loc.subtract(x,x,x).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }


    def blocksInDir(loc:Location,x:Int,y:Int,z:Int,cap:Int = 100,prorata:Int = 4) = {
      (0 to cap/prorata).map(a => loc.subtract(x,y,z).multiply(a*prorata).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
    }
    def getSurroundingBlocks(player:Player,searchCap:Int,prorata:Int):Seq[Block] = {
      val funcs:Seq[(Int,Int) => Seq[Block]] = Seq(
      blocksBelow(player.getLocation.add(player.getVelocity),_:Int,_:Int),
        blocksAbove(player.getLocation.add(player.getVelocity),_:Int,_:Int),
        blocksX(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksMinusX(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksZ(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksMinusZ(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksTopRight(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksTopLeft(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksBottomRight(player.getLocation.add(player.getVelocity),_:Int,_:Int),
        blocksBottomLeft(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksBackRight(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
        blocksBackLeft(player.getLocation.add(player.getVelocity),_:Int,_:Int)
        )
      funcs.collect({case f if(math.random > 0.6) => f}).flatMap(f => f(searchCap,prorata))
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
      //println(s"knownBlocks = ${gravityEvent.knownBlocks.size}")
      val playerlocation = player.getLocation.toVector

      val filteredBlocks = (
          gravityEvent.knownBlocks ++
          expandBlockBlob(gravityEvent.knownBlocks) ++
          expandBlockBlobDepthFirstSet(gravityEvent.knownBlocks,20) ++
          expandBlockBlob(blocksInSight.toSet)  ++
            //expandBlockBlob(
              getSurroundingBlocks(player,searchCap,prorata).toSet
            //)
        )
      .toSeq.sortWith((a,b) =>
        (a !=null && b == null) ||
        materialDensityMap(a.getType)/ a.getLocation().distance(player.getLocation) >
          materialDensityMap(b.getType) / b.getLocation().distance(player.getLocation)
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
      if(forcevecs.length() > 0){player.setVelocity(currentVelocity.subtract(forcevecs))}
     // println(s"updating gravity${forcevecs}")
      src ++[SpaceCraftPlayerEvent,PlayerGravityEvent] gravityEvent.copy(knownBlocks = filteredBlocks,lastForceVector = forcevecs) //++ player//.copy(postProcessing = afterEffects)
    }

  }

}
