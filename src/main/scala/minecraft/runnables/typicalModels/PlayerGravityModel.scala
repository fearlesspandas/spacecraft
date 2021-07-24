package minecraft.runnables.typicalModels

import java.time.LocalTime

import minecraft.runnables.typicalModels.PlayerEvents.{MonadicEvent, SpaceCraftPlayerEvent}
import Typical.core.grammar._
import org.bukkit.scheduler.BukkitTask
import Players._
import org.bukkit.{Bukkit, Location, Material}
import org.bukkit.block.Block
import org.bukkit.event.player.PlayerVelocityEvent
import org.bukkit.util.{Vector => LVector}
import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.entity.Player
import org.bukkit.inventory.StonecutterInventory
import org.bukkit.potion.PotionEffectType

import scala.collection.JavaConverters._
object PlayerGravityModel {
  type Ticks = LocalTime
  case class PlayerGravityEvent(
                                 frequency:Double,
                                 probability:Double,
                                 knownBlocks:Set[Block],
                                 gravity:Double = 1,
                                 maxBlocks:Int = 1000,
                                 value:Option[BukkitTask] = None
                               ) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override val name: String = "PlayerGravityEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    val materials = Seq(Material.GLASS,Material.STONE,Material.DARK_OAK_WOOD,Material.BLACK_WOOL)

    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      if(isStanding(player) || (!player.isOnline) || blocksBelow(player.getLocation(),10).nonEmpty || blocksAbove(player.getLocation(),5).nonEmpty){
        if(player.isOnline) {
          player.value.setGravity(true);
          val gravevent = event.asInstanceOf[PlayerGravityEvent]
          val newKNown = gravevent.knownBlocks ++ expandBlockBlob(blocksBelow(player.getLocation,10).toSet)
          println(s"newKNownBlocks:${newKNown}")
          src.++[SpaceCraftPlayerEvent,PlayerGravityEvent](gravevent.copy(knownBlocks = newKNown))
        }else
          src
      }
      else {
        player.value.setGravity(false)
        handleGravity(event,player,src)
      }

    }

    def isConcrete(mat:Material):Boolean = {
      mat.toString.toUpperCase().contains("CONCRETE")
    }
    def isGlass(mat:Material):Boolean = {
      mat.toString.toUpperCase.contains("GLASS")
    }
    def isClay(mat:Material):Boolean = {
      mat.toString.toUpperCase().contains("TERRACOTTA")
    }
    import Material._
    def materialDensity(m:Material):Double= m match {
      case mat if mat.isFuel => 0.5
      case DIRT | COARSE_DIRT | SAND | RED_SAND => 1
      case STONE | BRICKS | NETHER_BRICK | CLAY | COBBLESTONE | SANDSTONE | SANDSTONE_WALL  => materialDensity(DIRT) * 2
      case COAL_ORE => materialDensity(STONE)/2
      case LAPIS_ORE => materialDensity(STONE) * 1.5
      case IRON_ORE => materialDensity(STONE) * 2
      case GOLD_ORE | SOUL_SAND => materialDensity(IRON_ORE) * 2
      case DIAMOND_ORE => materialDensity(GOLD_ORE) * 2
      case REDSTONE_ORE => materialDensity(DIAMOND_ORE) * 2
      case COAL_BLOCK => materialDensity(COAL_ORE) * 9
      case LAPIS_BLOCK => materialDensity(LAPIS_ORE) * 9
      case IRON_BLOCK => materialDensity(IRON_ORE)*9
      case GOLD_BLOCK => materialDensity(GOLD_ORE) * 9
      case DIAMOND_BLOCK => materialDensity(DIAMOND_ORE) * 9
      case REDSTONE_BLOCK => materialDensity(REDSTONE_ORE) * 64
      case OBSIDIAN => materialDensity(REDSTONE_BLOCK) * 10
      case BEDROCK => 50
      case _ if(isConcrete(m)) => materialDensity(STONE)
      case _ if(isClay(m)) => materialDensity(CLAY)
      case _ if(isGlass(m)) => materialDensity(SAND) * 1.5
      case _ if m.isSolid => 1
      case _ => 0


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
      new LVector(0,1,0),
      new LVector(0,0,1),
      new LVector(1,1,0),
      new LVector(1,0,1),
      new LVector(0,1,1),
      new LVector(1,1,1),
      new LVector(1,-1,0),
      new LVector(1,0,-1),
      new LVector(0,-1,1)
    )
    def expandBlockBlob(blocks:Set[Block],iterations:Int = 1):Set[Block] = {
      val foundBlocks = (0 to iterations).foldLeft(blocks)((accumBlocks,_) => {
        accumBlocks.flatMap(block => {
          (normals ++ (normals.map(_.multiply(-1))))
            .map(v => block.getLocation().add(v).getBlock)
            .filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
        })
      })
      //println(s"FoundBreadthFirst:${foundBlocks}")
      foundBlocks
    }
    def rotateVec(vec:LVector,angle:Double):LVector = {
      vec.rotateAroundX(angle).rotateAroundY(angle).rotateAroundZ(angle)
    }
    def expandBlockBlobDepthFirst(startingLoc:Block,dir:LVector,depth:Int = maxBlocks):Set[Block] = {
      val normal = dir
      //println(s"normal:${normal}")
       val foundBlocks = (0 to depth).flatMap(x => Seq(
         startingLoc.getLocation().add(normal.multiply(x)).getBlock,
         startingLoc.getLocation().subtract(normal.multiply(x)).getBlock
       )
       )
      //println(s"foundBlocks:${foundBlocks.size},${foundBlocks.count(b => b.getType.isAir)}")
      foundBlocks.toSet.filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
    }
    def expandBlockBlobDepthFirstSet(blocks:Set[Block],maxDist:Int = maxBlocks):Set[Block] = {
      blocks.flatMap(b => (normals ++ (normals.map(_.multiply(-1)))).flatMap( (n:LVector) => expandBlockBlobDepthFirst(b,n,maxDist)))
    }

    def randomSearch(block:Block,radius:Int,samples:Int):Set[Block] = {
      val loc = block.getLocation()
      val xadj = math.random * radius
      val yadj = math.random * radius
      val zadj = math.random * radius
      val randomNormals = () => normals.toSeq((math.random * normals.size).floor.toInt)
      (0 to samples).map(ind => {
        val (n1,n2,n3) = (randomNormals(),randomNormals(),randomNormals())
        loc.add(n1.multiply(xadj)).add(n2.multiply(yadj)).add(n3.multiply(zadj)).getBlock
      }).toSet.filterNot(b => b.getType.isAir)
    }
    def blocksBelow(loc:Location,cap:Int = 100):Seq[Block] = {
      (0 to cap).map(x => loc.subtract(0,x,0).getBlock).filterNot(_.getType.isAir)
    }
    def blocksAbove(loc:Location,cap:Int = 100):Seq[Block] = {
      (0 to cap).map(x => loc.add(0,x,0).getBlock).filterNot(_.getType.isAir)
    }
    def handleGravity(event:SpaceCraftPlayerEvent, player:SpaceCraftPlayer,  src:dataset[SpaceCraftPlayer with SpaceCraftPlayer]):dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] = {
      val gravityEvent = event.asInstanceOf[PlayerGravityEvent]
      val gravityScaler = gravityEvent.gravity
      val blocksInSight =
        player
        .getLineOfSight(Material.values().toSet.asJava,maxBlocks)
          .asScala
          .filterNot(
            b => b.getType.isAir || knownBlocks.contains(b)
          )
      println(s"knownBlocks = ${gravityEvent.knownBlocks.size}")
      val playerlocation = player.getLocation.toVector

      val filteredBlocks = (
          gravityEvent.knownBlocks ++
          expandBlockBlob(gravityEvent.knownBlocks) ++
          expandBlockBlobDepthFirstSet(gravityEvent.knownBlocks,30) ++
          expandBlockBlob(expandBlockBlobDepthFirstSet(blocksInSight.toSet,30))  ++
          blocksBelow(player.getLocation.add(player.getVelocity),30) ++
          randomSearch(player.getLocation.getBlock.getRelative(),30,100)
        ).toSeq.sortWith((a,b) =>
        materialDensity(a.getType)/ a.getLocation().distance(player.getLocation) >
          materialDensity(b.getType) / b.getLocation().distance(player.getLocation)
      ).take(maxBlocks).toSet
      //update known blocks with blocks in sight
      val newKnownBlocks =
        filteredBlocks
      println(s"newKNownBocks:${newKnownBlocks.size}")
      val forcevecs = filteredBlocks.map(vec => {
        val origBlockLoc = vec.getLocation().toVector
        val distance = origBlockLoc.distance(playerlocation)
        //println(s"distance:${distance}")
        val v  = origBlockLoc.subtract(playerlocation)
        val normal = v.normalize()
        //println(s"normal:${normal}")
        val scaler = -2*gravityScaler*(materialDensity(vec.getType))/(distance * distance)
        val res =
          if(distance< 10) new LVector(0,0,0) else
          normal.multiply(scaler)
        //println(s"res:${res}")
        res
      })
      println(s"forcevectors:${forcevecs.size}")
      val forcevec = if(forcevecs.size > 0) forcevecs.tail.foldLeft(forcevecs.head)((accumvec,v) => accumvec.add(v)) else new LVector(0,0,0)
      val currentVelocity = player.getVelocity
      if(forcevec.length() > 0){player.setVelocity(currentVelocity.subtract(forcevec))}
      player.sendMessage(s"gravityField:${forcevec.length()}")
      println(s"updating gravity${forcevec}")
      src ++[SpaceCraftPlayerEvent,PlayerGravityEvent] gravityEvent.copy(knownBlocks = newKnownBlocks) ++ player//.copy(postProcessing = afterEffects)
    }

  }

}
