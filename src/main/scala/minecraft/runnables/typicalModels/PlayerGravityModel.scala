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

import scala.collection.JavaConverters._
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
                                 value:Option[BukkitTask] = None
                               ) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
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
          val newKNown = (gravevent.knownBlocks ++ expandBlockBlob(blocksBelow(player.getLocation,10).toSet)).toSeq.sortWith((a,b) =>
            (a !=null && b == null) ||
              materialDensity(a.getType)/ a.getLocation().distance(player.getLocation) >
                materialDensity(b.getType) / b.getLocation().distance(player.getLocation)
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
      case OBSIDIAN =>   100
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
      println(s"knownBlocks = ${gravityEvent.knownBlocks.size}")
      val playerlocation = player.getLocation.toVector

      val filteredBlocks = (
          gravityEvent.knownBlocks ++
          expandBlockBlob(gravityEvent.knownBlocks) ++
          expandBlockBlobDepthFirstSet(gravityEvent.knownBlocks,30) ++
          expandBlockBlob(blocksInSight.toSet)  ++
            getSurroundingBlocks(player,searchCap,prorata)
        )//.toSet
              //)
            //)
      .toSeq.sortWith((a,b) =>
        (a !=null && b == null) ||
        materialDensity(a.getType)/ a.getLocation().distance(player.getLocation) >
          materialDensity(b.getType) / b.getLocation().distance(player.getLocation)
      ).take(maxBlocks).toSet
      //update known blocks with blocks in sight

      println(s"newKNownBocks:${filteredBlocks.size}")
      val forcevecs = filteredBlocks.foldLeft(new LVector(0,0,0))((accumVec,vec) => {
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
        res.add(accumVec)
      })

      val currentVelocity = player.getVelocity
      if(forcevecs.length() > 0){player.setVelocity(currentVelocity.subtract(forcevecs))}
      player.sendMessage(s"gravityField:${forcevecs.length()}")
      println(s"updating gravity${forcevecs}")
      src ++[SpaceCraftPlayerEvent,PlayerGravityEvent] gravityEvent.copy(knownBlocks = filteredBlocks) //++ player//.copy(postProcessing = afterEffects)
    }

  }

}
