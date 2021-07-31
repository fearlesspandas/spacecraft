package minecraft.utils

//import com.sk89q.worldedit.extent.clipboard.BlockVector3ckArrayClipboard
//import com.sk89q.worldedit.math.BlockVector3
//import com.sk89q.worldedit.regions.CuboidRegion
import org.bukkit.{Location, Material}
import org.bukkit.Material._
import org.bukkit.block.{Block, BlockFace}
import org.bukkit.entity.Player
import org.bukkit.util.{Vector => LVector}
object Surroundings {
  def isConcrete(mat:Material):Boolean = {
    mat.toString.toUpperCase().contains("CONCRETE")
  }
  def isGlass(mat:Material):Boolean = {
    mat.toString.toUpperCase.contains("GLASS")
  }
  def isClay(mat:Material):Boolean = {
    mat.toString.toUpperCase().contains("TERRACOTTA")
  }
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
  def expandBlockBlob(knownBlocks:Set[Block])(blocks:Set[Block],iterations:Int = 1):Set[Block] = {
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
  def expandBlockBlobDepthFirst(knownBlocks:Set[Block])(startingLoc:Block,dir:LVector,depth:Int):Set[Block] = {
    val normal = dir
    //println(s"normal:${normal}")
    val foundBlocks = (0 to depth/2).map(x =>
      startingLoc.getLocation().add(normal.multiply(x*2)).getBlock
    )
    //println(s"foundBlocks:${foundBlocks.size},${foundBlocks.count(b => b.getType.isAir)}")
    foundBlocks.toSet.filterNot(b => (b.getType.isAir || knownBlocks.contains(b)))
  }
  def expandBlockBlobDepthFirstSet(knownBlocks:Set[Block])(blocks:Set[Block],maxDist:Int):Set[Block] = {
    blocks.flatMap(b => normals.flatMap( n => expandBlockBlobDepthFirst(knownBlocks)(b,n,maxDist)))
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
  def blocksBelow(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(0,prorata*x + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))//.take(num)
  }
  def blocksAbove(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(0,prorata*x + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))//.take(num)
  }
  def blocksX(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(x/prorata + fudge(prorata),0,0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksMinusX(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(x* prorata + fudge(prorata),0,0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksZ(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(0,0,x* prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksMinusZ(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(0,0,x*prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksTopRight(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(x*prorata + fudge(prorata),x * prorata + fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksBottomLeft(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(x*prorata+ fudge(prorata),x*prorata+ fudge(prorata),0).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksBottomRight(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(0,x*prorata+ fudge(prorata),x*prorata+ fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksTopLeft(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(0,x*prorata+ fudge(prorata),x*prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksBackRight(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(x*prorata + fudge(prorata),0,x* prorata + fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksBackLeft(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.subtract(x * prorata+ fudge(prorata),0,x * prorata+ fudge(prorata)).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksUPUPUP(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap/prorata).map(x => loc.add(x,x,x).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def blocksDOWNDOWNDOWN(knownBlocks:Set[Block])(loc:Location,cap:Int = 100,prorata:Int = 4):Seq[Block] = {
    (0 to cap).map(x => loc.subtract(x,x,x).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }


  def blocksInDir(knownBlocks:Set[Block])(loc:Location,x:Int,y:Int,z:Int,cap:Int = 100,prorata:Int = 4) = {
    (0 to cap/prorata).map(a => loc.subtract(x,y,z).multiply(a*prorata).getBlock).filterNot(b => b.getType.isAir || knownBlocks.contains(b))
  }
  def getSurroundingBlocks(knownBlocks:Set[Block])(player:Player,searchCap:Int,prorata:Int):Seq[Block] = {
    val funcs:Seq[(Int,Int) => Seq[Block]] = Seq(
      blocksBelow(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int),
      blocksAbove(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int),
      blocksX(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksMinusX(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksZ(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksMinusZ(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksTopRight(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksTopLeft(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksBottomRight(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int),
      blocksBottomLeft(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksBackRight(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int) ,
      blocksBackLeft(knownBlocks)(player.getLocation.add(player.getVelocity),_:Int,_:Int)
    )
    funcs.collect({case f if(math.random > 0.6) => f}).flatMap(f => f(searchCap,prorata))
  }
  def sortByDensityAndDistance(materialDensityMap:Map[Material,Double])(src:Location)(a:Block,b:Block):Boolean = {
    materialDensityMap(a.getType)/ a.getLocation().distance(src) >
      materialDensityMap(b.getType) / b.getLocation().distance(src)
  }

  def getBox(loc:Location,size:Int):Seq[Block] = for{
    dx <- (0 until size)
    dz <- 0 until size
    y <- 0 until size
    blk = loc.add(new org.bukkit.util.Vector(loc.getBlockX + (dx - (size/2).floor.toInt),loc.getBlockY + y,loc.getBlockZ + (dz - (size/2).floor.toInt))).getBlock
    if !blk.getType.isAir
  }yield{
    blk
  }

//  def regionTEst = {
//    val v1 = new BlockVector3(0,0,0)
//    val v2 = new BlockVector3(10,10,10)
//    val cube = new CuboidRegion(v1,v2)
//    val blocks = new BlockArrayClipboard(cube)
//    blocks.getBlock(v2).getBlockType.getMaterial.getHardness
//  }
}
