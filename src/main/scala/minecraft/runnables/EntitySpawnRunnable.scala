package minecraft.runnables

import events.Events.Events
import events.{EventLoop, Events}
import minecraft.IO.SettingsDecoder
import minecraft.runnables.EntitySpawnRunnable.buildRandomEvent
import minecraft.runnables.SpaceCraftRunnable.{CommandProcessor, TabComplete}
import org.bukkit.block.Block
import org.bukkit.entity.EnderDragon.Phase
import org.bukkit.{ChatColor, Color, Location, Material, World, entity}
import org.bukkit.entity.{EnderDragon, Entity, EntityType, Flying, Ghast, Phantom, Player}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet
import scala.math.random
class EntitySpawnRunnable(override val player:Player) extends SpaceCraftRunnable {

  override val eventType = Events.SpawnEvent

  def runner(): Unit = if (player.isGliding) {
    val entity: () => EntityType = () => EntitySpawnRunnable.entityPool((scala.math.random * EntitySpawnRunnable.entityPool.size).floor.toInt)
    //val loot = EntitySpawnRunnable.loot((scala.math.random * EntitySpawnRunnable.loot.size).floor.toInt)
    val radius = EntitySpawnRunnable.MAX_LOOT_RADIUS
    player.sendMessage(s"${ChatColor.DARK_GREEN} Monsters are Attacking!")
    val world = player.getWorld()
    val centerblok = player.getTargetBlock(Set(Material.AIR).asJava, (scala.math.random * EntitySpawnRunnable.MAX_DIST).floor.toInt)
    (0 until (EntitySpawnRunnable.MAX_SWARM * scala.math.random()).ceil.toInt).foreach(_ =>
      world.spawnEntity(centerblok.getLocation().add(0,radius,0),entity())
    )
    val nummobs,lootnum,bossbatchsize = (random() * 4).toInt
    buildRandomEvent(radius,nummobs,bossbatchsize,30,lootnum)(centerblok.getLocation(),world,player)
    //EntitySpawnRunnable.buildRandomSphere(EntitySpawnRunnable.spawnFieldProbs,centerblok.getLocation(),50, 101)(world)
  }
}
object EntitySpawnRunnable extends RunnableCompanion[EntitySpawnRunnable] {
  type SpawnableMap = scala.collection.mutable.Map[Either[Material,EntityType],Double]
//  val eventpool = (location:Location,world:World,player:Player)=> Seq(
//    () => buildRandomSphere(EntitySpawnRunnable.spawnFieldProbs,location,50, 101)(world),
//    () => buildRandomEvent(30,2,3,40,4)(location,world,player)
//  )
  val entityPool = Seq(EntityType.PHANTOM,EntityType.GHAST,EntityType.BLAZE,EntityType.LIGHTNING)
  val loot = Seq(Material.DIAMOND_BLOCK,Material.EMERALD_BLOCK,Material.ENCHANTING_TABLE,Material.END_PORTAL_FRAME)
  val normals = Seq(
    (1,0,0),
    (0,1,0),
    (0,0,1),
    (-1,0,0),
    (0,-1,0),
    (0,0,-1)
  )
  val spawnFieldProbs:SpawnableMap = scala.collection.mutable.Map(
    Left(Material.BLUE_ICE) -> 1.0,
    Left(Material.MELON) -> 0.3,
    Left(Material.COBWEB) -> 0.9,
    Left(Material.RED_MUSHROOM_BLOCK) -> 0.3,
    Left(Material.BROWN_MUSHROOM_BLOCK) -> 0.3,
    Left(Material.LAPIS_BLOCK) -> 0.08,
    Left(Material.IRON_BLOCK) -> 0.08,
    Left(Material.SEA_LANTERN) -> 1.0 ,
    Right(EntityType.ENDER_DRAGON) -> 0.05,
    Right(EntityType.SKELETON) -> 0.5,
    Right(EntityType.LIGHTNING) -> 0.5,
    Right(EntityType.FIREBALL) -> 0.1,
    Left(Material.SWEET_BERRY_BUSH) -> 0.5,
    Left(Material.OBSIDIAN) -> 0.3
  ) ++ entityPool.map(Right(_) -> 0.05).toMap ++ loot.map(Left(_) -> 0.05).toMap //++ readSettings
  val MAX_SWARM=10
  val MAX_DIST=30
  var MAX_LOOT_RADIUS=20
  var MAX_FIELD_RADIUS = 100



  def apply(player:Player) = new EntitySpawnRunnable(player)

//  val buildOutCommandProcessor : CommandProcessor = (sender,cmd,_,args) => cmd.getName match{
//    case ""
//  }
  case class genInfo(lastgen:Seq[Block],total:HashSet[Location])
  def buildRecursive(locations:Seq[Block], num:Int, mat:Material):Unit = {
    (0 until num).foldLeft(genInfo(locations,HashSet()))((info, _) => {
      info.lastgen.foreach(_.setType(mat))
      val nextgen = info.lastgen.flatMap(block => normals.map(v => block.getLocation.add(v._1,v._2,v._3).getBlock))
          .filterNot(block => info.total.exists(l => block.getLocation().distance(l) == 0))
      genInfo(nextgen,info.total ++ info.lastgen.map(_.getLocation()).toSet[Location])
    })
  }

  val randomSphereCommandProcessor:CommandProcessor = (sender,cmd,_,args) => cmd.getName match {
    case "spawnevent_random" =>
      args.size match {
        case num if num >=2 =>
          val size = args(0).toInt
          val iterations = args(1).toInt
          val player = sender match {case p:Player => p}
          EntitySpawnRunnable.buildRandomSphere(
            EntitySpawnRunnable.spawnFieldProbs,
            player.getLocation,
            size,
            iterations
          )(player.getWorld)
          true
        case _ => true
      }
    case _ => true
  }
  val randomSphereTabComplete:TabComplete = (sender,cmd,_,args) => cmd.getName match {
    case "spawnevent_random" =>
      args.size match {
        case 1 => List("<radius>")
        case 2 => List("iterations:Int")
        case _ => List()
      }
    case _ => List()
  }
  def buildRandomSphere(probMap:SpawnableMap,centerblock:Location,radius:Int,iterations:Int = 1)(implicit world:World):Unit = {
    (0 until iterations).foreach(_ => probMap.keys.foreach(key => {
      val x, y, z = random() * radius * (if ((random() * 2).toInt == 0) 1 else -1)
      key match {
        case Left(m:Material) if random() <= probMap(Left(m)) =>
          centerblock.add(x, y, z).getBlock.setType(m)
        case Right(e:EntityType) if random() <= probMap(Right(e)) =>
          val spawnpoint = centerblock.add(x, y, z)
          world.spawnEntity(spawnpoint,e).postSpawn()
        case _ =>
      }
    }))
  }
  def randomOffset(radius:Int) = random() * radius * (if ((random() * 2).toInt == 0) 1 else -1)


  implicit class EntityHandler(e:Entity){
    val postSpawn = () => {
      e.setGlowing(true)
      e match {
        case d:EnderDragon => d.fixFlight()
        case p:Phantom => p.fixFlight()
        case _ =>
      }
    }
  }
  implicit class DragonHandler(e:EnderDragon){
    val fixFlight = () => e.setPhase(Phase.SEARCH_FOR_BREATH_ATTACK_TARGET)
  }
  implicit class PhantomHandler(e:Phantom){
    val fixFlight = () => {
      e.setSize(e.getSize * 3)
      e.setVelocity(e.getVelocity.multiply(3))
    }
  }
  val randomEventCommandProcessor:CommandProcessor = (sender,cmd,_,args) => cmd.getName match {
    case "spawnevent_theme" =>
      args.size match {
        case num if num >=5 =>
          val radius = args(0).toInt
          val nummobs = args(1).toInt
          val bossbatchsize = args(2).toInt
          val lootsize = args(3).toInt
          val lootnum = args(4).toInt
          val player = sender match {case p:Player => p}
          EntitySpawnRunnable.buildRandomEvent(
            radius,
            nummobs,
            bossbatchsize,
            lootsize,
            lootnum
          )(player.getLocation,player.getWorld,player)
          true
        case _ => true
      }
    case _ => true
  }
  val randomEventTabComplete:TabComplete = (sender,cmd,_,args) => cmd.getName match {
    case "spawnevent_theme" =>
      args.size match {
        case 1 => List("<radius>")
        case 2 => List("mobnum:Int")
        case 3 => List("bossbatchsize:Int")
        case 4 => List("lootsize:Int")
        case 5 => List("lootnum:Int")
        case _ => List()
      }
    case _ => List()
  }
  def buildRandomEvent(radius:Int, nummobs:Int, bossbatchsize:Int, lootsize:Int, lootnum:Int)(implicit location:Location, world:World, player:Player):Unit = {
    //import scala.math.random
    val eventMobs = eventMap.keys.toSeq
    val mobs = (0 until nummobs).map(_ =>
      eventMobs((eventMobs.size * random()).toInt)
    )
    mobs.foreach(mob =>{
      val entity = world.spawnEntity(
        location.add(
          randomOffset(radius),
          randomOffset(radius),
          randomOffset(radius)
        ),
        mob
      ).postSpawn()

    }
    )
    val mobboss = eventMobs((eventMobs.size * random()).toInt)
    (0 until bossbatchsize).foreach(_ =>
       world.spawnEntity(
        location.add(
          randomOffset(radius),
          randomOffset(radius),
          randomOffset(radius)
        ),
        mobboss
      ).postSpawn()
    )
    (0 until bossbatchsize).foreach(_ =>
      world.spawnEntity(
        location.add(
          randomOffset(radius),
          randomOffset(radius),
          randomOffset(radius)
        ),
        mobboss
      ).postSpawn()
    )
    val mobtext = mobs.foldLeft(mobboss.toString)(_ + "'s, " +  _.toString)
    player.sendMessage(s"$mobtext are attacking")
    val loot = eventMap(mobboss)
    val rewards = (0 until lootnum).map(_ => loot((loot.size * random).toInt))
    val bigreward = loot((loot.size * random).toInt)
    rewards.foreach(item => {
      (0 until lootsize).foreach(_ =>
        location.add(
          randomOffset(radius),
          randomOffset(radius),
          randomOffset(radius)
        ).getBlock.setType(item)
      )
    })
      (0 until 4).foreach( _ => {
        val randomloc = location.add(
          randomOffset(radius),
          randomOffset(radius),
          randomOffset(radius)
        ).getBlock
        buildRecursive(Seq(randomloc),3,bigreward)
      }
      )


  }
  val eventMap : Map[EntityType,Seq[Material]] = Map(
    EntityType.ENDER_DRAGON -> Seq(
      Material.DIAMOND_BLOCK,
      Material.ENDER_CHEST,
      Material.ENCHANTING_TABLE,
      Material.BLUE_ICE,
      Material.REDSTONE_BLOCK,
      Material.SEA_LANTERN,
      Material.OBSIDIAN,
      Material.END_PORTAL_FRAME
    ),
    EntityType.GHAST -> Seq(
      Material.GLOWSTONE,
      Material.MAGMA_BLOCK,
      Material.NETHERRACK,
      Material.BROWN_MUSHROOM_BLOCK,
      Material.RED_MUSHROOM_BLOCK,
      Material.REDSTONE_BLOCK,
      Material.BLUE_ICE,
      Material.OBSIDIAN
    ),
    EntityType.BLAZE -> Seq(
      Material.MAGMA_BLOCK,
      Material.GLOWSTONE,
      Material.REDSTONE_BLOCK,
      Material.IRON_BLOCK,
      Material.MELON,
      Material.BLUE_ICE,
      Material.LAPIS_BLOCK,
      Material.GOLD_BLOCK
    ),
    EntityType.PHANTOM -> Seq(
      Material.SEA_LANTERN,
      Material.MELON,
      Material.RED_MUSHROOM_BLOCK,
      Material.BROWN_MUSHROOM_BLOCK,
      Material.HAY_BLOCK
    )
  )

//  import SettingsDecoder._
//  import scala.collection._
//  def readSettings:mutable.Map[Either[Material,EntityType],Double] = SettingsDecoder
//    .readSettings[Material,Double](s"plugins/spacecraft/${Events.SpawnEvent}/probabilities.json")("".processMaterial(_),"".processDouble(_))
//    .map({
//      case (k,v) => (Left(k),v)
//  })

//  def main(args:Array[String]):Unit = {
//
//  }
}