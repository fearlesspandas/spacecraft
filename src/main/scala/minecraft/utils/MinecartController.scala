package minecraft.utils

import java.util.UUID

import Typical.core.dataset
import org.bukkit.entity.{Entity, Player}

import scala.collection.JavaConverters._
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.Players._
import org.bukkit.scheduler.BukkitTask
import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.{Location, Material}
import org.bukkit.inventory.ItemStack
object MinecartController {

  case class MinecartControlModel(
                                   name:String,
                                   frequency:Double,
                                   probability:Double,
                                   speed:Double,
                                   inventorySnapshot:Seq[ItemStack],
                                   waypoints:Seq[Location],
                                   fuel:Map[UUID,Double] = Map(),
                                   value:Option[BukkitTask]= None
                                 ) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(src: dataset.dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset.dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] =
      for{
        player <- src.player
        event <- src.<--[SpaceCraftPlayerEvent]
      }yield{
        val cartModel = event.asInstanceOf[MinecartControlModel]
        val inventory = player.getInventory
        val mainHand = inventory.getItemInMainHand.getType
        val offHand = inventory.getItemInOffHand.getType
        if(player.isInsideVehicle){
          val vehicle = player.getVehicle
          if(Surroundings.blocksBelow(Set())(player.getLocation(),5).nonEmpty){
            if(vehicle.isGlowing) vehicle.setGlowing(false)
            if(!vehicle.hasGravity) vehicle.setGravity(true)
          }else{
            if(vehicle.hasGravity)vehicle.setGravity(false)
            if(!vehicle.isGlowing)vehicle.setGlowing(true)
          }
        }
        handleControls(offHand)(src)(cartModel,player,mainHand)

      }
    def addFuel(entity:Entity,amt:Double):MinecartControlModel = this.copy(fuel = fuel ++ Map(entity.getUniqueId -> (fuel(entity.getUniqueId) + amt)))

  }
  def setInventoryControls(player:Player):Unit ={
    val inventory = player.getInventory
    player.setCanPickupItems(false)
    val prehelmet = inventory.getArmorContents
    inventory.setContents(Array())
    inventory.setArmorContents(prehelmet)
    inventory.setItemInOffHand(new ItemStack(Material.FIREWORK_ROCKET))
    inventory.setItem(0,new ItemStack(Material.FIREWORK_ROCKET))
    inventory.setItem(2,new ItemStack(Material.IRON_SHOVEL))
    inventory.setItem(1,new ItemStack(Material.ARROW))
    inventory.setItem(3,new ItemStack(Material.COMPASS))
  }
  def moveCart(player:Player,speed:Double,target:Option[Location] = None):Unit = if(player.isInsideVehicle){
    val vehicle = player.getVehicle
    val block = player.getLineOfSight(Material.values().toSet.asJava,100).asScala.sortWith((a,b) => {
      a.getLocation().distance(player.getLocation()) > b.getLocation().distance(player.getLocation())
    }).head
    import org.bukkit.util.Vector
    val boundedBlock = if(block.getY < 0) new Vector(block.getX,0,block.getZ) else if(block.getY > 300) new Vector(block.getX,300,block.getZ) else block.getLocation().toVector
    val dir =
      target
        .map(_.toVector.subtract(player.getLocation.toVector).normalize().multiply(speed))
      .getOrElse(boundedBlock.subtract(player.getLocation.toVector).normalize().multiply(speed))
    val currVel = vehicle.getVelocity
    vehicle.setVelocity(currVel.add(dir))
    vehicle.setRotation(player.getLocation().getPitch,player.getLocation.getYaw)
  }
  def handleControls(
                      offHand:Material
                    )(
                      src:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]
                    )(
                      cartModel:MinecartControlModel,
                      player:Player,
                      mainHand:Material
                    ) = offHand match {
    case Material.FIREWORK_ROCKET =>
      handleAccelerationControls(src)(cartModel,player,mainHand,None)
    case Material.COMPASS =>
     if(cartModel.waypoints.headOption.exists(player.getLocation.distance(_) < 20))
      {src ++[SpaceCraftPlayerEvent,MinecartControlModel] cartModel.copy(waypoints = cartModel.waypoints.tail)}
      else if(cartModel.waypoints.isEmpty) src
     else handleAccelerationControls(src)(cartModel,player,mainHand,cartModel.waypoints.headOption)

  }
  def handleAccelerationControls(
                                  src:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]
                                )(
    cartModel:MinecartControlModel,
    player:Player,
    mainHand:Material,
    target:Option[Location]
  ) =
    mainHand match {
      case _ if cartModel.fuel(player.getVehicle.getUniqueId) <= 0 => src
      case Material.FIREWORK_ROCKET =>
        val newspeed = cartModel.speed + 0.01
        val fuelUsed = 0.1
        val newfuel = cartModel.fuel ++ Map(player.getVehicle.getUniqueId -> (cartModel.fuel(player.getVehicle.getUniqueId) - fuelUsed))
        moveCart(player,newspeed,target)
        src ++[SpaceCraftPlayerEvent,MinecartControlModel] cartModel.copy(speed = newspeed, fuel = newfuel)
      case Material.ARROW =>
        moveCart(player,cartModel.speed,target)
        src
      case Material.IRON_SHOVEL =>
        val newspeed = math.max(0,cartModel.speed - 0.1)
        val fuelUsed = 0.1
        val newfuel = cartModel.fuel ++ Map(player.getVehicle.getUniqueId -> (cartModel.fuel(player.getVehicle.getUniqueId) - fuelUsed))
        moveCart(player,newspeed,target)
        src ++[SpaceCraftPlayerEvent,MinecartControlModel] cartModel.copy(speed = newspeed, fuel = newfuel)
      case _ =>
        val fuelUsed = 0.01
        src ++[SpaceCraftPlayerEvent,MinecartControlModel] cartModel.copy(speed = cartModel.speed - (fuelUsed * player.getVelocity.lengthSquared()))
    }
}
