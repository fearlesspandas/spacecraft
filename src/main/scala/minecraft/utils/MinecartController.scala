package minecraft.utils

import Typical.core.dataset
import org.bukkit.entity.Player

import scala.collection.JavaConverters._
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.Players._
import org.bukkit.scheduler.BukkitTask
import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.Material
object MinecartController {

  case class MinecartControlModel(
                                   name:String,
                                   frequency:Double,
                                   probability:Double,
                                   speed:Double,
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
        if(offHand == Material.FIREWORK_ROCKET && mainHand == Material.FIREWORK_ROCKET){
          moveCart(player,cartModel.speed)
        }

        src
      }

  }
  def moveCart(player:Player,speed:Double):Unit = if(player.isInsideVehicle){
    val vehicle = player.getVehicle
    val block = player.getLineOfSight(null,10).asScala.sortWith((a,b) => {
      a.getLocation().distance(player.getLocation()) > b.getLocation().distance(player.getLocation())
    }).head
    val dir = block.getLocation().subtract(player.getLocation).toVector.normalize().multiply(speed)
    val currVel = vehicle.getVelocity
    vehicle.setVelocity(currVel.add(dir))
    vehicle.setRotation(player.getLocation().getPitch,player.getLocation.getYaw)

  }
}
