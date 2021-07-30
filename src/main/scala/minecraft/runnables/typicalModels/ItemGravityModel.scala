package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import PlayerEvents._
import Players._
import org.bukkit.entity.{Entity, Item, Vehicle}
import org.bukkit.scheduler.BukkitTask
object ItemGravityModel {
  case class ItemGravityEvent(name:String,frequency:Double,probability:Double,gravity:Double,items:Seq[Entity],value:Option[BukkitTask] = None) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    val minDist = 14
    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      val itemGravEvent = event.asInstanceOf[ItemGravityEvent]
      val livingItems = itemGravEvent.items.filterNot(i => i.isDead || i.isOnGround || i.getPassengers.contains(player.value))
      val deadItems = itemGravEvent.items.filter(i => i.isDead || i.isOnGround || i.getPassengers.contains(player.value))
      deadItems.foreach(i => {
        i.setCustomNameVisible(false)
        i.setCustomName(null)
      })

      val gravityScaler = itemGravEvent.gravity
      livingItems.foreach(i => {
        val dist = i.getLocation().distance(player.getLocation())
        if(dist <= minDist) {
          i.setVelocity(new org.bukkit.util.Vector(0,0,0))
        }
        else{
          val vec = player.getLocation().subtract(i.getLocation())
          val playerVel = player.getVelocity.length()
          val baseScaler =  100
          val scaler = if(playerVel > 0) playerVel+ (playerVel * 0.10) else -baseScaler*gravityScaler/(dist*dist)
          val currvel = i.getVelocity
          i.setVelocity(currvel.subtract(vec.toVector.normalize().multiply(scaler)))
          i.setCustomName(s"Following ${player.getName}")
          i.setCustomNameVisible(true)
        }
      })
      src ++ itemGravEvent.copy(items = livingItems)
    }

  }
}
