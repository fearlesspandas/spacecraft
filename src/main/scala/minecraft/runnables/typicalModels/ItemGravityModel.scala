package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import PlayerEvents._
import Players._
import org.bukkit.entity.Item
import org.bukkit.scheduler.BukkitTask
object ItemGravityModel {
  case class ItemGravityEvent(name:String,frequency:Double,probability:Double,gravity:Double,items:Seq[Item],value:Option[BukkitTask] = None) extends SpaceCraftPlayerEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    val minDist = 7
    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      val itemGravEvent = event.asInstanceOf[ItemGravityEvent]
      val livingItems = itemGravEvent.items.filterNot(_.isDead)
      //println(s"living items:${livingItems.size}")
      val gravityScaler = itemGravEvent.gravity
      livingItems.foreach(i => {
        val dist = i.getLocation().distance(player.getLocation())
        val playerloc = player.getLocation
        if(dist <= minDist) {
          //println("Freezing items")
          //println(s"itemDistace:${dist}")
          i.setVelocity(new org.bukkit.util.Vector(0,0,0))
        }
        else{
          //println("Calculating item gavity")
          //println(s"itemDistace:${dist}")
          val vec = player.getLocation().subtract(i.getLocation()).add(player.getVelocity)
          val scaler = -100*gravityScaler/(dist*dist)
          val currvel = i.getVelocity
          i.setVelocity(currvel.subtract(vec.toVector.normalize().multiply(scaler)))
        }
      })
      src ++ itemGravEvent.copy(items = livingItems)
    }

  }
}
