package minecraft.runnables
import events.Events
import events.Events.Events
import org.bukkit.Material
import org.bukkit.entity.Player
import scala.collection.JavaConverters._
class TerrainHandler(override val player: Player) extends SpaceCraftRunnable {
  override val eventType: Events = Events.TerrainEvent

  override def runner(): Unit = {
    val centerpoint = player.getTargetBlock(Set(Material.AIR).asJava,100)

  }
}

object TerrainHandler {

}