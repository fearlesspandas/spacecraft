package minecraft.runnables.typicalModels

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.event.player.PlayerMoveEvent
import org.bukkit.inventory.ItemStack
import org.bukkit.scheduler.BukkitTask
import org.bukkit.{ChatColor, Material}

import scala.reflect.runtime.universe.TypeTag
object RocketChargeModel {
  case class RocketChargeModel(frequency:Double,value:Option[BukkitTask]) extends SpaceCraftPlayerEvent {
     def apply(player:SpaceCraftPlayer): SpaceCraftPlayer = {
      if(player.isGliding){
        val inventory = player.getInventory
        inventory.addItem(new ItemStack(Material.FIREWORK_ROCKET,1))
        player.sendMessage(s"${ChatColor.GRAY} +1 Rocket")
      }
      player
    }

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override val name: String = "RocketChargeEvent"


    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq()

    override val probability: Double = 1

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency =frequency)

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
  }

  implicit class RocketGrammer[A<:SpaceCraftPlayer](src:dataset[A])(implicit ttag:TypeTag[A]){
//    def addRockets(amt:Int):dataset[SpaceCraftPlayer] = for{
//      player <- src.player
//    }yield player --> RocketChargeModel(10000)
  }
}
