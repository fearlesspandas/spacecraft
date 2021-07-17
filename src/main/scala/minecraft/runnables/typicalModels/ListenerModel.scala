package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.scheduler.BukkitRunnable

import scala.reflect.runtime.universe.TypeTag
object ListenerModel{
  trait ListenerModel[A<:dataset[_]] extends (A ==> A) {
    override def apply(src: dataset[A]): dataset[A] = ???



  }



  implicit class ListenerGrammer[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]) extends BukkitRunnable{
    //def derive[B<:dataset[A],U<:(A ==> B)](u:U)(implicit tagu:TypeTag[U],tagb:TypeTag[B]): dataset[B] = (src +- u) --> u
    def runnable = this
    val queue = Seq()

    def runner(dat:dataset[A],delay:Long):dataset[A] = {
      Thread.sleep(delay)
      runner(dat.-->[SpaceCraftPlayerEvent],delay)
    }
    override def run(): Unit = runner(src,10000)
  }
}

