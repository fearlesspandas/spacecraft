package minecraft.runnables.typicalModels
import java.io.{BufferedWriter, File, FileWriter}

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.scheduler.BukkitRunnable
import io.circe.generic.JsonCodec, io.circe.syntax._
import scala.reflect.runtime.universe.TypeTag
object ListenerModel{
  trait ListenerModel[A<:dataset[_],U<:(A ==> (_>:A<:dataset[_]))] extends BukkitRunnable{
    val delay:Long
    val src:dataset[A with U]
    def run()(implicit taga:TypeTag[A],tagu:TypeTag[U]):Unit = runner(src)
    def shouldRun:Boolean
    val serializeA:dataset[A] => String
    def runner(dat:dataset[A])(implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A] = {
      println(s"Running:${this.toString}")
      Thread.sleep(delay)
      writeFile(serializeA(dat),this.getTaskId)
      val next =  if(shouldRun) dat.-->[U] else dat
      runner(next)
    }
    def writeFile(text:String,taskId:Long) = {
      val base = "spacecraftSnapshots"
      val dir = new File(base)
      val file = new File(s"$base${File.separator}${taskId}.json")
      dir.mkdirs()
      file.createNewFile()
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(text)
      bw.close()
    }
  }

  implicit class ListenerGrammer[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](override val src:dataset[A])(implicit taga:TypeTag[A]) extends ListenerModel [SpaceCraftPlayer,SpaceCraftPlayerEvent]{
    val event = src.multifetch[SpaceCraftPlayerEvent].asInstanceOf[SpaceCraftPlayerEvent]
    override val delay: Long = event.frequency.toLong
    override def shouldRun: Boolean = event.probability >= scala.math.random()

    override def run(): Unit = super.run()

    override val serializeA: dataset[SpaceCraftPlayer] => String = spc => spc.get.asJson.toString
  }
}

