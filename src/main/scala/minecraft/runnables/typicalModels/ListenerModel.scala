package minecraft.runnables.typicalModels
import java.io.{BufferedWriter, File, FileWriter}

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.scheduler.BukkitRunnable
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.reflect.runtime.universe.TypeTag
import minecraft.events.EventLoop._
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
object ListenerModel{
  trait ListenerModel[A<:dataset[_],U<:(A ==> (_>:A<:dataset[_]))] extends BukkitRunnable{
    val delay:Long
    val src:dataset[A with U]
    def run()(implicit taga:TypeTag[A],tagu:TypeTag[U]):Unit = runner
    def shouldRun:Boolean
    val serializer:dataset[A with U] => Unit
    val deserializer: () => dataset[A with U]
    def runner(implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A] = try{
      Thread.sleep(delay)
      println(s"Running:${this.toString}")
      val dat = deserializer()
      if(this.isCancelled) return dat
      val next =  if(shouldRun) dat.-->[U] else dat
      serializer(next)
      runner
    }catch{
      case e:Exception =>
        println(s"[SPACECRAFT-ERROR]${e.getMessage}")
        Thread.sleep(delay)
        runner
    }
    def noLoopRunner(implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A] = {
      val dat = deserializer()
      val next = if(shouldRun) dat.-->[U] else dat
      serializer(next)
      next
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

  implicit class ListenerGrammer[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](
                                                                                  override val src:dataset[A]
                                                                                )(
    implicit taga:TypeTag[A],
    override val serializer:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Unit
  ) extends ListenerModel [SpaceCraftPlayer with SpaceCraftPlayerEvent,SpaceCraftPlayerEvent]{
    val event = src.multifetch[SpaceCraftPlayerEvent].get//.asInstanceOf[SpaceCraftPlayerEvent]
    override val delay: Long = event.frequency.toLong
    override def shouldRun: Boolean = event.probability >= scala.math.random() && src.player.get.isOnline

    override def run(): Unit = super.run()

    override val deserializer: () => dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = () => for{
      player <- src.player
      em <- eventManager
    }yield{
      val event = src.multifetch[SpaceCraftPlayerEvent].asInstanceOf[SpaceCraftPlayerEvent]
      em.value((event.name,player.getUniqueId))
    }
  }
}

