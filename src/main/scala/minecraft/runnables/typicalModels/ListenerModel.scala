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
      val dat = deserializer()
      println(s"Running:${dat}")
      if(this.isCancelled) return dat
      val nextMaybe =  if(shouldRun) dat.-->[U] else dat
      val next = if(nextMaybe.isEmpty) dat else nextMaybe
      if(nextMaybe.isEmpty){
        println(s"[SPACECRAFT-ERROR]:${nextMaybe.asInstanceOf[DatasetError[_]].value.headOption.map(_.getMessage)}")
        dat
      }else{
        serializer(next)
        runner
      }
    }catch{
      case e:Exception =>
        println(s"[SPACECRAFT-ERROR]${e.getCause}\n")
        e.printStackTrace()
        Thread.sleep(delay)
        runner
    }
    def noLoopRunner(implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A] = {
      val dat = deserializer()
      val next = if(shouldRun) dat.-->[U] else dat
      serializer(next)
      next
    }
  }

  implicit class ListenerGrammer[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](
                                                                                  override val src:dataset[A]
                                                                                )(
    implicit taga:TypeTag[A],
    override val serializer:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Unit
  ) extends ListenerModel [SpaceCraftPlayer with SpaceCraftPlayerEvent,SpaceCraftPlayerEvent]{
    val frequency = src.<--[SpaceCraftPlayerEvent].biMap(_ => 3000d)(_.get.frequency)
    val probability = src.<--[SpaceCraftPlayerEvent].biMap(_ => 0d)(_.get.probability)
    override val delay: Long = frequency.toLong
    override def shouldRun: Boolean = probability >= scala.math.random() && src.player.biMap(_ => false)( _.get.isOnline) && (!this.isCancelled)

    override def run(): Unit = super.run()

    override val deserializer: () => dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = () => for{
      player <- src.player
      em <- eventManager
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield{
      em.value((event.name,player.getUniqueId))
    }
  }
}

