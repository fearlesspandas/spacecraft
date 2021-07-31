package minecraft.runnables.typicalModels
import java.io.{BufferedWriter, File, FileWriter}

import Typical.core.dataset._
import Typical.core.grammar._
import io.circe.{Decoder, Encoder}
import Players.{Players, SpaceCraftPlayer, _}
import org.bukkit.Bukkit
import org.bukkit.entity.Player
import org.bukkit.event.Event
import org.bukkit.scheduler.BukkitTask
import ListenerModel._
import org.bukkit.plugin.java.JavaPlugin
import io.circe.{Decoder, Encoder}
import io.circe.generic._
import io.circe.syntax._
import io.circe._
import io.circe.parser._
import minecraft.runnables.typicalModels.PlayerEvents.filename
import org.bukkit.Bukkit

import scala.reflect.runtime.universe.TypeTag
object PlayerEvents{
  val snapshotBase = "spacecraftSnapshots"
  def filename(taskid:Long):String = s"$snapshotBase${File.separator}$taskid.json"

  trait SpaceCraftTask
  trait SpaceCraftPlayerEvent extends (SpaceCraftPlayer with SpaceCraftPlayerEvent ==> SpaceCraftPlayer with SpaceCraftPlayerEvent) with produces[Option[BukkitTask]]{
    def setFrequency(frequency:Double):SpaceCraftPlayerEvent
    def setProbability(probability:Double):SpaceCraftPlayerEvent
    def apply(bukkitTask: BukkitTask):SpaceCraftPlayerEvent
    val name:String
    val frequency:Double
    val probability:Double
  }
  trait MonadicEvent extends SpaceCraftPlayerEvent{
    def apply(player:SpaceCraftPlayer):SpaceCraftPlayer
    override def apply(src: dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent]): dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
      player <- src.player
      event <- src.<--[SpaceCraftPlayerEvent]
    }yield src ++ apply(player) ++ event
  }
  case object NoEvent extends MonadicEvent {
    override val name: String = "NoEvent"
    override val frequency: Double = 0
    override val probability: Double = 0
    override val value: Option[BukkitTask] = None
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this
    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = player

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = NoEvent

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = NoEvent
  }

  implicit class PlayerEventsGram[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](
                                                                                   src:dataset[A]
                                                                                 )(
    implicit taga:TypeTag[A],
    serializer:dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] => Unit
  ){
    def updateEvent(event:SpaceCraftPlayerEvent,plugin:JavaPlugin,via:SpaceCraftPlayerEvent = NoEvent):dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer] =
      src
        .multifetch[SpaceCraftPlayerEvent]
        .fold[SpaceCraftPlayerEvent with SpaceCraftPlayer](
          _ => {
            val newsrc = src --> via
            for{
              player <-(newsrc).player
              task <- newsrc.<--[SpaceCraftPlayerEvent]
            }yield{
              player.sendMessage("triggering task")
              val newtask = if(via == NoEvent)event else task
              val ctx =  newsrc ++ newtask ++ player
              val eventTask = newtask.apply(ctx.runTaskAsynchronously(plugin))
              val res = src ++ eventTask ++ player
              serializer(res)
              res
            }
          }
        )(
          d => {
            val oldEvent = d.asInstanceOf[SpaceCraftPlayerEvent]
            //get task id and immediately cancel task
            val eventTaskId = oldEvent.value.map(_.getTaskId)
            println(s"modifying ${eventTaskId}")
            serializer(src)
            oldEvent.value.map(_.cancel())
            println(s"event is cancelled:${oldEvent.value.map(_.isCancelled)}")
            val deserialized = src.deserializer()
            val newSrc = deserialized --> via
            for{
              player <- (newSrc).player
              task <- newSrc.<--[SpaceCraftPlayerEvent]
            }yield{
              //update context and start the task again
              println("Player event updated")
              val newtask = if(via == NoEvent)event else task
              val ctx =  newSrc ++ newtask ++ player
              val eventTask = newtask.apply(ctx.runTaskAsynchronously(plugin))
              val res = newSrc ++ eventTask ++ player
              serializer(res)
              println("PlayerEventSerialized")
              res
            }
          }
        )
    def cancelTask():Unit = src.<--[SpaceCraftPlayerEvent].biMap(_ => ())(d => d.get.value.map(_.cancel()))
  }

}

