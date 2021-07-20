package minecraft.runnables.typicalModels
import java.io.{BufferedWriter, File, FileWriter}

import Typical.core.dataset._
import Typical.core.grammar._
import io.circe.{Decoder, Encoder}
import minecraft.events.{FrequencyMap, ProbabilityMap}
import Players.{Players, SpaceCraftPlayer, readPlayer, _}
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


  trait SpaceCraftPlayerEvent extends (SpaceCraftPlayer ==> SpaceCraftPlayer) with produces[Option[BukkitTask]]{

    val commandProcessor:Seq[PartialFunction[(String,Array[String]), Boolean]]
    val tabComplete : Seq[PartialFunction[(String,Int) , List[String]]]
    def setFrequency(frequency:Double):SpaceCraftPlayerEvent
    def setProbability(probability:Double):SpaceCraftPlayerEvent

    def apply(player:SpaceCraftPlayer):SpaceCraftPlayer
    override def apply(src: dataset[SpaceCraftPlayer]): dataset[SpaceCraftPlayer] = for{
      player <- src.player
    }yield src ++ apply(player)

    def apply(bukkitTask: BukkitTask):SpaceCraftPlayerEvent
    val name:String
    val frequency:Double
    val probability:Double
  }

  case object NoEvent extends SpaceCraftPlayerEvent {
    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq()
    override val name: String = "NoEvent"
    override val frequency: Double = 0
    override val probability: Double = 0
    override val value: Option[BukkitTask] = None
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this
    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = player

    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = NoEvent

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = NoEvent
  }

  implicit class SpaceCraftPlayerEventGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def handle[U <: (A ==> A)](u:U)(implicit tagu:TypeTag[U]):dataset[A] = ((src +- u) --> u)
  }

  implicit class PlayerEventsGram[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def updateEvent(event:SpaceCraftPlayerEvent,plugin:JavaPlugin,via:SpaceCraftPlayerEvent = NoEvent):dataset[A] =
      src
        .multifetch[SpaceCraftPlayerEvent]
        .fold(
          _ => for{
            player <-(src --> via).player
          }yield{
            player.sendMessage("triggering task")
            val ctx = src ++ event ++ player
            val eventTask = event.apply(ctx.runTaskAsynchronously(plugin))
            src ++ eventTask ++ player
          }
        )(
          d => {

            val oldEvent = d.asInstanceOf[SpaceCraftPlayerEvent]
            //get task id and immediately cancel task
            val eventTaskId = oldEvent.value.map(_.getTaskId)
            println(s"modifying ${eventTaskId}")
            oldEvent.value.map(_.cancel())
            //retrieve player from task output
//            val transientPlayer = (for{
//              updatedPlayer <- eventTaskId.map(t => readPlayer(filename(t))).fromOption
//            }yield{
//              updatedPlayer
//            }).biMap(_ => src.player.get)(r => r.get)
            //println(transientPlayer)
            //apply other transformations on top of serialized result
            for{
              player <- (src --> via).player
            }yield{
              //update context and start the task again
              val ctx = src ++ event ++ player
              val eventTask = event.apply(ctx.runTaskAsynchronously(plugin))
              src ++ eventTask ++ player
            }
          }
        )
    def cancelTask():Unit = src
      .multifetch[SpaceCraftPlayerEvent]
      .asInstanceOf[SpaceCraftPlayerEvent]
      .value.map(_.cancel())
  }
//
//
//  case class PlayerEvents(value:Seq[SpaceCraftPlayerEvent])
//    extends produces[Seq[SpaceCraftPlayerEvent]]
//      with (PlayerEvents ==> PlayerEvents){
//    //'empty' constructor, i.e. we only check that dependencies are met
//    override def apply(src: dataset[PlayerEvents]): dataset[PlayerEvents] = this
//    def apply(player:SpaceCraftPlayerEvent) :dataset[PlayerEvents] =
//      if(value.exists(_.name == player.name))
//        PlayerEvents(value.collect({case p if p == player => {
//          player.value.map(_.cancel)
//          player
//        }; case p => p}))
//      else PlayerEvents(player +: value)
//  }
//
//  implicit class PlayerEventsGrammar[A<: Players with PlayerEvents](src:dataset[A])(implicit taga:TypeTag[A]){
//    def playerEvents:dataset[PlayerEvents] = if(src.isInstanceOf[PlayerEvents]) src else src.<--[PlayerEvents]
//
//    def runByName(name:String,playerIn:Player):dataset[A] = for{
//      players <- src.players
//      player <- players.get(playerIn)
//      playerEvents <- src.playerEvents
//    }yield{
//      playerEvents.value.collectFirst({case p if p.name == name => p}).fold[dataset[A]](DatasetError[A](new Error(s"No value found for ${name}")))(e =>
//      for{
//        updatedPlayer <- player handle e
//      }yield src ++ players.apply(updatedPlayer))
//    }
//    def handle[U<:(SpaceCraftPlayer ==> SpaceCraftPlayer)](u:U,playerIn:Player)(implicit tagu:TypeTag[U]):dataset[A] = for{
//      players <- src.players
//      player <- players.get(playerIn)
//      updatedPlayer <- player handle u
//    }yield{
//      src ++  players.apply(updatedPlayer)
//    }
//
//
//
//
//  }

}

