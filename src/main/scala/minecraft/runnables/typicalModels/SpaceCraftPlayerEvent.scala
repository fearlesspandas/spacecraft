package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import io.circe.{Decoder, Encoder}
import minecraft.events.{FrequencyMap, ProbabilityMap}
import minecraft.runnables.typicalModels.Players.{Players, SpaceCraftPlayer}
import org.bukkit.Bukkit
import org.bukkit.entity.Player
import org.bukkit.event.Event
import org.bukkit.scheduler.BukkitTask
import ListenerModel._
import org.bukkit.plugin.java.JavaPlugin

import scala.reflect.runtime.universe.TypeTag
object PlayerEvents{
  trait SpaceCraftPlayerEvent extends (SpaceCraftPlayer ==> SpaceCraftPlayer) with produces[Option[BukkitTask]]{

    val commandProcessor:Seq[PartialFunction[(String,Array[String]), Boolean]]
    val tabComplete : Seq[PartialFunction[(String,Int) , List[String]]]
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
  }

  implicit class SpaceCraftPlayerEventGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def handle[U <: (A ==> A)](u:U)(implicit tagu:TypeTag[U]):dataset[A] = ((src +- u) --> u)
  }


  case class PlayerEvents(value:Seq[SpaceCraftPlayerEvent])
    extends produces[Seq[SpaceCraftPlayerEvent]]
      with (PlayerEvents ==> PlayerEvents){
    //'empty' constructor, i.e. we only check that dependencies are met
    override def apply(src: dataset[PlayerEvents]): dataset[PlayerEvents] = this
    def apply(player:SpaceCraftPlayerEvent) :dataset[PlayerEvents] =
      if(value.exists(_.name == player.name))
        PlayerEvents(value.collect({case p if p == player => {
          player.value.map(_.cancel)
          player
        }; case p => p}))
      else PlayerEvents(player +: value)
  }

  implicit class PlayerEventsGram[A<:SpaceCraftPlayerEvent with SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
      def updateEvent(event:SpaceCraftPlayerEvent,plugin:JavaPlugin,via:SpaceCraftPlayerEvent = NoEvent):dataset[A] =
        src
          .multifetch[SpaceCraftPlayerEvent]
          .fold(
        _ => for{
          player <-(src --> via).player
        }yield{
          player.sendMessage("adding tasks within")
          val ctx = src ++ event ++ player
          val eventTask = event.apply(ctx.runTaskAsynchronously(plugin))
          src ++ eventTask ++ player
        }
      )(
        d => for{
          player <- (src --> via).player
        }yield{
          player.sendMessage("adding tasks within")
          d.asInstanceOf[SpaceCraftPlayerEvent].value.map(_.cancel())
          val ctx = src ++ event ++ player
          val eventTask = event.apply(ctx.runTaskAsynchronously(plugin))
          src ++ eventTask ++ player
        }
      )
    def cancelTask():Unit = src
      .multifetch[SpaceCraftPlayerEvent]
      .asInstanceOf[SpaceCraftPlayerEvent]
      .value.map(_.cancel())
  }
  implicit class PlayerEventsGrammar[A<: Players with PlayerEvents](src:dataset[A])(implicit taga:TypeTag[A]){
    def playerEvents:dataset[PlayerEvents] = if(src.isInstanceOf[PlayerEvents]) src else src.<--[PlayerEvents]

    def runByName(name:String,playerIn:Player):dataset[A] = for{
      players <- src.players
      player <- players.get(playerIn)
      playerEvents <- src.playerEvents
    }yield{
      playerEvents.value.collectFirst({case p if p.name == name => p}).fold[dataset[A]](DatasetError[A](new Error(s"No value found for ${name}")))(e =>
      for{
        updatedPlayer <- player handle e
      }yield src ++ players.apply(updatedPlayer))
    }
    def handle[U<:(SpaceCraftPlayer ==> SpaceCraftPlayer)](u:U,playerIn:Player)(implicit tagu:TypeTag[U]):dataset[A] = for{
      players <- src.players
      player <- players.get(playerIn)
      updatedPlayer <- player handle u
    }yield{
      src ++  players.apply(updatedPlayer)
    }




  }

}

