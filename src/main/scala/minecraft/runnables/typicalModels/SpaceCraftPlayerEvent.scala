package minecraft.runnables.typicalModels
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.events.{FrequencyMap, ProbabilityMap}
import minecraft.runnables.typicalModels.Players.{Players, SpaceCraftPlayer}
import org.bukkit.entity.Player
import org.bukkit.event.Event

import scala.reflect.runtime.universe.TypeTag
object PlayerEvents{

  trait SpaceCraftPlayerEvent extends (SpaceCraftPlayer ==> SpaceCraftPlayer){
    type EventTag <: Event
    val commandProcessor:Seq[PartialFunction[(String,Array[String]), Boolean]]
    val tabComplete : Seq[PartialFunction[(String,Int) , List[String]]]
    def apply(player:SpaceCraftPlayer):SpaceCraftPlayer
    override def apply(src: dataset[SpaceCraftPlayer]): dataset[SpaceCraftPlayer] = for{
      player <- src.player
    }yield src ++ apply(player)

    val name:String
    val frequency:Double
    val probability:Double
  }

  implicit class SpaceCraftPlayerEventGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def handle[U <: (A ==> A)](u:U)(implicit tagu:TypeTag[U]):dataset[A] = ((src +- u) --> u)
  }

  case object NoEvent extends SpaceCraftPlayerEvent {
    override val name: String = "NoEvent"
    override val frequency: Double = 0
    override val probability: Double = 0

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = player

    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq()
  }

  case class PlayerEvents(value:Seq[SpaceCraftPlayerEvent])
    extends produces[Seq[SpaceCraftPlayerEvent]]
      with (ProbabilityMap with FrequencyMap ==> PlayerEvents){
    //'empty' constructor, i.e. we only check that dependencies are met
    override def apply(src: dataset[ProbabilityMap with FrequencyMap]): dataset[PlayerEvents] = for{
      _ <- src.<--[ProbabilityMap]
      _ <- src.<--[FrequencyMap]
    }yield this
    def apply(player:SpaceCraftPlayerEvent) :dataset[PlayerEvents] =
      if(value.exists(_.name == player.name))
        PlayerEvents(value.collect({case p if p == player => player; case p => p}))
      else PlayerEvents(player +: value)
  }
  implicit class PlayerEventsGrammar[A<:ProbabilityMap with FrequencyMap with Players with PlayerEvents](src:dataset[A])(implicit taga:TypeTag[A]){
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

