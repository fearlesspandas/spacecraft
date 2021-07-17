package minecraft.runnables.typicalModels

import java.time.LocalTime

import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.entity.Player
import OxygenModel._
import RocketChargeModel._

import scala.reflect.runtime.universe.TypeTag
object Players {
  case class SpaceCraftPlayer(value:Player,oxygenRemaining:Double,lastBreadth:LocalTime = LocalTime.now) extends (SpaceCraftPlayer ==> SpaceCraftPlayer) with produces[Player]{
    override def apply(src: dataset[SpaceCraftPlayer]): dataset[SpaceCraftPlayer] = src.<--[SpaceCraftPlayer]
  }
  implicit class SpPlayerGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit typeTag: TypeTag[A]){
    def player :dataset[SpaceCraftPlayer] = if(src.isInstanceOf[SpaceCraftPlayer]) src else src.<--[SpaceCraftPlayer]
    def addOxy(amt:Int):dataset[SpaceCraftPlayer] = for {
      player <- src.player
    }yield player.copy(oxygenRemaining = player.oxygenRemaining + amt)

    def handlePlayer:dataset[SpaceCraftPlayer] = for{
      player <- src.player
      updatedPlayer <- (data[SpaceCraftPlayer]() ++ player).<-+[SpaceCraftPlayer]
    }yield updatedPlayer
    def setGravity(b:Boolean):dataset[SpaceCraftPlayer] = for{
      player <- src.player
    } yield {
      player.value.setGravity(b)
      player
    }
  }

  case class Players(value:Seq[SpaceCraftPlayer]) extends (Players ==> Players) with produces[Seq[SpaceCraftPlayer]]{
    override def apply(src: dataset[Players]): dataset[Players] = src.<--[Players]
    def apply(player:SpaceCraftPlayer) :dataset[Players] =
      if(value.exists(_.getUniqueId == player.getUniqueId))
        Players(value.collect({case p if p.getUniqueId == player.getUniqueId => player; case p => p}))
      else Players(player +: value)
    def get(player:Player):dataset[SpaceCraftPlayer] = value.collectFirst({case p if p.value.getUniqueId == player.getUniqueId => p}).fromOption
  }

  implicit class PlayerGrammar[A<:Players](src:dataset[A])(implicit taga:TypeTag[A]){
        def players:dataset[Players] = if(src.isInstanceOf[Players]) src else src.<--[Players]
        def setGravity(player:Player,b:Boolean):dataset[A] = for{
          players <- src.players
          player <- players.get(player).setGravity(b)
        }yield src ++ players.apply(player)
        def getPlayer(player:Player):dataset[SpaceCraftPlayer] = for{
          players <- src.players
          player <- players.get(player)
        }yield player
        def addPlayers(players:SpaceCraftPlayer* ):dataset[A] = for{
          allPlayers <- src.players
        }yield players.foldLeft(src)((accumsrc,player) => for {
         updatedPlayers <-  allPlayers.apply(player)
        }yield accumsrc ++ updatedPlayers)
        def addOxy(player:Player,amt:Int):dataset[A] = for{
          players <- src.players
          player <- players.get(player)
          updatedPlayer <- player.addOxy(amt)
        }yield src ++ players.apply(updatedPlayer)
      def getOxy(player:Player):produces[Double] = src.players.getPlayer(player).biMap[produces[Double]](
        e => noVal(e.value :_*)
      )(
        d => someval(d.asInstanceOf[SpaceCraftPlayer].oxygenRemaining)
      )
      def handleOxy(player:Player) : dataset[A] = for{
        players <- src.players
        player <- players.get(player)
        updatedPlayer <- player.handleOxy
      }yield src ++ players.apply(updatedPlayer)

    def addRockets(player:Player,amt:Int):dataset[A] = for{
      players <- src.players
      playerState <- players.get(player)
      _ <- playerState.addRockets(amt)
    }yield src
  }

}
