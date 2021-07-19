package minecraft.runnables.typicalModels
import java.util.UUID

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.mutable.Map
import scala.reflect.runtime.universe.TypeTag
object EventManager {
  case class EventManager(value:Map[(String,UUID),dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]]) extends ::[EventManager] with produces[Map[(String,UUID),dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]]]

  implicit class EventManagerGrammar[A<:EventManager](src:dataset[A])(implicit taga:TypeTag[A]){
    def eventManager:dataset[EventManager] = if(src.isInstanceOf[EventManager]) src else src.<--[EventManager]
    def updateEvent(event:SpaceCraftPlayerEvent,player:SpaceCraftPlayer,plug:JavaPlugin,via:SpaceCraftPlayerEvent = NoEvent):dataset[A] = for{
      eventManager <- src.eventManager
    }yield {
      player.sendMessage("adding tasks")
      val updatedEventTask  = eventManager.value.getOrElse((event.name,player.getUniqueId),data[SpaceCraftPlayer]() ++ player ++ event ).updateEvent(event,plug,via)
       eventManager.update((event.name,player.getUniqueId),updatedEventTask)
      for{
        taskplayer <- updatedEventTask.player
      }yield{
        taskplayer.sendMessage(s"EventUpdated:${updatedEventTask.multifetch[SpaceCraftPlayerEvent].asInstanceOf[SpaceCraftPlayerEvent].value}")
        player
      }
      player.sendMessage(s"event updated${updatedEventTask}")
      player.sendMessage(s"EventManager${eventManager.value}")
      src ++ eventManager
    }
  }


}
