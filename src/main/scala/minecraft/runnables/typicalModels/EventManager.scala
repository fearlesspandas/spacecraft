package minecraft.runnables.typicalModels
import java.util.UUID

import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents._
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.concurrent.Map
import scala.reflect.runtime.universe.TypeTag
object EventManager {
  case class EventManager(
                           value:Map[(String,UUID),dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]],
                           eventStack:scala.collection.mutable.ArrayStack[() => Unit] = scala.collection.mutable.ArrayStack()
                         )
    extends ::[EventManager]
    with produces[Map[(String,UUID),dataset[SpaceCraftPlayerEvent with SpaceCraftPlayer]]]

  implicit class EventManagerGrammar[A<:EventManager](src:dataset[A])(implicit taga:TypeTag[A],serializer:dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] => Unit){
    def eventManager:dataset[EventManager] = if(src.isInstanceOf[EventManager]) src else src.<--[EventManager]
    def updateEvent(event:SpaceCraftPlayerEvent,player:SpaceCraftPlayer,plug:JavaPlugin,via:SpaceCraftPlayerEvent = NoEvent):dataset[A] = for{
      eventManager <- src.eventManager
    }yield {
      val updatedEventTask  =
        eventManager
          .value
          .getOrElse(
            (
              event.name,
              player.getUniqueId
            ),
              data[SpaceCraftPlayer]() ++ player ++ event
          )
          .updateEvent(event,plug,via)
      println("event updated in event manager")
       //eventManager.update((event.name,player.getUniqueId),updatedEventTask)
      for{
        taskplayer <- updatedEventTask.player
      }yield{
        player
      }
      println(s"isEmpty:${eventManager.isEmpty}")
      println("task updated")
      val res = if(src.isInstanceOf[EventManager]) eventManager.asInstanceOf[dataset[A]] else src ++ eventManager
      println("newEvent manager crated")
      println(s"newmanager isemtpy:${res.isEmpty}")
      res
    }
  def getTask(player:Player,task:String):dataset[SpaceCraftPlayer with SpaceCraftPlayerEvent] = for{
    em <- src.eventManager
  }yield em.value.getOrElse((task,player.getUniqueId),DatasetError[SpaceCraftPlayerEvent with SpaceCraftPlayer](new Error(s"No task ${task} for player ${player.getDisplayName}")))
    def triggerEvent(event:SpaceCraftPlayerEvent,player:SpaceCraftPlayer,plugin: JavaPlugin):dataset[A] = for{
      eventmanager <- src.eventManager
    }yield src ++ eventmanager.updateEvent(event,player,plugin,event)


  }


}
