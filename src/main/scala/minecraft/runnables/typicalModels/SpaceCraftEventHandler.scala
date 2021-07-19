package minecraft.runnables.typicalModels

import Typical.core.dataset._
import Typical.core.grammar._
import scala.reflect.runtime.universe.TypeTag
import org.bukkit.event.Event

object SpaceCraftEventHandler {
  case class SpaceCraftEvent(value:Event) extends ::[SpaceCraftEvent] with produces[Event]
  implicit class EventGrammar[A<:SpaceCraftEvent](src:dataset[A])(implicit taga:TypeTag[A]){
    def event:dataset[SpaceCraftEvent] = if(src.isInstanceOf[SpaceCraftEvent]) src else src.<--[SpaceCraftEvent]
  }

  trait SpaceCraftEventHandler[A<:dataset[_]] extends (SpaceCraftEvent with A ==> SpaceCraftEventHandler[A]) with produces[PartialFunction[Event,Boolean]]

}
