package minecraft.events
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
case class FrequencyMap(value:Map[SpaceCraftPlayerEvent,Int]) extends ::[FrequencyMap] with produces[Map[SpaceCraftPlayerEvent,Int]]
