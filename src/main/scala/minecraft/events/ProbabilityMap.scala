package minecraft.events
import Typical.core.dataset._
import Typical.core.grammar._
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
case class ProbabilityMap(value:Map[SpaceCraftPlayerEvent,Int]) extends ::[ProbabilityMap] with produces[Map[SpaceCraftPlayerEvent,Int]]