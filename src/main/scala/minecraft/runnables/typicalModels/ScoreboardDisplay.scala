package minecraft.runnables.typicalModels

import minecraft.runnables.typicalModels.PlayerEvents.MonadicEvent
import org.bukkit.scheduler.BukkitTask
import Players._
import Typical.core.grammar._
import PlayerEvents._
import org.bukkit.{Bukkit, ChatColor}
import minecraft.events.EventLoop._
import ListenerModel._
object ScoreboardDisplay {
  case class ScoreboardDisplayModel(frequency:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    override def setFrequency(frequency: Double): PlayerEvents.SpaceCraftPlayerEvent = this.copy(frequency = frequency)

    override def setProbability(probability: Double): PlayerEvents.SpaceCraftPlayerEvent = this

    override val name: String = "ScoreboardDisplayEvent"
    override val probability: Double = 1

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
      player.copy(postProcessing = () => handleScore(player))
    }

    def handleScore(player:SpaceCraftPlayer):Unit = {
      val scoreboard =
        Bukkit.getScoreboardManager.getMainScoreboard
      val maybeObjective = scoreboard.getObjective("Vitals")
      val objective = if(maybeObjective == null) scoreboard.registerNewObjective(s"Vitals","dummy",s"Vitals") else maybeObjective
      println(s"registeringObjective:${objective != null}")
      val dat = oxyModel.deserializer()
      val oxyremaining = dat.player.get.oxygenRemaining
      println(s"data intact:${dat.isEmpty}")
      objective.getScore(player.getDisplayName).setScore((oxyremaining).floor.toInt)
    }
  }
}
