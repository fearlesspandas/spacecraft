package minecraft;

import java.util

import Typical.core.dataset.DatasetError
import minecraft.events.EventLoop._
import minecraft.events.EventLoopTaskHandler.{EventLoopTask, ScoreboardTask}
import org.bukkit.Bukkit
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scoreboard.DisplaySlot
import PlayerCommandProcessor._
import scala.collection.JavaConverters._
class SpaceCraft extends JavaPlugin{

	val commandProcessor = new PlayerCommandProcessor(this)
  override def onEnable() {
		getLogger().info("Enabling SpaceCraft!");
		new EventLoopListener(this)
		new EventLoopTask().runTaskTimer(this,0,3)
		val scoreboard = Bukkit.getScoreboardManager.getMainScoreboard
		val maybeObjective = scoreboard.getObjective("Vitals")
		val objective = if(maybeObjective == null) scoreboard.registerNewObjective(s"Vitals","dummy",s"Vitals") else maybeObjective
		objective.setDisplaySlot(DisplaySlot.SIDEBAR)
		new ScoreboardTask().runTaskTimer(this,0,3)
	}
	override def onDisable() {
		getLogger().info("Shutting down SpaceCraft");
	}

	override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean = sender match {
		case p:Player => val res = commandProcessor.onCommand(p,command,label,args)
			if(res.isEmpty) p.sendMessage(res.asInstanceOf[DatasetError[_]].value.head.getCause.toString)
			!res.isEmpty
	}

	override def onTabComplete(sender: CommandSender, command: Command, alias: String, args: Array[String]): util.List[String] = sender match {
		case p:Player => PlayerCommandProcessor.onTabComplete(p,command,alias,args)
		case _ => List().asJava
	}


}