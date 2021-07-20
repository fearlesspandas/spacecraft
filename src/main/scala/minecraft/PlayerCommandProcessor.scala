package minecraft

import java.util

import minecraft.events.EventLoop._
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import Typical.core.grammar._
import Typical.core.dataset._
import minecraft.runnables.typicalModels.EventManager.EventManager
import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin

case class PlayerCommandProcessor(plugin:JavaPlugin) {
  def onCommand(sender: Player, command: Command, label: String, args: Array[String]): dataset[EventManager] = command match {
    case cmd if cmd.getName == "addoxy" =>
      sender.sendMessage("add oxy command triggered")
      for{
      em <- eventManager
      spcplayer <- em.value.getOrElse((oxyModel.name,sender.getUniqueId),throw new Error(s"No OxygenModel found for ${sender.getDisplayName}")).player
    }yield{
        eventManager.updateEvent(oxyModel,spcplayer,plugin,oxyReplenishModel)
      }
    case cmd if cmd.getName == "showevents" =>
      sender.sendMessage(eventManager.getValue.value.keys.toString);
      eventManager
    case cmd => sender.sendMessage(s"No command found for ${cmd.getName}")
      eventManager
  }
  def onTabComplete(sender: Player, command: Command, alias: String, args: Array[String]): util.List[String] = List().asJava
}
