package minecraft

import java.util

import minecraft.events.EventLoop
import minecraft.events.EventLoop.{eventManager, oxyModel, oxyReplenishModel}
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import Typical.core.grammar._
import Typical.core.dataset._
import minecraft.runnables.typicalModels.EventManager.EventManager
import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin

case class PlayerCommandProcessor(plugin:JavaPlugin) {
  def onCommand(sender: Player, command: Command, label: String, args: Array[String]): dataset[EventManager] = command match {
    case cmd if cmd == "addoxy" =>
      for{
      em <- eventManager
      spcplayer <- em.value.getOrElse((oxyModel.name,sender.getUniqueId),throw new Error(s"No OxygenModel found for ${sender.getDisplayName}")).player
    }yield
      eventManager.updateEvent(oxyModel,spcplayer,plugin,oxyReplenishModel)

    case cmd if cmd == "showEvents" => sender.sendMessage(eventManager.getValue.value.toString);eventManager

    case _ => eventManager
  }
  def onTabComplete(sender: Player, command: Command, alias: String, args: Array[String]): util.List[String] = List().asJava
}
