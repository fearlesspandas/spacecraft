package minecraft

import java.util

import minecraft.events.EventLoop._
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import Typical.core.grammar._
import Typical.core.dataset._
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent

import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin

case class PlayerCommandProcessor(plugin:JavaPlugin) {
  def onCommand(sender: Player, command: Command, label: String, args: Array[String]): dataset[EventManager] = command.getName match {
    case cmd if cmd == "addoxy" =>
      val amt = args.head.toInt
      modifyOxy(amt,sender)
    case cmd if cmd == "removeOxy" =>
      val amt = args.head.toInt
      modifyOxy(amt,sender)
    case cmd if cmd == "showevents" =>
      sender.sendMessage(eventManager.getValue.value.keys.toString);
      eventManager
    case cmd => sender.sendMessage(s"No command found for ${cmd}")
      eventManager
  }

  def modifyOxy( amt:Double,sender:Player):dataset[EventManager] = for{
    em <- eventManager
    spcplayer <- em.value.getOrElse((oxyModel.name,sender.getUniqueId),throw new Error(s"No OxygenModel found for ${sender.getDisplayName}")).player
  }yield{
    val m = OxygenReplenishEvent(amt)
    eventManager.updateEvent(oxyModel,spcplayer,plugin,m)
  }
  def onTabComplete(sender: Player, command: Command, alias: String, args: Array[String]): util.List[String] = List().asJava
}
