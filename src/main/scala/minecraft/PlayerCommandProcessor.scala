package minecraft

import java.util

import minecraft.events.EventLoop._
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import Typical.core.grammar._
import Typical.core.dataset._
import minecraft.events.EventLoop
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent

import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin

case class PlayerCommandProcessor(plugin:JavaPlugin) {
  def onCommand(sender: Player, command: Command, label: String, args: Array[String]): dataset[EventManager] = command.getName match {
    case cmd if cmd == "addoxy" =>
      val amt = args.headOption.map(_.toInt).getOrElse(1)
      modifyOxy(amt,sender)
    case cmd if cmd == "removeOxy" =>
      val amt = args.headOption.map(_.toInt).getOrElse(1)
      modifyOxy(amt,sender)
    case cmd if cmd == "showevents" =>
      sender.sendMessage(eventManager.getValue.value.keys.toString);
      eventManager
    case cmd if cmd == "updateprob" =>
      val eventName = args.headOption.getOrElse("Nothing")
      val prob = args.tail.headOption.map(_.toDouble).getOrElse(0d)
      updateProb(prob,eventName,sender)
    case cmd if cmd == "updatefreq" =>
      val eventName =  args.headOption.getOrElse("Nothing")
      val freq = args.tail.headOption.map(_.toDouble).getOrElse(0d)
      updateFrequency(freq,eventName,sender)
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

  def updateProb(value:Double,eventName:String,player:Player): dataset[EventManager] = {
    for {
      event <- baseTasks.find(_.name == eventName).map(_.setProbability(value)).fromOption
      em <- eventManager
      spcplayer <- em.value.getOrElse((event.name,player.getUniqueId),throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
    }yield em.updateEvent(event,spcplayer,plugin)
  }
  def updateFrequency(value:Double,eventName:String,player:Player): dataset[EventManager] = {
    for {
      event <- baseTasks.find(_.name == eventName).map(_.setFrequency(value)).fromOption
      em <- eventManager
      spcplayer <- em.value.getOrElse((event.name, player.getUniqueId), throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
    } yield em.updateEvent(event, spcplayer, plugin)
  }
  def onTabComplete(sender: Player, command: Command, alias: String, args: Array[String]): util.List[String] = command.getName match {
    case "updateprob" => args.size match {
      case 1 =>baseTasks.filter(_.name.toUpperCase().contains(args.head.toUpperCase())).map(_.name).toList.asJava
      case 2 => List("""0 < number < 1 """).asJava
      case _ => List().asJava
    }
    case "updatefreq" => args.size match {
      case 1 =>baseTasks.filter(_.name.toUpperCase().contains(args.head.toUpperCase())).map(_.name).toList.asJava
      case 2 => List("""0 < number < 1 """).asJava
      case _ => List().asJava
    }
    case "addoxy" => args.size match {
      case 1 => List("""0 < number < 1 """).asJava
      case _ => List().asJava
    }
    case _ => List().asJava
  }
}
