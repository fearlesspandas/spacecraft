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
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
import org.bukkit.{Bukkit, Material}

import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scoreboard.DisplaySlot

object PlayerCommandProcessor{
  case class PlayerCommandProcessor(plugin:JavaPlugin) {
    def onCommand(sender: Player, command: Command, label: String, args: Array[String]): dataset[EventManager] = command.getName match {
      case "addoxy" =>
        val amt = args.headOption.map(_.toInt).getOrElse(1)
        modifyOxy(amt,sender)
      case "removeoxy" =>
        val amt = args.headOption.map(_.toInt).getOrElse(1)
        modifyOxy(amt,sender)
      case "showevents" =>
        sender.sendMessage(eventManager.getValue.value.keys.toString);
        eventManager
      case "updateprob" =>
        val eventName = args.headOption.getOrElse("Nothing")
        val prob = args.tail.headOption.map(_.toDouble).getOrElse(0d)
        updateProb(prob,eventName,sender)
      case "updatefreq" =>
        val eventName =  args.headOption.getOrElse("Nothing")
        val freq = args.tail.headOption.map(_.toDouble).getOrElse(0d)
        updateFrequency(freq,eventName,sender)
      case "gravitystrength" =>
        val value = args.headOption.map(_.toDouble).getOrElse(1d)
        updateGravityStrength(value,sender)
      case "gravitymeter" =>
        gravScoreBoard
        eventManager
      case "oxymeter" =>
        oxyScoreBoard
        eventManager
      case "giveplayercompass" =>
        givePlayerGravCompass(sender)
        eventManager
      case cmd => sender.sendMessage(s"No command found for ${cmd}")
        eventManager
    }
    def updateGravityStrength(value:Double,player:Player):dataset[EventManager] = for{
      em <- eventManager
      spcplayer <- em.value.getOrElse((gravityEvent.name,player.getUniqueId),throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
    }yield{
      eventManager
        .updateEvent(gravityEvent.copy(gravity = value),spcplayer,plugin)
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
        spcplayer <- em.value.getOrElse((event.name,player.getUniqueId),throw new Error(s"No OxygenModel found for ${player.getDisplayName}")).player
      }yield em.updateEvent(event,spcplayer,plugin)
    }

  }


  def givePlayerGravCompass(player:Player): Unit =for{
    em <- eventManager
    event1 <-  em.getTask(player,gravityEvent.name).<--[SpaceCraftPlayerEvent]
    event2 <- em.getTask(player,gravityEvent2.name).<--[SpaceCraftPlayerEvent]
    event3 <- em.getTask(player,gravityEvent3.name).<--[SpaceCraftPlayerEvent]
  }yield{
    val (e1,e2,e3) = (event1.asInstanceOf[PlayerGravityEvent],event2.asInstanceOf[PlayerGravityEvent],event3.asInstanceOf[PlayerGravityEvent])
    player.setCompassTarget(e1.knownBlocks.head.getLocation())
    em
  }
  def gravScoreBoard = {
    Bukkit.getScoreboardManager.getMainScoreboard.getObjective("GravityMeter").setDisplaySlot(DisplaySlot.SIDEBAR)
  }
  def oxyScoreBoard = {
    Bukkit.getScoreboardManager.getMainScoreboard.getObjective("Vitals").setDisplaySlot(DisplaySlot.SIDEBAR)
  }

  def onTabComplete(sender: Player, command: Command, alias: String, args: Array[String]): util.List[String] = command.getName match {
    case "updateprob" => args.size match {
      case 1 =>baseTasks.filter(_.name.toUpperCase().contains(args.head.toUpperCase())).map(_.name).toList.asJava
      case 2 => List("""0 < number < 1 """).asJava
      case _ => List().asJava
    }
    case "updatefreq" => args.size match {
      case 1 =>baseTasks.filter(_.name.toUpperCase().contains(args.head.toUpperCase())).map(_.name).toList.asJava
      case 2 => List("""numberOfTicks""").asJava
      case _ => List().asJava
    }
    case "addoxy" => args.size match {
      case 1 => List(""" amount:Integer """).asJava
      case _ => List().asJava
    }
    case "removeoxy" => args.size match {
      case 1 => List("""amount:Integer """).asJava
      case _ => List().asJava
    }
    case "gravitystrength" => args.size match {
      case 1 => List("""value > 0  (unless you want to)""").asJava
      case _ => List().asJava
    }
    case _ => List().asJava
  }
}

