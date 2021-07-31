package minecraft

import java.util

import minecraft.events.EventLoop._
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import Typical.core.grammar._
import Typical.core.dataset._
import minecraft.events.EventLoop
import minecraft.events.EventLoopTaskHandler.{GravityMeter, Vitals}
import minecraft.runnables.typicalModels.EventManager.EventManager
import minecraft.runnables.typicalModels.OxygenReplenishEvent.OxygenReplenishEvent
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.PlayerGravityModel.PlayerGravityEvent
import minecraft.utils.ItemCommands
import minecraft.utils.MinecartController.MinecartControlModel
import org.bukkit.inventory.ItemStack
import org.bukkit.{Bukkit, Location, Material}

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
      case "gravityoff" =>
        updateProb(0,gravityEvent.name,sender)
        updateProb(0,gravityEvent2.name,sender)
        updateProb(0,gravityEvent3.name,sender)
        eventManager
      case "gravityon" =>
        updateProb(1,gravityEvent.name,sender)
        updateProb(1,gravityEvent2.name,sender)
        updateProb(1,gravityEvent3.name,sender)
        eventManager
      case "addcmd" =>
        val cmdSeq = args.foldLeft("")(_ + " " +  _).split(';').map(cmd => cmd.dropWhile(_.isWhitespace))
        val item = sender.getInventory.getItemInMainHand.getType
        ItemCommands.addCommand(sender,item,cmdSeq)
        eventManager
      case "removecmd" =>
        args.size match {
          case 1 =>
            val item = Material.getMaterial(args(0).toUpperCase())
            ItemCommands.removeCommand(sender,item)
          case _ =>
            val item = sender.getInventory.getItemInMainHand.getType
            ItemCommands.removeCommand(sender,item)
        }
        eventManager
      case "addwaypoint" =>
        args.size match {
          case 3 =>
            val coords = args.map(_.toInt)
            val (x,y,z) = (coords.head,coords.tail.head,coords.tail.tail.head)
            addWayPoint(sender)(x,y,z)
          case _ => eventManager
        }
      case "showwaypoints" =>
        showWaypoints(sender)
      case "clearwaypoints" =>
        clearWaypoints(sender)
      case "givesiphon" =>
        giveOxySiphon(sender)
        eventManager
      case cmd => sender.sendMessage(s"No command found for ${cmd}")
        eventManager
    }
    def giveOxySiphon(player:Player):Unit = {
      val siphon = new ItemStack(Material.BLAZE_ROD,1)
      val meta = siphon.getItemMeta
      meta.setDisplayName("Oxy Siphon")
      if(siphon.setItemMeta(meta)) player.getInventory.addItem(siphon)
    }
    def clearWaypoints(player:Player):dataset[EventManager] = for{
      spcplayer <- eventManager.getTask(player,minecartController.name).player
      task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
    }yield{
      val mctask = task.asInstanceOf[MinecartControlModel]
      eventManager.updateEvent(mctask.copy(waypoints = Seq()),spcplayer,plugin)
    }
    def showWaypoints(player:Player):dataset[EventManager]= for{
      task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
    }yield{
      val mctask = task.asInstanceOf[MinecartControlModel]
      player.sendMessage(s"Waypoints:${mctask.waypoints.foldLeft("")(_ + _)}")
      eventManager
    }
    def addWayPoint(player:Player)(x:Int,y:Int,z:Int):dataset[EventManager] = for{
      spcplayer <- eventManager.getTask(player,minecartController.name).player
      task <- eventManager.getTask(player,minecartController.name).<--[SpaceCraftPlayerEvent]
    }yield{
      val mctask = task.asInstanceOf[MinecartControlModel]
      val loc = new Location(player.getWorld,x,y,z)
      eventManager.updateEvent(mctask.copy(waypoints =  mctask.waypoints :+ loc),spcplayer,plugin)
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
    val target = Seq(e1,e2,e3)
      .map(
        p => p.knownBlocks.headOption.map(_.getLocation)
      ).sortWith(
      (a,b) => a.exists(l => b.exists(bl => l.distance(player.getLocation) < bl.distance(player.getLocation)))
    )
    player.setCompassTarget(target.head.getOrElse(player.getLocation))
    em
  }
  def gravScoreBoard = {
    Bukkit.getScoreboardManager.getMainScoreboard.getObjective(GravityMeter.name).setDisplaySlot(DisplaySlot.SIDEBAR)
  }
  def oxyScoreBoard = {
    Bukkit.getScoreboardManager.getMainScoreboard.getObjective(Vitals.name).setDisplaySlot(DisplaySlot.SIDEBAR)
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
    case "addcmd" => args.size match {
      case n if n > 0 => List(
        "player commands seperated by ;"
      ).asJava
      case _ => List().asJava
    }
    case "removecmd" => args.size match {
      case 1 => ItemCommands.itemCommands.filterKeys(k => k._1 == sender.getUniqueId).toList.map(p => p._1._2.toString).asJava
      case _ => List().asJava
    }
    case "addwaypoint" =>
      args.size match {
        case 1 => List("coordinate:x").asJava
        case 2 => List("coordinate:y").asJava
        case 3 => List("coordinate:z").asJava
      }
    case _ => List().asJava
  }
}

