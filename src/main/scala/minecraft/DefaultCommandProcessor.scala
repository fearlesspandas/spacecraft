package minecraft

import minecraft.events.EventLoop._
import minecraft.events.EventLoop.Events._
import minecraft.runnables.{EntitySpawnRunnable, OxygenHandler}
import minecraft.runnables.SpaceCraftRunnable.{CommandProcessor, TabComplete}
import org.bukkit.Material
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player

object DefaultCommandProcessor {
  val defaultCommandProcessor:CommandProcessor = (sender:CommandSender,cmd:Command,_,args:Array[String]) => cmd.getName() match {
    case "setfreq" =>
      val eventName = args(0)
      val eventType = Events.withName(eventName)
      val newvalue = args(1).toInt
      frequencyMap.update(eventType,newvalue)
      sender.sendMessage(s"Set $eventType frequency to ${frequencyMap.getOrElse(eventType,0l).toString} (this requires a restart to take effect)")
      true
    case "setprob" =>
      val eventName = args(0)
      val eventType = Events.withName(eventName)
      val newvalue = args(1).toDouble
      probabilityMap.update(eventType,newvalue)
      sender.sendMessage(s"Set $eventType probability to ${probabilityMap.getOrElse(eventType,0l).toString}")
      true
    case "readfreq" =>
      val eventName = args(0)
      val eventType = Events.withName(eventName)
      sender.sendMessage(frequencyMap.getOrElse(eventType,0l).toString)
      true
    case "readprob" =>
      val eventName = args(0)
      val eventType = Events.withName(eventName)
      sender.sendMessage(probabilityMap.getOrElse(eventType,0l).toString)
      true
    case "disablespc" =>
      disabled = true
      true
    case "disableEvent" => sender match {
      case player:Player => if(args.size > 0) {
        val eventType = Events.withName(args(0))
        disabledMap.update((eventType,player),true)
        player.sendMessage(s"Set disable $eventType to ${disabledMap((eventType,player))}")
      }
    }
      true
    case "enableEvent" => sender match {
        case player:Player => if(args.size > 0) {
          val eventType = Events.withName(args(0))
          disabledMap.update((eventType,player),false)
          player.sendMessage(s"Set disable $eventType to ${disabledMap((eventType,player))}")
        }
      }
      true
    case "setcmd" =>
      sender match{
        case p:Player =>
          val mat = p.getInventory.getItemInMainHand.getType
          val concatargs = if(args.size < 1) "" else args.drop(1).foldLeft(args(0))(_ + " " + _)
          cmdmap.update((mat,p),concatargs)
          p.sendMessage(s"Command set:$concatargs")
          true
      }
    case "addloot" =>
      args.size match{
        case 2 =>
          val mat = Material.getMaterial(args(0).toUpperCase())
          val prob = args(1).toDouble
          EntitySpawnRunnable.spawnFieldProbs.update(Left(mat),prob)
          true
        case _ => true
      }
    case "setgravity" =>
      args.size match{
        case 1 =>
          val b = args(0).toBoolean
          sender match {
            case p:Player => p.setGravity(b)
              true
          }
        case _ => true
      }
    case _ => true
  }
}

object DefaultTabComplete {
  import scala.collection.JavaConverters._
  val defaultTabComplete:TabComplete = (sender:CommandSender,cmd:Command,_,args:Array[String]) => cmd.getName() match {
    case "setfreq" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case 2 => List("num_ticks > 0")
        case _ => List()
      }
    case "setprob" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case 2 => List("0< probability <1")
        case _ => List()
      }
    case "readfreq" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case _ => List()
      }
    case "readprob" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case _ => List()
      }
    case "disableEvent" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case _ => List()
      }
    case "enableEvent" =>
      args.size match {
        case 1 => Events.values.map(_.toString).filter(_.contains(args(0))).toList
        case _ => List()
      }
    case "setcmd" =>
      args.size match {
        case 1 => List("[command]")
        case _ => List()
      }
    case "addloot" =>
      args.size match{
        case 1 => Material.values.filter(_.toString.contains(args(0).toUpperCase())).map(_.toString).toList
        case 2 => List("0 < prob < 1")
        case _ => List()
      }
  }
}
