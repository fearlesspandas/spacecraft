package minecraft.runnables

import minecraft.events.EventLoop._
import minecraft.runnables.SpaceCraftRunnable.{CommandProcessor, TabComplete}
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import org.bukkit.scheduler.BukkitRunnable
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scala.io.Source


object SpaceCraftRunnable{
  type CommandProcessor = (CommandSender,Command,String, Array[String]) => Boolean
  type TabComplete = (CommandSender,Command,String, Array[String]) => List[String]
}
trait RunnableCompanion[A<:SpaceCraftRunnable] {
  def apply(player:Player):A
  val runCmd:CommandProcessor = (sender,cmd,label,args)=>apply(sender.asInstanceOf[Player]).runCmd(sender,cmd,label,args)
  val tabComplete:TabComplete =  (sender,cmd,label,args)=>apply(sender.asInstanceOf[Player]).tabComplete(sender,cmd,label,args)
}
trait SpaceCraftRunnable extends BukkitRunnable{
//  type Settings
//  val settingsLoc : String
//  val settingsDecoder = decode[Settings](Source.fromFile(settingsLoc).getLines().foldLeft("")(_ + _))
  val eventType:Events.Events
  val player:Player
  def runner():Unit
  override def run(): Unit = try {if(shouldRun(eventType,player)) runner()}catch{
    case e:Exception =>
      println(s"$eventType event exception")
      e.printStackTrace()
  }

  val runCmd:CommandProcessor = (sender,cmd,_,args) => cmd.getName match {
    case "runevent" =>
      args match {
        case _ if args.size == 1 =>
          val event = Events.withName(args.head)
          val player = sender match { case p:Player => p}
          event match {
            case this.eventType => runnerMap(this.eventType)(player).run()
          }
      }
      true
    case _ => true
  }
  val tabComplete:TabComplete = (sender,cmd,_,args) => cmd.getName() match {
    case "runevent" if args.size == 1  &&
      this.eventType.toString.contains(args(0)) =>
      List(this.eventType.toString)
    case _ => List()
  }
}
