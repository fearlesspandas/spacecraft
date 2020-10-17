package minecraft;

import events.{EventLoop, EventLoopListener, Events}
import minecraft.runnables.OxygenHandler
import org.bukkit.Material
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin



object SpaceCraft{

}

class SpaceCraft extends JavaPlugin{



  override def onEnable() {
		getLogger().info("Enabling SpaceCraft!");
		new EventLoopListener(this)
	}

	override def onDisable() {
		getLogger().info("Shutting down SpaceCraft");
//    val outpath = "plugins/orderdata/orders.json"
//    val append = false
//    try{
//      val pw = new PrintWriter(new FileOutputStream(outpath,append))
//      if (append) pw.append("res") else pw.write("res")
//      pw.close()
//    }catch{
//      case e:Exception => e.printStackTrace()
//    }
	}

//  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
//		val res = cmd.getName() match {
//			case "setfreq" =>
//				args.size match {
//					case 0 =>
//				}
//  	}
//		//(new java.util.ArrayList()).add()
//	}

	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {
   cmd.getName() match {
		 case "setfreq" =>
			 val eventName = args(0)
			 val eventType = Events.withName(eventName)
			 val newvalue = args(1).toInt
			 EventLoop.frequencyMap.update(eventType,newvalue)
			 sender.sendMessage(s"Set $eventType probability to ${EventLoop.frequencyMap.getOrElse(eventType,0l).toString} (this requires a restart to take effect)")
			 true
		 case "setprob" =>
			 val eventName = args(0)
			 val eventType = Events.withName(eventName)
			 val newvalue = args(1).toDouble
			 EventLoop.probabilityMap.update(eventType,newvalue)
			 sender.sendMessage(s"Set $eventType frequency to ${EventLoop.probabilityMap.getOrElse(eventType,0l).toString}")
			 true
		 case "setoxygen" =>
			 sender match {
				 case player:Player if args.size > 0 =>
					 OxygenHandler.oxygenMap.update(player.getUniqueId,args(0).toInt)
					 true
			 }
		 case "readfreq" =>
			 val eventName = args(0)
			 val eventType = Events.withName(eventName)
			 sender.sendMessage(EventLoop.frequencyMap.getOrElse(eventType,0l).toString)
			 true
		 case "readprob" =>
			 val eventName = args(0)
			 val eventType = Events.withName(eventName)
			 sender.sendMessage(EventLoop.probabilityMap.getOrElse(eventType,0l).toString)
			 true
		 case "disablespc" =>
			 EventLoop.disabled = true
			 true
		 case "disableEvent" => sender match {
			 case player:Player => if(args.size > 0) {
				 val eventType = Events.withName(args(0))
				 EventLoop.disabledMap.update((eventType,player),true)
				 player.sendMessage(s"Set disable $eventType to ${EventLoop.disabledMap((eventType,player))}")
			 }
		 }
			 true
		 case "enableEvent" => sender match {
			 case player:Player => if(args.size > 0) {
				 val eventType = Events.withName(args(0))
				 EventLoop.disabledMap.update((eventType,player),false)
				 player.sendMessage(s"Set disable $eventType to ${EventLoop.disabledMap((eventType,player))}")
			 }
		 }
			 true
		 case "setcmd" =>
			 sender match{
				 case p:Player =>
					 val mat = p.getInventory.getItemInMainHand.getType
					 val concatargs = if(args.size < 1) "" else args.drop(1).foldLeft(args(0))(_ + " " + _)
					 EventLoop.cmdmap.update((mat,p),concatargs)
					 p.sendMessage(s"Command set:$concatargs")
					 true
			 }

	 }
	}

}