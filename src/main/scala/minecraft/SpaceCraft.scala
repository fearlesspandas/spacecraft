package minecraft;

import events.{EventLoop, EventLoopListener, Events}
import minecraft.runnables.{EntitySpawnRunnable, OxygenHandler, RocketCharge, SpaceCraftRunnable}
import org.bukkit.Material
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.{EntityType, Player}
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.JavaConverters._
import runnables.SpaceCraftRunnable._

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
	val COMMAND_PROCESSORS:Seq[CommandProcessor] = Seq(
		DefaultCommandProcessor.defaultCommandProcessor,
		EntitySpawnRunnable.randomEventCommandProcessor,
		EntitySpawnRunnable.randomSphereCommandProcessor,
		EntitySpawnRunnable.runCmd,
		OxygenHandler.runCmd,
		RocketCharge.runCmd
	)
	val TAB_COMPLETERS:Seq[TabComplete] = Seq(
		DefaultTabComplete.defaultTabComplete,
		EntitySpawnRunnable.randomEventTabComplete,
		EntitySpawnRunnable.randomSphereTabComplete,
		EntitySpawnRunnable.tabComplete,
		OxygenHandler.tabComplete,
		RocketCharge.tabComplete
	)
  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] =
		TAB_COMPLETERS.foldLeft(List.empty[String])((res,processor) => try{res ++ processor(sender,cmd,label,args)}catch{case _ => res}).asJava


	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean =
		COMMAND_PROCESSORS.foldLeft(true)((res,processor) => try{res&& processor(sender,cmd,label,args)}catch{case _ => res})


}