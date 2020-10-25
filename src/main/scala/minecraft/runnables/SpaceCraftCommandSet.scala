package minecraft.runnables

import org.bukkit.command.{Command, CommandSender}

trait SpaceCraftCommandSet {
  def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean
  def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : List[String]
}
