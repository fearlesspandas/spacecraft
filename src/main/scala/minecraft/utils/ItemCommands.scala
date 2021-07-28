package minecraft.utils

import java.util.UUID

import org.bukkit.Material
import org.bukkit.entity.Player

import scala.collection.concurrent.TrieMap

object ItemCommands {
  type PlayerId = UUID
  val itemCommands:scala.collection.concurrent.Map[(PlayerId,Material),Seq[String]] = TrieMap()
  def addCommand(player:Player,item:Material,cmd:Seq[String]):Unit = {
    player.sendMessage(s"Adding commands ${cmd}")
    itemCommands.update((player.getUniqueId,item),cmd)
  }
  def getCommand(player:Player,item:Material):Seq[String] = itemCommands.getOrElse((player.getUniqueId,item),Seq())
  def runCommand(player:Player,item:Material):Unit = getCommand(player,item).foreach(player.performCommand(_))
  def removeCommand(player:Player,item:Material):Unit = itemCommands.remove((player.getUniqueId,item))
}
