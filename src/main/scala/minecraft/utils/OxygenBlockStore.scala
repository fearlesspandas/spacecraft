package minecraft.utils

import org.bukkit.Location
import org.bukkit.block.Block

object OxygenBlockStore {
  val store:scala.collection.concurrent.Map[Block,Boolean] = scala.collection.concurrent.TrieMap()

  def addlocations(locs:Block*):Unit = locs.foreach(l => store.update(l,true))
  def removelocations(locs:Block*):Unit = locs.foreach(l => store.remove(l))
  def contains(loc:Block):Boolean = store.getOrElse(loc,false)
}
