package minecraft.events

import org.bukkit.scheduler.BukkitRunnable
import EventLoop._
import Typical.core.dataset._
import Typical.core.grammar._
object EventLoopTaskHandler {
case class EventLoopTask() extends BukkitRunnable{
  override def run(): Unit = for{
    em <- eventManager
  }yield {
    em.eventStack.foreach(f => f())
    em.eventStack.foreach( _ =>  em.eventStack.pop())
    em
  }
}
}
