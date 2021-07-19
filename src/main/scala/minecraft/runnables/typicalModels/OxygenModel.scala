package minecraft.runnables.typicalModels

import java.time.LocalTime

import Typical.core.dataset._
import Typical.core.grammar._
import io.circe.generic.JsonCodec
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.scheduler.BukkitTask
import org.bukkit.{ChatColor, Material}

import scala.reflect.runtime.universe.TypeTag
object OxygenModel {
  import io.circe._
  import io.circe.generic.auto._
  import io.circe.generic.semiauto._
  implicit val decoderOxy:Decoder[OxygenDepletionModel] = Decoder.forProduct3("startingMax","siphonAmt","breadthDelay")(OxygenDepletionModel.serialize)
  implicit val encodeOxy:Encoder[OxygenDepletionModel] = Encoder.forProduct3("startingMax","siphonAmt","breadthDelay")(o => (o.startingMax,o.siphonAmt,o.breadthDelay))
  object OxygenDepletionModel{
    def serialize(startingMax: Int, siphonAmt:Int, breadthDelay:Double):OxygenDepletionModel = {
      OxygenDepletionModel(startingMax,siphonAmt,breadthDelay,None)
    }
  }
  case class OxygenDepletionModel(startingMax: Int, siphonAmt:Int, breadthDelay:Double ,value:Option[BukkitTask]) extends SpaceCraftPlayerEvent {
    val STARTING_MAX = startingMax
    val SIPHON_AMT = siphonAmt
    val frequency = breadthDelay
    val probability = 1

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
      println("Doing thing")
      player.value.sendMessage("oxy remaining: " + player.oxygenRemaining)
      if (
        player.isOnline && player.isGliding
      ) {
        if (
          player.getInventory.getHelmet != null &&
            player.getInventory.getHelmet.getType == Material.DIAMOND_HELMET
        ) {
          val remaining = player.oxygenRemaining
          if (remaining <= 0 ) {
            player.value.sendMessage(s"${ChatColor.LIGHT_PURPLE} No oxy remaining")
            player.value.damage(player.getHealthScale / 4)
            player
          }
          else {
              val res = player.copy(oxygenRemaining = player.oxygenRemaining - siphonAmt,lastBreadth = LocalTime.now())
              player.value.sendMessage(s"${ChatColor.AQUA} oxy:${res.oxygenRemaining}")
              res
          }
        } else {
          player.damage(player.getHealthScale / 2)
          player.value.sendMessage("Put on a diamond helmet or you'll suffocate")
          player
        }
      }else{
        player.value.sendMessage("nothing to do")
        player
      }
    }

    val addOxyTabComplete:PartialFunction[(String, Int), List[String]] =  {
      case ("addOxy",0) => List("integer > 0")
    }
    override val name: String = "OxygenDiminishEvent"
    override val commandProcessor: Seq[PartialFunction[(String, Array[String]), Boolean]] = Seq()
    override val tabComplete: Seq[PartialFunction[(String, Int), List[String]]] = Seq(
      addOxyTabComplete
    )
  }

  implicit class OxygenModelGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def handleOxy:dataset[A] = (src +- OxygenDepletionModel(100,3,5,None)).-->[OxygenDepletionModel]
  }

}

