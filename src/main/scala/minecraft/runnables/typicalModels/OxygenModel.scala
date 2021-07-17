package minecraft.runnables.typicalModels

import java.time.LocalTime

import Typical.core.dataset._
import Typical.core.grammar._
import io.circe.generic.JsonCodec
import minecraft.runnables.typicalModels.PlayerEvents.SpaceCraftPlayerEvent
import minecraft.runnables.typicalModels.Players.SpaceCraftPlayer
import org.bukkit.{ChatColor, Material}

import scala.reflect.runtime.universe.TypeTag
object OxygenModel {
  import io.circe._
  import io.circe.generic.auto._
  import io.circe.generic.semiauto._
  implicit val decoderMat:Decoder[Material] = Decoder.forProduct1("name")(Material.getMaterial)
  implicit val encoderMat:Encoder[Material] = Encoder.forProduct1("name")(m => (m.toString))
  implicit val decoderOxy:Decoder[OxygenDepletionModel] = Decoder.forProduct4("startingMax","siphonAmt","oxyConverters","breadthDelay")(OxygenDepletionModel.apply)
  implicit val encodeOxy:Encoder[OxygenDepletionModel] = Encoder.forProduct4("startingMax","siphonAmt","oxyConverters","breadthDelay")(o => (o.startingMax,o.siphonAmt,o.oxyConverters,o.breadthDelay))
  case class OxygenDepletionModel(startingMax: Int, siphonAmt:Int  , oxyConverters:Seq[Material] , breadthDelay:Double ) extends SpaceCraftPlayerEvent {
    val STARTING_MAX = startingMax
    val SIPHON_AMT = siphonAmt
    val oxyconverters: Seq[Material] = oxyConverters
    val frequency = breadthDelay
    val probability = 1

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
    def handleOxy:dataset[A] = (src +- OxygenDepletionModel(100,3,Seq(Material.BLUE_ICE),5)).-->[OxygenDepletionModel]
  }

}

