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
  implicit val decoderOxy:Decoder[OxygenDepletionModel] = Decoder.forProduct2("siphonAmt","breadthDelay")(OxygenDepletionModel.serialize)
  implicit val encodeOxy:Encoder[OxygenDepletionModel] = Encoder.forProduct2("siphonAmt","breadthDelay")(o => (o.siphonAmt,o.breadthDelay))
  object OxygenDepletionModel{
    def serialize(siphonAmt:Int, breadthDelay:Double):OxygenDepletionModel = {
      OxygenDepletionModel(siphonAmt,breadthDelay,None)
    }
  }
  case class OxygenDepletionModel(siphonAmt:Int, breadthDelay:Double ,value:Option[BukkitTask]) extends SpaceCraftPlayerEvent {
    val SIPHON_AMT = siphonAmt
    val frequency = breadthDelay
    val probability = 1

    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = {
      println("Doing thing")
      player.value.sendMessage("oxy remaining: " + player.oxygenRemaining)
      if (
        player.isOnline && !player.isOnGround
      ) {
        if (
          player.getInventory.getHelmet != null &&
            player.getInventory.getHelmet.getType == Material.DIAMOND_HELMET
        ) {
          val remaining = player.oxygenRemaining
          if (remaining <= 0 ) {
            player.value.sendMessage(s"${ChatColor.LIGHT_PURPLE} No oxy remaining")
            val postProcess = () => player.value.damage(player.getHealthScale / 2)
            player.copy(postProcessing = postProcess)
          }
          else {
              val res = player.copy(oxygenRemaining = player.oxygenRemaining - siphonAmt,postProcessing = () => ())
              player.value.sendMessage(s"${ChatColor.AQUA} oxy:${res.oxygenRemaining}")
              res
          }
        } else {

          val postProcess = () => {
            player.value.sendMessage("Damaged by Process")
            player.value.damage(player.getHealth/2)
          }
          player.value.sendMessage("Put on a diamond helmet or you'll suffocate")
          player.copy(postProcessing = postProcess)
        }
      }else{
        player.value.sendMessage("nothing to do")
        player.copy(postProcessing = () => ())
      }
    }

    override val name: String = "OxygenDiminishEvent"


    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = OxygenDepletionModel(this.siphonAmt,frequency,this.value)

    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this
  }

  implicit class OxygenModelGrammar[A<:SpaceCraftPlayer](src:dataset[A])(implicit taga:TypeTag[A]){
    def handleOxy:dataset[A] = (src +- OxygenDepletionModel(3,5,None)).-->[OxygenDepletionModel]
  }

}

