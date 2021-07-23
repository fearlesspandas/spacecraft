package minecraft.runnables.typicalModels
import Players._
import minecraft.runnables.typicalModels.PlayerEvents.{MonadicEvent, SpaceCraftPlayerEvent}
import org.bukkit.entity.{EnderDragon, Entity, EntityType, Mob, Phantom, Player}
import Typical.core.grammar._
import org.bukkit.ChatColor
import org.bukkit.potion.{PotionEffect, PotionEffectType}
import org.bukkit.scheduler.BukkitTask

import scala.math.random
object EntitySpawnModel {
  val radius = 10
  def randomNormal = if(random() > 0.5) 1 else -1
  def randomScaler = random()*radius
  def randomVec = Vector(randomScaler*randomNormal,randomScaler*randomNormal,randomScaler*randomNormal)
  def handleSpawnEvent(entity:EntityType,player:SpaceCraftPlayer,num:Int = 1,postProcess:Entity => Unit = (_ => ())):SpaceCraftPlayer = {
    player.sendMessage(s"${ChatColor.DARK_PURPLE}${entity.toString} are attacking")
    player.copy(
      postProcessing = () => {
        (0 to num).foreach(_ =>{
          val spawned = player.value.getWorld.spawnEntity(
            player.getLocation.add(randomScaler*randomNormal,randomScaler*randomNormal,randomScaler*randomNormal),
            entity
          )
          postProcess(spawned)
        }
          )}
    )
  }
  def fixEntityFlight(entity: Entity):Unit = entity match {
    case d:EnderDragon => d.setPhase(EnderDragon.Phase.SEARCH_FOR_BREATH_ATTACK_TARGET)
      d.addPotionEffect(new PotionEffect(PotionEffectType.GLOWING,Int.MaxValue,100))
      d.setSilent(true)
    case p:Mob =>
      p.addPotionEffect(new PotionEffect(PotionEffectType.GLOWING,Int.MaxValue,100))
      p.setSilent(true)
    case p:Player =>
      p.addPotionEffect(new PotionEffect(PotionEffectType.GLOWING,Int.MaxValue,100))
      p.addPotionEffect(new PotionEffect(PotionEffectType.SLOW_FALLING,Int.MaxValue,100))
      p.addPotionEffect(new PotionEffect(PotionEffectType.SPEED,Int.MaxValue,2))
      p.addPotionEffect(new PotionEffect(PotionEffectType.JUMP,Int.MaxValue,3))
  }
  case class DragonSpawnEvent(frequency:Double,probability:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override val name: String = "DragonSpawnEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))

    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = handleSpawnEvent(EntityType.ENDER_DRAGON,player,1,fixEntityFlight(_))
  }
  case class BlazeSpawnEvent(frequency:Double,probability:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override val name: String = "BlazeSpawnEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = handleSpawnEvent(EntityType.BLAZE,player,3,fixEntityFlight(_))
  }
  case class GhastSpawnEvent(frequency:Double,probability:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override val name: String = "GhastSpawnEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = handleSpawnEvent(EntityType.GHAST,player,2,fixEntityFlight(_))
  }
  case class PhantomSpawnEvent(frequency:Double,probability:Double,value:Option[BukkitTask] = None) extends MonadicEvent {
    override def setFrequency(frequency: Double): SpaceCraftPlayerEvent = this.copy(frequency = frequency)
    override def setProbability(probability: Double): SpaceCraftPlayerEvent = this.copy(probability = probability)
    override val name: String = "PhantomSpawnEvent"
    override def apply(bukkitTask: BukkitTask): SpaceCraftPlayerEvent = this.copy(value = Some(bukkitTask))
    override def apply(player: SpaceCraftPlayer): SpaceCraftPlayer = handleSpawnEvent(EntityType.PHANTOM,player,5,fixEntityFlight(_))
  }

}
