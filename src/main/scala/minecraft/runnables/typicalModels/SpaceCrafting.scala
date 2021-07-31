package minecraft.runnables.typicalModels
import PlayerEvents._
import org.bukkit.scheduler.BukkitTask
import Players._
import Typical.core.dataset._
import Typical.core.grammar._
import org.bukkit.{Material, NamespacedKey}
import org.bukkit.inventory.{ItemStack, ShapelessRecipe}
import org.bukkit.plugin.java.JavaPlugin
object SpaceCrafting {
  def createSiphon:ItemStack = {
    val siphon = new ItemStack(Material.BLAZE_ROD,1)
    val meta = siphon.getItemMeta
    meta.setDisplayName("Oxy Siphon")
    siphon.setItemMeta(meta)
    siphon
  }
  def createSpaceHelmet:ItemStack = {
    val siphon = new ItemStack(Material.DIAMOND_HELMET,1)
    val meta = siphon.getItemMeta
    meta.setDisplayName("Space Helmet")
    siphon.setItemMeta(meta)
    siphon
  }
   class OxySiphonRecipe(plugin:JavaPlugin) extends ShapelessRecipe(new NamespacedKey(plugin,"oxy_siphon"),createSiphon)
  object OxySiphonRecipe{
    def apply(plugin: JavaPlugin):ShapelessRecipe = (new OxySiphonRecipe(plugin)).addIngredient(1,Material.BLAZE_ROD).addIngredient(8,Material.DIAMOND)
  }
  class SpaceHelmetRecipe(plugin: JavaPlugin) extends ShapelessRecipe(new NamespacedKey(plugin,"space_helmet"),createSpaceHelmet)
  object SpaceHelmetRecipe{
    def apply(plugin: JavaPlugin): ShapelessRecipe= (new SpaceHelmetRecipe(plugin)).addIngredient(1,Material.LEATHER_HELMET).addIngredient(8,Material.DIAMOND_BLOCK)
  }

}
