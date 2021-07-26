package minecraft.utils

import org.bukkit.Material
import org.bukkit.Material._

object Surroundings {
  def isConcrete(mat:Material):Boolean = {
    mat.toString.toUpperCase().contains("CONCRETE")
  }
  def isGlass(mat:Material):Boolean = {
    mat.toString.toUpperCase.contains("GLASS")
  }
  def isClay(mat:Material):Boolean = {
    mat.toString.toUpperCase().contains("TERRACOTTA")
  }
  def materialDensity(m:Material):Double= m match {
    case mat if mat.isFuel => 0.5
    case DIRT | COARSE_DIRT | SAND | RED_SAND => 1
    case STONE | BRICKS | NETHER_BRICK | CLAY | COBBLESTONE | SANDSTONE | SANDSTONE_WALL  => materialDensity(DIRT) * 2
    case COAL_ORE => materialDensity(STONE)/2
    case LAPIS_ORE => materialDensity(STONE) * 1.5
    case IRON_ORE => materialDensity(STONE) * 2
    case GOLD_ORE | SOUL_SAND => materialDensity(IRON_ORE) * 2
    case DIAMOND_ORE => materialDensity(GOLD_ORE) * 2
    case REDSTONE_ORE => materialDensity(DIAMOND_ORE) * 2
    case COAL_BLOCK => materialDensity(COAL_ORE) * 9
    case LAPIS_BLOCK => materialDensity(LAPIS_ORE) * 9
    case IRON_BLOCK => materialDensity(IRON_ORE)*9
    case GOLD_BLOCK => materialDensity(GOLD_ORE) * 9
    case DIAMOND_BLOCK => materialDensity(DIAMOND_ORE) * 9
    case REDSTONE_BLOCK => materialDensity(REDSTONE_ORE) * 64
    case OBSIDIAN =>   100
    case BEDROCK => 50
    case _ if(isConcrete(m)) => materialDensity(STONE)
    case _ if(isClay(m)) => materialDensity(CLAY)
    case _ if(isGlass(m)) => materialDensity(SAND) * 1.5
    case _ if m.isSolid => 1
    case _ => 0


  }
}
