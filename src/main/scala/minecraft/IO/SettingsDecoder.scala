package minecraft.IO

import events.Events
import org.bukkit.Material

import scala.collection.mutable.Map
import scala.io.Source
object SettingsDecoder {
  val probfileloc = "plugins/spacecraft/probabilities.json"
  val freqfileloc = "plugins/spacecraft/frequencies.json"

  implicit class ArgsConverter(str:String) {
    def removeSpecialChars(str:String):String = str.filter(c => c.isLetterOrDigit || c == '.')
    def processLong(str:String): Long = removeSpecialChars(str).toLong
    def processDouble(str:String):Double = removeSpecialChars(str).toDouble
    def processEventString(str:String):Events.Events = Events.withName(removeSpecialChars(str))
    def processMaterial(str:String):Material = Material.getMaterial(removeSpecialChars(str).toUpperCase())
  }
  def processInput[A,B](str:String)(implicit fa:String => A, fb:String => B):(A,B) = {
    val splitstr = str.split(":")
    (fa(splitstr(0)),fb(splitstr(1)))
  }
  def getFreqSettings(loc:String):Map[Events.Events,Long] = readSettings[Events.Events,Long](loc)(loc.processEventString(_),loc.processLong(_))
  def getProbSettings(loc:String):Map[Events.Events, Double] = readSettings[Events.Events,Double](loc)(loc.processEventString(_),loc.processDouble(_))
  def readSettings[A,B](loc:String)(implicit fa:String => A,fb:String => B):Map[A,B] = try{
    val lines = Source.fromFile(loc).getLines().toSeq
    lines.foldLeft(Map[A,B]())( (valmap,line) => valmap ++ (line match {
      case "{" => Map.empty[A,B]
      case "}" => Map.empty[A,B]
      case str =>
        val pair = processInput[A,B](str)
        Map[A,B](pair)
    }))
  }catch{
    case e:Exception => println("Error while parsing settings")
      e.printStackTrace()
      Map()
  }
  def writeMap[A,B](m:Map[A,B]):String = {
    "{\n" +
       m.toSeq.map(p => s"  ${p._1} : ${p._2},\n").foldLeft("")(_ + _) +
    "}"
  }


//  def main(args: Array[String]): Unit = {
//    val probfile = "src/main/resources/probabilities.json"
//    val freqfile = "src/main/resources/frequencies.json"
//    val res  = getProbSettings(probfile)
//    //getFreqSettings(freqfile)
//    //Events.withName("".removeSpecialChars("'SpawnEvent'"))
//    val res2 = //processString("thing:0.2")
//   //readSettings2
//    //res.foreach(println(_))
//    println(res)
//  }
}
