package minecraft.IO

import events.Events

import scala.io.Source
import scala.collection.mutable.Map
import scala.util.matching.Regex
object SettingsDecoder {
  val probfileloc = "plugins/spacecraft/probabilities.json"
  val freqfileloc = "plugins/spacecraft/frequencies.json"
  def processString(str:String) = str
    .split(":").map(substr =>
    substr.filterNot(_ == ",")
    .filter(c => c.isLetterOrDigit  || c == ".")
  )


  def getProbSettings(loc:String) = {
    val lines = Source.fromFile(loc).getLines().toSeq
    lines.foldLeft(Map[Events.Events, Double]())((valmap, line) => valmap ++ (line match {
      case "{" => Map.empty[Events.Events, Double]
      case "}" => Map.empty[Events.Events, Double]
      case str => println(str)
        val pair = processString(str)
        pair.foreach(println(_))
        if (pair.size == 2) Map[Events.Events, Double](Events.withName(pair(0)) -> pair(1).toDouble) else Map.empty[Events.Events, Double]
    }))
  }
  def getFreqSettings(loc:String) = {
    val lines = Source.fromFile(loc).getLines().toSeq
    lines.foldLeft(Map[Events.Events,Long]())( (valmap,line) => valmap ++ (line match {
      case "{" => Map.empty[Events.Events,Long]
      case "}" => Map.empty[Events.Events,Long]
      case str => println(str)
        val pair = processString(str)
        if(pair.size == 2) Map[Events.Events,Long](Events.withName(pair(0)) -> pair(1).toLong) else Map.empty[Events.Events,Long]
    }))
  }
  def writeMap[A,B](m:Map[A,B]):String = {
    "{\n" +
      m.toSeq.map(p => s"${p._1} : ${p._2},\n").foldLeft("")(_ + _) +
    "}"
  }

//  def main(args: Array[String]): Unit = {
//    val probfile = "src/main/resources/probabilities.json"
//    println(getProbSettings(probfile))
//  }
}
