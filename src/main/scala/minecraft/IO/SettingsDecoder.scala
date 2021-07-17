package minecraft.IO

import minecraft.events.EventLoop._
import io.circe.Decoder.Result
import org.bukkit.Material

import scala.collection.mutable.Map
import scala.io.Source
import io.circe._
//import io.circe.generic.auto._
//import io.circe.generic.semiauto._
//import io.circe.parser._
//import io.circe.syntax._
import io.circe.generic.JsonCodec, io.circe.syntax._

import minecraft.runnables.typicalModels.OxygenModel._
object SettingsDecoder {
  val probfileloc = "plugins/spacecraft/probabilities.json"
  val freqfileloc = "plugins/spacecraft/frequencies.json"

  implicit class ArgsConverter(str:String) {
    def removeSpecialChars(str:String)(regx:Seq[Char => Boolean] = Seq(_.isLetterOrDigit,_ == '.')):String = str.filter(c => regx.foldLeft(false)((b,r) => b || r(c)))
    def processLong(str:String): Long = removeSpecialChars(str)().toLong
    def processDouble(str:String):Double = removeSpecialChars(str)().toDouble
    def processEventString(str:String):Events.Events = Events.withName(removeSpecialChars(str)())
    def processMaterial(str:String):Material = Material.getMaterial(removeSpecialChars(str)().toUpperCase())
    def processSeq[A](str:String)(implicit f:String => A):Seq[A] = removeSpecialChars(str)(Seq(
      _.isLetterOrDigit,_ == '.',_ == ","
    )).split(",").map(f(_))
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
       m.toSeq.tail.map(p => s"  ${p._1} : ${p._2},\n").foldLeft("")(_ + _) +
      m.headOption.fold("")( p => s"  ${p._1} : ${p._2}\n") +
    "}"
  }

//  implicit val decodeEvents = KeyDecoder[Events] = null
//    new KeyDecoder[Events] {
//    override def apply(key: String): Option[Events] = Some(Events.withName(key))
//  }

//  implicit val decodeMapEnums:Decoder[scala.collection.immutable.Map[Events,Double]] = Decoder.decodeMap[Events,Double]
//
//  implicit def decoderEnums[E<:Events.Events](enum:E) = Decoder.decodeEnumeration(enum)
  implicit val decodeConfig:Decoder[config] = new Decoder[config] {
  override def apply(c: HCursor): Result[config] = for{
     eventName <- c.downField("event").as[String]
     event = Events.withName(eventName)
     prob <- c.downField("prob").as[Double]
  }yield  config(event,prob)
}
  case class config(event:Events.Events,prob:Double)
//  def read2(str:String) = {
//    val raw = Source.fromFile(str).getLines().toSeq.map( str => {
//      val json = parse(str).getOrElse(Json.Null)
//      json.as[config]
//    })
//      .map(r => r.right.get)
//    raw
//  }

  def main(args: Array[String]): Unit = {
    val probfile = "src/main/resources/probabilities.json"
    val freqfile = "src/main/resources/frequencies.json"
    val res  = getProbSettings(probfile)
    val testOxy =
      """{
        |"startingMax":100,
        |"siphonAmt":10,
        |"oxyConverters" : [],
        |"breadthDelay":5
        |}""".stripMargin
    //getFreqSettings(freqfile)
    //Events.withName("".removeSpecialChars("'SpawnEvent'"))
    //val res2 = read2(probfile)
   //readSettings2
    //res.foreach(println(_))
    //println(OxygenDepletionModel(100,5,Seq(),10).asJson.as[OxygenDepletionModel].right.get.asJson)
    println(res)
    println(writeMap(res))
  }
}
