package exe4

import scala.util.control.Breaks
import scala.util.matching.Regex

object main extends App{
  val myString = "atara"
  val KeyWordsPattern =List("atara","kimchit")

  KeyWordsPattern.find(x=>x == myString) match {
    case Some(_) => println("yes")
    case None => println("fail")
  }
}

