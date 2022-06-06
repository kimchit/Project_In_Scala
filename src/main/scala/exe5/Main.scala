package exe5

import exe5.{Parsing, Tokenizing}

object Main {
  def main(args: Array[String]) ={
    println("please enter your directory")
    val pathName= scala.io.StdIn.readLine(); //get path name from the user
    Tokenizing.tokenizing(pathName)
    Parsing.parsing(pathName)
  }

}
