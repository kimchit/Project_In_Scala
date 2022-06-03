package exe4

  import exe4.Parsing._
  import exe4.Tokenizing._

  object Main{

    def main(args: Array[String]) ={
      println("please enter your directory")
      val pathName= scala.io.StdIn.readLine(); //get path name from the user
      Tokenizing.tokenizing(pathName)
      Parsing.parsing(pathName)
    }

}
