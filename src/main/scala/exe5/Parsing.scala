package exe5


import exe5.TokenizingFunctions._

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source

object Parsing extends App {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  // println("please enter your directory")
  //val pathName = scala.io.StdIn.readLine(); //get path name from the user

  def parsing(pathName: String): Unit = {
    //declare the variables before the "try"
    var directory: File = null
    var printWriter: PrintWriter = null
    var fileObject: File = null
    val bool = Files.exists(Paths.get(pathName))
    //  var str: String=null
    // var temp:String=""
    try {
      if (!bool)
        throw new FileNotFoundException()
      directory = new File(pathName)
      //loop over files, choose whichever end with T.xml
      recursiveListFiles(directory).filter(_.getName.endsWith("T.xml")).foreach // loops over files in "directory"
      {
        file =>

          val name = (file.getName.split("\\\\").last).split("T.xml").last; //split name of directory and take the last segment
          fileObject = new File(directory, name + ".vm")
          printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
          var sourceFileTokens: List[Token] = List[Token]()
          Source.fromFile(file.getAbsolutePath).getLines().foreach { line =>
            val token = line.split(" ")
            if (token(0) != "<tokens>" && token(0) != "</tokens>") {
              var content=""
              for(i<-1 to token.length-2) {
                content+=token(i)}
              sourceFileTokens :+= new Token(token(0), content, token(token.length-1) + "\n")

            }
          }
          VM_Writer.Contsructor(fileObject);
          ParsingFunctions.start(sourceFileTokens);
          sourceFileTokens=List[Token]()
          printWriter.close();
      }
    }

    catch {
      case e: FileNotFoundException => println("Couldn't find this directory");
      case e: IOException => println("Had an IOException trying to read that file");
      case e: IllegalArgumentException => println("invalid command in input file");
    }
  }
}