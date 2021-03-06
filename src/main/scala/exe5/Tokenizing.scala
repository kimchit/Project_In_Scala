package exe5
//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782

import java.io._
import java.nio.file.{Files, Paths}
import scala.io.Source

object Tokenizing extends App {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  //println("please enter your directory")
  //val pathName= scala.io.StdIn.readLine(); //get path name from the user

  def tokenizing(pathName: String): Unit = {
    var directory: File = null
    var printWriter: PrintWriter = null
    var fileObject: File = null
    val bool = Files.exists(Paths.get(pathName))
    try {
      if (!bool)
        throw new FileNotFoundException()
      directory = new File(pathName)
      //loop over files, choose whichever end with .jack
      recursiveListFiles(directory).filter(_.getName.endsWith(".jack")).foreach // loops over files in "directory"
      {
        file =>
          val name = (file.getName.split("\\\\").last).split(".jack").last; //split name of directory and take the last name of package
          //create outPut file and save in fileObject
          fileObject = new File(directory, "My" + name + "T.xml")
          printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
          printWriter.write("<tokens>\n");
          var sourceFileInString = ""
          Source.fromFile(file.getAbsolutePath).getLines().foreach { line =>
            sourceFileInString += (line.mkString + "\n")
          }
          printWriter.write(TokenizingFunctions.start(sourceFileInString));
          printWriter.write("</tokens>\n");
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
