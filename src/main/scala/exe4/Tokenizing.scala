package exe4;
//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782
import java.io._
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.io.Source._
import scala.util.control.Breaks


object Tokenizing {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def main(args: Array[String])={
  // val directory;
  println("please enter your directory")
  val pathName= scala.io.StdIn.readLine(); //get path name from the user C:\Users\user\Desktop\nand2tetris\nand2tetris\projects\07\MemoryAccess\BasicTest
  var directory:File=null
  var printWriter:PrintWriter=null
  var fileObject:File=null
  val bool= Files.exists(Paths.get(pathName))
  var str: String=null;
  var temp:String=""
  try
  {
    if(!bool)
      throw new FileNotFoundException ()
    directory = new File(pathName)
    //loop over files, choose whichever end with vm
    recursiveListFiles(directory).filter(_.getName.endsWith(".jack")).foreach// loops over files in "files"
    {
      file=>
        val name= (file.getName.split("\\\\").last).split(".jack").last; //split name of directory and take the last name of package
        //create outPut file and save in fileObject
        //fileObject= new File( name+".asm") //new file to the output
        fileObject=new File(directory,"My"+name+"T.xml")
        printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
        printWriter.write("<tokens>\n");
        var sourceFileInString=""
        Source.fromFile(file.getAbsolutePath).getLines().foreach
        {line=>
          sourceFileInString+=(line.mkString+"\n")
        }
        printWriter.write(TokenizingFunctions.start(sourceFileInString));
        printWriter.write("</tokens>\n");
        printWriter.close();

    }
  }
  catch
  {
    case e:FileNotFoundException  => println("Couldn't find this directory");
    case e:IOException => println("Had an IOException trying to read that file");
    case e:IllegalArgumentException => println("invalid command in input file");

  }
  }
}