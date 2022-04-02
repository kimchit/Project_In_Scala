package exe1
//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782
import function._
import java.io._
import java.nio.file.{Files, Paths}
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.control._

object exe1 extends App{

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  // val directory;
  println("please enter your directory")
  val pathName= scala.io.StdIn.readLine(); //get path name from the user C:\Users\user\Desktop\nand2tetris\nand2tetris\projects\07\MemoryAccess\BasicTest
  var directory:File=null
  var printWriter:PrintWriter=null
  var fileObject:File=null
  val bool= Files.exists(Paths.get(pathName))

  try
  {
    if(!bool)
      throw new FileNotFoundException ()
    directory = new File(pathName)
    //loop over files, choose whichever end with vm
    recursiveListFiles(directory).filter(_.getName.endsWith(".vm")).foreach// loops over files in "files"
    {
      file=>
        val name= (file.getName.split("\\\\").last).split(".vm").last; //split name of directory and take the last name of package
        //create outPut file and save in fileObject
        //fileObject= new File( name+".asm") //new file to the output
        fileObject=new File(directory,name+".asm")
        printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
        Source.fromFile(file.getAbsolutePath).getLines().foreach
        { line=>
          val words= line.split(" ")
          words(0) match {
            case "add" =>
              str = add()
              printWriter.write(str)
              Breaks
            case "sub" =>
              str = sub()
              printWriter.write(str)
              Breaks
            case "neg" =>
              str = neg()
              printWriter.write(str)
              Breaks
            case "eq" =>
              str = eqA()
              printWriter.write(str)
              Breaks
            case "gt" =>
              str = gt()
              printWriter.write(str)
              Breaks
            case "lt" =>
              str = lt()
              printWriter.write(str)
              Breaks
            case "and" =>
              str = and()
              printWriter.write(str)
              Breaks

            case "or" =>
              str = or()
              printWriter.write(str)
              Breaks

            case "not" =>
              str = not()
              printWriter.write(str)
              Breaks

            case "push" =>
              str=push(words(1),words(2).toInt,file.getName)
              printWriter.write(str)
              Breaks

            case "pop" =>

              str=pop(words(1),words(2).toInt,file.getName)
              printWriter.write(str)
              Breaks

            case _ =>Breaks;//println("invalid command in input file") ///default
          }
        }
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



