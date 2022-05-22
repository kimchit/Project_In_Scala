package exe2
//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782
import _root_.exe1._
import java.io._

import java.nio.file.{Files, Paths}
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.control._

object exe2 extends App{

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
  var str: String=null;

  try
  {
    if(!bool)
      throw new FileNotFoundException ()
    directory = new File(pathName)
    //loop over files, choose whichever end with vm
//    Initialization. This asm code should appear as the first lines of each asm file.

    var Array = recursiveListFiles(directory)
    Array.filter(_.getName.endsWith(".vm"))
    val name= (pathName.split("\\\\").last).split(".vm").last; //split name of directory and take the last name of package
    //create outPut file and save in fileObject
    //fileObject= new File( name+".asm") //new file to the output
    fileObject=new File(directory,name+".asm")
    printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
    if(Array.filter(_.getName.endsWith(".vm")).length > 1)
    {
      str=s"@256\nD=A\n@SP\nM=D\n@Sys.init.RETURN0\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nD=M\n@0\nD=D-A\n@5\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n@Sys.init\n0;JMP\n(Sys.init.RETURN0)\n";
      printWriter.write(str)
    }
    Array.filter(_.getName.endsWith(".vm")).foreach// loops over files in "files"
    {
      file=>
//        val name= (file.getName.split("\\\\").last).split(".vm").last; //split name of directory and take the last name of package
//        //create outPut file and save in fileObject
//        //fileObject= new File( name+".asm") //new file to the output
//        fileObject=new File(directory,name+".asm")
//        printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter
        Source.fromFile(file.getAbsolutePath).getLines().foreach
        { line=>
          val words= line.split(" ")
          words(0) match {
            case "add" =>
              str = function.add()
              printWriter.write(str)
              Breaks
            case "sub" =>
              str = function.sub()
              printWriter.write(str)
              Breaks
            case "neg" =>
              str = function.neg()
              printWriter.write(str)
              Breaks
            case "eq" =>
              str = function.eqA()
              printWriter.write(str)
              Breaks
            case "gt" =>
              str = function.gt()
              printWriter.write(str)
              Breaks
            case "lt" =>
              str = function.lt()
              printWriter.write(str)
              Breaks
            case "and" =>
              str = function.and()
              printWriter.write(str)
              Breaks

            case "or" =>
              str = function.or()
              printWriter.write(str)
              Breaks

            case "not" =>
              str = function.not()
              printWriter.write(str)
              Breaks

            case "push" =>
              str=function.push(words(1),words(2).toInt,file.getName.split(".vm").last)
              printWriter.write(str)
              Breaks

            case "pop" =>

              str=function.pop(words(1),words(2).toInt,file.getName.split(".vm").last)
              printWriter.write(str)
              Breaks

            case "label" =>

              str= function.label(file.getName.split(".vm").last, words(1))
              printWriter.write(str)
              Breaks

            case "goto" =>

              str=function.goto(file.getName.split(".vm").last, words(1));
              printWriter.write(str)
              Breaks

            case "if-goto" =>

              str=function.if_goto(file.getName.split(".vm").last, words(1));
              printWriter.write(str)
              Breaks

            case "function" =>

              str=function.function(words(1),words(2))
              printWriter.write(str)
              Breaks

            case "return" =>

              str=function.return_();
              printWriter.write(str)
              Breaks

            case "call" =>

              str=function.call(words(1),words(2))
              printWriter.write(str)
              Breaks

            case _ =>Breaks;//println("invalid command in input file") ///default
          }
        }
       //printWriter.close();
    }
    printWriter.close();
  }
  catch
  {
    case e:FileNotFoundException  => println("Couldn't find this directory");
    case e:IOException => println("Had an IOException trying to read that file");
    case e:IllegalArgumentException => println("invalid command in input file");

  }
}





