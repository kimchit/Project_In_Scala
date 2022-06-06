package exe5
import Command._
import Segment._
import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import java.io.File
object VM_Writer {

  var outPutFile: File = null
  var printWriter: PrintWriter = null

  //open the output file for writing
  def Contsructor(_outPutFile:File):Unit={
    outPutFile=_outPutFile;
    printWriter=new PrintWriter(outPutFile)
  }

  def writePush(segment:Segment,index:Int):String={
    /*if(segment.equals(Segment.LOCAL))
      printWriter.write(s"push LCL $index\n")
    else if(segment.equals(Segment.ARG))
      printWriter.write(s"push ARG $index\n")
    else if(segment.equals(Segment.THIS))
      printWriter.write(s"push THIS $index\n")
    else if(segment.equals(Segment.THAT))
      printWriter.write(s"push THAT $index\n")
    else if(segment.equals(Segment.CONST))
      printWriter.write(s"push CONST $index\n")
    else if(segment.equals(Segment.STATIC))
      printWriter.write(s"push static $index\n")
    else if(segment.equals(Segment.TEMP))
      printWriter.write(s"push temp $index\n")
    else if(segment.equals(Segment.POINTER))
      printWriter.write(s"push pointer $index\n")*/
    if(Segment.values.contains(segment))
      (s"push ${segment.toString.toLowerCase} $index")
    else
      ("Error in vm_writer push")
  }

  def writePop(segment:Segment,index:Int):String={
    /*if(segment.equals(Segment.LCL))
      printWriter.write(s"pop LCL $index\n")
    else if(segment.equals(Segment.ARG))
      printWriter.write(s"pop ARG $index\n")
    else if(segment.equals(Segment.THIS))
      printWriter.write(s"pop THIS $index\n")
    else if(segment.equals(Segment.THAT))
      printWriter.write(s"pop THAT $index\n")
    else if(segment.equals(Segment.CONST))
      printWriter.write(s"pop CONST $index\n")
    else if(segment.equals(Segment.STATIC))
      printWriter.write(s"pop static $index\n")
    else if(segment.equals(Segment.TEMP))
      printWriter.write(s"pop temp $index\n")*/
    if(Segment.values.contains(segment))
     (s"pop ${segment.toString.toLowerCase} $index\n")
    else
     ("Error in vm_writer pop")
  }
  def writeArithmetic(command:Command):String={
   (s"${command.toString.toLowerCase}\n")
  }
  def writeLabel(label:String):String={
    (s"label $label\n")
  }
  def writeGoto(label:String):String={
    (s"goto $label\n")
  }
  def writeIf(label:String):String={
    (s"if-goto $label\n")
  }
  def writeCall(name:String,nArgs:Int):String={
    (s"call $name $nArgs\n")
  }
  def writeFunction(name:String,nLocals:Int):String={
     s"function $name $nLocals\n"
  }
  def writeReturn():String={
    "push constant 0\n"+ "return\n"
  }

  //close the output file for writing
  def close():File={
  printWriter.close()
    return outPutFile
  }
  def expression_to_vm_ops(basic_op:String):Unit= {

    if (basic_op == "+")
      writeArithmetic(Command.ADD)
    else if (basic_op == "-")
      writeArithmetic(Command.NEG)
    else if (basic_op == "*")
      writeCall("Math.multiply", 2)
    else if (basic_op == "/")
      writeCall("Math.divide", 2)
    else if (basic_op == "&")
      writeArithmetic(Command.ADD)
    else if (basic_op == "|")
      writeArithmetic(Command.OR)
    else if (basic_op == ">")
      writeArithmetic(Command.GT)
    else if (basic_op == "<")
      writeArithmetic(Command.LT)
    else if (basic_op == "=")
      writeArithmetic(Command.EQ)
    else println("error in expression_to_vm_ops")
  }
  def term_to_vm_ops(basic_op:String):String= {
    if (basic_op == "-")
      writeArithmetic(Command.NEG)
    else if (basic_op == "~")
      writeArithmetic(Command.NOT)
  //  else if (basic_op.contains("integer ".+()))
    //  writePush(Segment.CONSTANT,basic_op.)//result = "push constant "+basic_op.split(' ').last
    else if (basic_op == "True")
      writePush(Segment.CONSTANT,0)
    else if (basic_op == "False"||basic_op=="null")
      writePush(Segment.CONSTANT,1)
    /*  else if (basic_op == "string")
        writePush(Segment.CONST,basic_op.length)
          writeCall()"call String.new\n" +
        "for(i=0 ; i<"+basic_op.length+" ; ++i){\n" +
        "\ttemp = cast(int)str[i];\n" +
        writePush(Segment.CONST,temp)
        writeCall("String.appendChar", 2) */
    else  ("error in term_to_vm_ops")
  }

}
