package exe1
//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782
import scala.util.control._
object function extends App{
  var str="";
  var counter=0;
  def add(): String=
  {
    str="@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M+D\nM=D\n@SP\nM=M+1\n";
    return str;
  }
  def sub():String=
  {
    str="@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\nM=D\n@SP\nM=M+1\n";
    return str;
  }
  def neg(): String=
  {
    str="@SP\nAM=M-1\nD=-M\nM=D\n@SP\nM=M+1\n";
    return str;
  }
  def eqA():String=
  {
    str=s"@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\n@IF_TRUE$counter\nD;JEQ\n@SP\nA=M\nM=0\n@END$counter\n0;JMP\n(IF_TRUE$counter)\n@SP\nA=M\nM=-1\n(END$counter)\n@SP\nM=M+1\n";
    counter+=1;
    return str;
  }
  def gt(): String= //גדול מ...
  {
    str=s"@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\n@IF_TRUE$counter\nD;JGT\n@SP\nA=M\nM=0\n@END$counter\n0;JMP\n(IF_TRUE$counter)\n@SP\nA=M\nM=-1\n(END$counter)\n@SP\nM=M+1\n";
    counter+=1;
    return str;
  }
  def lt(): String= //קטן מ...
  {
    str=s"@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nD=M-D\n@IF_TRUE$counter\nD;JLT\n@SP\nA=M\nM=0\n@END$counter\n0;JMP\n(IF_TRUE$counter)\n@SP\nA=M\nM=-1\n(END$counter)\n@SP\nM=M+1\n";
    counter+=1;
    return str;
  }
  def and(): String=
  {
    str=s"@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=D&M\n@SP\nM=M+1\n";
    return str;
  }
  def or(): String=
  {
    str=s"@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=D|M\n@SP\nM=M+1\n";
    return str;
  }
  def not(): String=
  {
    str=s"@SP\nAM=M-1\nD=!M\nM=D\n@SP\nM=M+1\n";
    return str;
  }
  def push(typArg:String,arg:Int,fileName:String): String =
  {
    typArg match {
      case "constant" => { //הכנסת קבוע למחסנית
        str=s"@$arg\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "local" => { //הולכים ל LCL + ארגומנט שולפים משם את הערך ומציבים בראש המחסנית
        str=s"@$arg\nD=A\n@LCL\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "argument" => { //הולכים ל ARG + ארגומנט שולפים משם את הערך ומציבים בראש המחסנית
        str=s"@$arg\nD=A\n@ARG\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "that" => {  //הולכים ל THAT + ארגומנט שולפים משם את הערך ומציבים בראש המחסנית
        str=s"@$arg\nD=A\n@THAT\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "this" => { //הולכים ל THIS + ארגומנט שולפים משם את הערך ומציבים בראש המחסנית
        str=s"@$arg\nD=A\n@THIS\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "static" => {
        str=s"@$fileName.$arg\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "temp" => { //temp+5 temp=5,12 arg=0,7
        str=s"@$arg\n";
        for(i<-1 to 5)
        {
          str+="A=A+1\n";
        }
        str+=s"D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
      case "pointer" => {
        if(arg==0) //THIS
          str=s"@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        if(arg==1) //THAT
          str=s"@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n";
        return str;
      };
    }
  }
  def pop(typArg:String,arg:Int,fileName:String): String=
  {
    typArg match {
      case "local" => {
        str=s"@SP\nA=M–1\nD=M\n@LCL\nA=M\n";
        for(i<-1 to arg)
        {
          str+="A=A+1\n";
        }
        str+=s"M=D\n@SP\nM=M-1\n";
        return str;
      };
      case "argument" => {
        str=s"@SP\nA=M–1\nD=M\n@ARG\nA=M\n";
        for(i<-1 to arg)
        {
          str+="A=A+1\n";
        }
        str+=s"M=D\n@SP\nM=M-1\n";
        return str;
      };
      case "that" => {
        str=s"@SP\nA=M–1\nD=M\n@THAT\nA=M\n";
        for(i<-1 to arg)
        {
          str+="A=A+1\n";
        }
        str+=s"M=D\n@SP\nM=M-1\n";
        return str;
      };
      case "this" => {
        str=s"@SP\nA=M–1\nD=M\n@THIS\nA=M\n";
        for(i<-1 to arg)
        {
          str+="A=A+1\n";
        }
        str+=s"M=D\n@SP\nM=M-1\n";
        return str;
      };
      case "static" => {
        str=s"@SP\nA=M-1\nD=M\n@$fileName.$arg\nM=D\n@SP\nM=M-1\n";
        return str;
      };
      case "temp" => {
        str=s"@SP\nA=M-1\nD=M\n@$arg\n";
        for(i<-1 to 5)
        {
          str+="A=A+1\n";
        }
        str+=s"M=D\n@SP\nM=M-1\n";
        return str;
      };
      case "pointer" => {
        if(arg==0)
          str=s"@SP\nA=M-1\nD=M\n@THIS\nM=D\n@SP\nM=M-1\n";
        if(arg==1)
          str=s"@SP\nA=M-1\nD=M\n@THAT\nM=D\n@SP\nM=M-1\n";
        return str;
      };
    }
  }
  def label(fileName:String,programName:String): String=
  {
      str= s"($fileName.$programName)\n";
      return str;
  }
  def goto(fileName:String,programName:String): String=
  {
      str= s"@$fileName.$programName\n0;JMP\n";
       return str;
  }
  def if_goto(fileName:String,programName:String): String=
  {
      str= s"@SP\nM=M-1\nA=M\nD=M\n@$fileName.$programName\nD;JNE\n";
      return str;
  }
  def call(functionName:String,numArgs:String): String=
  {
    val newARG=numArgs.toInt-5
    str= s"@$functionName.ReturnAddress\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    //str+= push("constant","LCL","")//push LCL
    str+=s"@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    //str+=push("constant","ARG","")//push ARG
    str+=s"@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    //str+= push("constant","THIS","")// push THIS
    str+=s"@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    //str+=push("constant","THAT","")// push THAT
    str+=s"@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    str+=s"@SP\nD=M\n@$newARG\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n@$functionName\n0;JMP\n($functionName.ReturnAddress)\n"
    return str
  }
  def function(functionName:String,numArgs:String): String=
  {
    str=s"($functionName)\n@$numArgs\nD=A\n@$functionName.End\nD;JEQ\n($functionName.Loop)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@$functionName.Loop\nD=D-1;JNE\n($functionName.End)\n"
    return str
  }
  def return_(): String=
  {
    str=s"@LCL\nD=M\n@5\nA=D-A\nD=M\n@13\nM=D\n@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nM=D\n@ARG\nD=M\n@SP\nM=D+1\n"
    str+=s"@LCL\nM=M-1\nA=M\nD=M\n@THAT\nM=D\n@LCL\nM=M-1\nA=M\nD=M\n@THIS\nM=D\n@LCL\nM=M-1\nA=M\nD=M\n@ARG\n"
    str+=s"M=D\n@LCL\nM=M-1\nA=M\nD=M\n@LCL\nM=D\n@13\nA=M\n0;JMP\n"
    return str
  }
}
