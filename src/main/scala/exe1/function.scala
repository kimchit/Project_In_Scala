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
}
