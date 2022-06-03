package exe5

import scala.util.matching.Regex

object TokenizingFunctions extends App{
  var ListOfTokens:String=""
  lazy val LetterPattern:Regex = raw"[a-z]|[A-Z]|'_'".r
  lazy val NumberPattern:Regex= "[0-9]".r
  lazy val SymbolPattern:Regex= "[-{}().,;+*&|=<>~]".r// without the []
  lazy val KeyWordsPattern= List("class","constructor","function","method","file","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return","field")

  def start(file:String):String=
  {  ListOfTokens=""//reinitialize each time apon entry
    Q0(file)
    ListOfTokens
  }

  def  Q0(line:String):Unit={
    if(line.isEmpty()) //if reached eof
      return
    val cur_char=line(0)
    var tokenString=""
    if(cur_char==' '|cur_char=='\n'|cur_char=='\t')//white space
    {
      Q0(line.substring( 1))
    }
    else if(cur_char=='/')
    {
      var char=cur_char
      var temp=line.substring(1)// remove the /
      if(temp(0)=='/')//if the next char is also a /, the line is commented out
      {
        temp=temp.substring(1)
        char=temp(0)
        while(char!='\n' ) //while we didn't reach the end of the line
        {
          temp=temp.substring(1) //decrease the line by 1
          char=temp(0)
        }
        Q0(temp.substring(1)) //go to Q0 without the comment
      }
      else if(temp(0)=='*')//if the next char is a *, the lines are commented out
      {
        temp=temp.substring(1)
        char=temp(0)
        while(char!='*' || temp(1)!='/') //while we didn't reach the end of the comment
        {
          temp=temp.substring(1) //decrease the line by 1
          char=temp(0)
        }
        Q0(temp.substring(2)) //go to Q0 without the comment
      }
      else {
        GenTokenSymbol(char.toString()) //generate a / token
        Q0(temp.substring(1))}
    }
    else if (LetterPattern.findAllMatchIn(cur_char.toString).nonEmpty) //letter or underscore
    {
      Q1(line.substring(1),tokenString.concat(cur_char.toString))
    }
    else if(NumberPattern.findAllMatchIn(cur_char.toString).nonEmpty) //"number"
    {
      Q2(line.substring(1),tokenString.concat(cur_char.toString))
    }
    else if(SymbolPattern.findAllMatchIn(cur_char.toString).nonEmpty||cur_char==']'||cur_char=='[') //symbol
    {
      Q3(line.substring(1),tokenString.concat(cur_char.toString))
    }
    else if (cur_char=='"')//quotes
    {
      Q4(line.substring(1),tokenString)
    }
    else
      println("error in tokenizing file")
  }


  def  Q1(line:String,tokenString:String):Unit={ //Keyword or id
    var cur_char=line(0)
    var temp=line
    var str=tokenString
    while(cur_char=='_'||NumberPattern.matches(cur_char.toString())||LetterPattern.matches(cur_char.toString()))
    {
      str+=cur_char.toString//add the char to the tokenString
      temp=temp.substring(1) //decrease the line by 1
      cur_char=temp(0)
    }
    if(KeyWordsPattern.contains(str))//key word
      GenTokenKeyword(str)
    else
      GenTokenIdentifier(str)
    Q0(temp)
  }

  def  Q2(line:String,tokenString:String):Unit={ //number

    if(line.isEmpty()){ //if reached end of line, generate token
      GenTokenInteger(tokenString)
      Q0(line)
    }
    var cur_char=line(0)
    var temp=line
    var str=tokenString
    while(NumberPattern.findAllMatchIn(cur_char.toString).nonEmpty)
    {
      str+=cur_char.toString //add the char to the tokenString
      temp=temp.substring(1) //decrease the line by 1
      cur_char=temp(0)
    }
    GenTokenInteger(str)
    Q0(temp) //remove the quotes at the end of the string


  }
  def  Q3(line:String,tokenString:String):Unit={ //symbol
    GenTokenSymbol(tokenString)
    Q0(line)
  }
  def  Q4(line:String,tokenString:String):Unit={//string

    var cur_char=line(0)
    var temp=line
    var str=tokenString
    while(cur_char!='"')
    {
      str+=cur_char.toString //add the char to the tokenString
      temp=temp.substring(1) //decrease the line by 1
      cur_char=temp(0)
    }
    GenTokenString(str)
    Q0(temp.substring(1)) //remove the quotes at the end of the string

  }
  def GenTokenKeyword(Token:String)={
    if(ListOfTokens!=null)
      ListOfTokens+=s"<keyword> $Token </keyword>\n"
    else
      ListOfTokens=s"<keyword> $Token </keyword>\n"
  }
  def GenTokenInteger(Token:String)={
    if(ListOfTokens!=null)
      ListOfTokens+=s"<integerConstant> $Token </integerConstant>\n"
    else
      ListOfTokens=s"<integerConstant> $Token </integerConstant>\n"
  }
  def GenTokenString(Token:String)={
    if(ListOfTokens!=null)
      ListOfTokens+=s"<stringConstant> $Token </stringConstant>\n"
    else
      ListOfTokens=s"<stringConstant> $Token </stringConstant>\n"
  }
  def GenTokenSymbol(Token:String)={
    var symbol=Token
    if(Token=="<")
      symbol="&lt;"
    else if(Token==">")
      symbol="&gt;"
    else if(Token=="\"")
      symbol="&quot;"
    else if(Token=="&")
      symbol="&amp;"
    if(ListOfTokens.nonEmpty)
      ListOfTokens+=s"<symbol> $symbol </symbol>\n"
    else
      ListOfTokens=s"<symbol> $symbol </symbol>\n"
  }
  def GenTokenIdentifier(Token:String)=
  {
    if(ListOfTokens!=null)
      ListOfTokens+=s"<identifier> $Token </identifier>\n"
    else
      ListOfTokens=s"<identifier> $Token </identifier>\n"
  }

}

