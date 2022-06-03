package exe4

object ParsingFunctions {

  var listOfTokens:List[Token]=null
  var current:Token=null //variable that holds the current tokens value
  var next:Token=null //variable that holds the current tokens value
  var result:String=""
  val firstTerm:List[String]=List("<integerConstant>","<stringConstant>","<keywordConstant>","<identifier>","(","-","~")
  val firstStatements:List[String]=List("let","if","while","do","return")
  val firstOp:List[String]=List("=", "+",  "*" ,"/", "&amp;","|","&lt;" ,"&gt;","-")
  val firstClassVarDec:List[String]=List("static","field")
  val firstSubroutineDec:List[String]=List("constructor","function","method")
  val firstType:List[String]=List("int","char","boolean")



  def start(input:List[Token]):String={
    result=""
    listOfTokens=input
    current=input(0)
    next=input(1)
    class_()
    return result
  }

  /* ***helper methods*** */

  //confirms that the current token matches the expected token
  def matchTerminal(token:String):Unit={

    if(current.getContent()==token || current.getOpenLabel()==token) {
      result+=current.toString
      next_token()
    }
  }

  //updates current and next tokens from the token list
  def next_token():Unit={

    if (listOfTokens.indexOf(current)+1 != listOfTokens.length)
      current= listOfTokens(listOfTokens.indexOf(current)+1)
    if (listOfTokens.indexOf(current)+1 != listOfTokens.length)
      next= listOfTokens(listOfTokens.indexOf(current)+1)
  }

  /* ***Program Structure Parsing Functions*** */

  def class_():Unit={
    result+= "<class>\n"
    matchTerminal("class")
    className()
    matchTerminal("{")
    // classVarDec* || subroutineDec*
    while(firstClassVarDec.contains(current.getContent()))
    {
      classVarDec()
    }
    while(firstSubroutineDec.contains(current.getContent()))
    {
      subroutineDec()
    }
    matchTerminal("}")
    result+= "</class>\n"
  }
  def classVarDec():Unit={
    result+="<classVarDec>\n"
    if(current.getContent()=="static")
      matchTerminal("static")
    else if(current.getContent()=="field")
      matchTerminal("field")
    type_()
    varName()
    while(current.getContent()==",")
    {
      matchTerminal(",")
      varName()
    }
    matchTerminal(";")
    result+="</classVarDec>\n"
  }
  def type_():Unit={
    if(current.getContent()=="int")
      matchTerminal("int")
    else if(current.getContent()=="char")
      matchTerminal("char")
    else if(current.getContent()=="boolean")
      matchTerminal("boolean")
    else
      className()
  }
  def subroutineDec():Unit={
    result+="<subroutineDec>\n"
    if(current.getContent()=="constructor")
      matchTerminal("constructor")
    else if(current.getContent()=="function")
      matchTerminal("function")
    else if(current.getContent()=="method")
    {matchTerminal("method")}
    if(current.getContent()=="void")
      matchTerminal("void")
    else
      type_()
    subroutineName()
    matchTerminal("(")
    parameterList()
    matchTerminal(")")
    subroutineBody()
    result+="</subroutineDec>\n"
  }
  def parameterList():Unit={
    result+="<parameterList>\n"
    if(firstType.contains(current.getContent()) || current.getOpenLabel()=="<identifier>" )//0 or 1 time ((type varName)(','type varName)*)?
    {
      type_()
      varName()
      while (current.content == ",") //(','type varName)*
      {
        matchTerminal(",")
        type_()
        varName()
      }

    }
    result+="</parameterList>\n"
  }
  def subroutineBody():Unit={
    result+="<subroutineBody>\n"
    matchTerminal("{")
    while(current.getContent()=="var")
      varDec()
    statements()
    matchTerminal("}")
    result+="</subroutineBody>\n"
  }
  def varDec():Unit={
    result+="<varDec>\n"
    if(current.getContent()=="var")
      matchTerminal("var")
    type_()
    varName()
    while (current.content == ",") //(','type varName)*
    {
      matchTerminal(",")
      varName()
    }
    matchTerminal(";")
    result+="</varDec>\n"
  }
  def className():Unit={
    if(current.getOpenLabel()=="<identifier>") //className
      matchTerminal("<identifier>")
  }
  def subroutineName():Unit={
    if(current.getOpenLabel()=="<identifier>")
      matchTerminal("<identifier>")
  }
  def varName():Unit={
    if(current.getOpenLabel()=="<identifier>") //varName
      matchTerminal("<identifier>")
  }
  /* ***Expressions Parsing Funsvtions*** */

  def expression():Unit={ //term (op term)*
    result+="<expression>\n"
    term()
    while(firstOp.contains(current.getContent())) //(op term) => 0 or more times
    {
      op()
      term()
    }
    result+="</expression>\n"
  }

  def term():Unit={// integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall |'(' expresison  ')'| unaryOp term
    result+="<term>\n"
    if(current.getContent()=="~"||current.getContent()=="-") { //unaryOp term
      unaryOp()
      term()
    }
    else if(current.getContent()=="(")//'(' expresison  ')'
    {
      matchTerminal("(")
      expression()
      matchTerminal(")")
    }
    else if(current.getOpenLabel()=="<identifier>" & next.getContent()=="[") //varName '[' expression ']'
    {
      varName()
      matchTerminal("[")
      expression()
      matchTerminal("]")
    }
    else if((current.getOpenLabel()=="<identifier>" & next.getContent()==".")||(current.getOpenLabel()=="<identifier>" & next.getContent()=="("))//subroutineCall
      subroutinecall()
    else if(current.getOpenLabel()=="<integerConstant>")//integerConstant
      matchTerminal("<integerConstant>")
    else if (current.getOpenLabel()=="<stringConstant>")//stringConstant
      matchTerminal("<stringConstant>")
    else if(current.getOpenLabel()=="<keyword>")//keywordConstant
      matchTerminal("<keyword>")
    else //varName
      matchTerminal("<identifier>")
    result+="</term>\n"
  }

  def subroutinecall():Unit={ //subroutineName '(' epressionList ')' | (className | varName) '.' subroutineName '(' epressionList ')'
    if(next.getContent()==".")//(className | varName) '.' subroutineName '(' epressionList ')'
    {
      className()
      matchTerminal(".")
    }
    //this code fragment applies to both options
    subroutineName()
    matchTerminal("(")
    expressionList()
    matchTerminal(")")
  }

  def expressionList():Unit={// (expression (',' expression)* )?
    result+="<expressionList>\n"
    if(current.getContent()!=")") //0 or 1 times
    {
      expression()
      while(current.getContent()==",") // ( ',' expression ) * => 0 or more times
      {
        matchTerminal(",")
        expression()
      }
    }
    result+="</expressionList>\n"
  }

  def op():Unit={// '=' | '+' | '*' | '/' | '&' | '|' | '<' | '>' | '-'
    if(current.getContent()=="=")
      matchTerminal("=")
    else if(current.getContent()=="+")
      matchTerminal("+")
    else if(current.getContent()=="*")
      matchTerminal("*")
    else if(current.getContent()=="/")
      matchTerminal("/")
    else if(current.getContent()=="&amp;")
      matchTerminal("&amp;")
    else if(current.getContent()=="|")
      matchTerminal("|")
    else if(current.getContent()=="&gt;")
      matchTerminal("&gt;")
    else if(current.getContent()=="&lt;")
      matchTerminal("&lt;")
    else if(current.getContent()=="-")
      matchTerminal("-")
    else println("error in op\n")
  }
  def unaryOp():Unit={ // '~' | "'-'
    if(current.getContent()=="~")
      matchTerminal("~")
    else if(current.getContent()=="-")
      matchTerminal("-")
    else println("error in unaryOp\n")
  }

  def KeywordConstants():Unit= { // 'true' | 'false' | 'null' | 'this'
    if(current.getContent()=="true")
      matchTerminal("true")
    else if(current.getContent()=="false")
      matchTerminal("false")
    else if(current.getContent()=="null")
      matchTerminal("null")
    else if(current.getContent()=="this")
      matchTerminal("this")
    else println("error in KeywordsConstants\n")
  }

  /* ***Statements Parsing Functions*** */

  //statement*
  def statements():Unit={
    result+="<statements>\n"
    while(firstStatements.contains(current.getContent()))
      statement()
    result+="</statements>\n"
  }


  def statement():Unit={  //letStatement | ifStatement | doStatement | whileStatement| returnStatement
    if(current.content=="let")
      letStatement()
    else if(current.content=="if")
      ifStatement()
    else if(current.content=="do")
      doStatement()
    else if(current.content=="while")
      whileStatement()
    else if(current.content=="return")
      returnStatement()
    else println("error in \"statement\" \n")
  }

  def letStatement():Unit={ // 'let' varName ('[' expression ']')? '=' expression ';'
    result+= "<letStatement>\n"
    matchTerminal("let")
    varName()
    // ( '[' expression ']' )=> 0 or 1 times
    if(current.getContent()=="[")
    {
      matchTerminal("[")
      expression()
      matchTerminal("]")
    }
    matchTerminal("=")
    expression()
    matchTerminal(";")
    result+= "</letStatement>\n"
  }
  def ifStatement():Unit={ // 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
    result+="<ifStatement>\n"
    matchTerminal("if")
    matchTerminal("(")
    expression()
    matchTerminal(")")
    matchTerminal("{")
    statements()
    matchTerminal("}")
    if(current.getContent()=="else")//('else' '{' statements '}') => 0 or 1 times
    {
      matchTerminal("else")
      matchTerminal("{")
      statements()
      matchTerminal("}")
    }
    result+="</ifStatement>\n"
  }

  def whileStatement():Unit={ // 'while' '(' expression ')' '{' statements '}'
    result+="<whileStatement>\n"
    matchTerminal("while")
    matchTerminal("(")
    expression()
    matchTerminal(")")
    matchTerminal("{")
    statements()
    matchTerminal("}")
    result+="</whileStatement>\n"
  }

  def doStatement():Unit={// 'do' subroutineCall ';'
    result+="<doStatement>\n"
    matchTerminal("do")
    subroutinecall()
    matchTerminal(";")
    result+="</doStatement>\n"
  }

  def returnStatement():Unit={ // 'return' expression? ';'
    result+="<returnStatement>\n"
    matchTerminal("return")
    //epression => 0 or 1 times
    if(current.getContent()!=";")
      expression()
    matchTerminal(";")
    result+="</returnStatement>\n"
  }

}