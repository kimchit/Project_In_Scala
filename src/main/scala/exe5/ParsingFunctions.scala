package exe5

import exe5.Token

import java.io.File
import java.util
import javax.xml.transform.Result
import scala.collection.immutable.Range.Int

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
  var symbolTable: SymbolTable = null;
  var class_name: String = null
  var function_name:String=null
  var label_num_if:Int=0
  var label_num_while:Int=0
  var num_param_list: Int = 0;
  var sub_routine_source:String=null;

  def start(input:List[Token]):String={
    symbolTable = new SymbolTable();
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
    next_token()
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
    matchTerminal("class")
    class_name=current.getContent()
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
  }
  def classVarDec():Unit={
    var kind=""
    var _type=""
    var name=""
    if(current.getContent()=="static") {
      matchTerminal("static")
      symbolTable.define(next.getContent(), current.getContent(), "STATIC") //name,type,kind="Static"
    kind="STATIC"
    } else if(current.getContent()=="field") {
      matchTerminal("field")
      symbolTable.define(next.getContent(), current.getContent(), "FIELD") //name,type,kind="field"
      kind="FIELD"
    }
    _type=current.getContent()
    type_()
    name=current.getContent()
    varName()
    symbolTable.define(name,_type,kind)
    while(current.getContent()==",")
    {
      matchTerminal(",")
      name=current.getContent()
      varName()
      symbolTable.define(name,_type,kind)
    }
    matchTerminal(";")
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
    label_num_if=0
    label_num_while=0
    num_param_list=0;
    sub_routine_source=""
    symbolTable.startSubRoutine();

    if(current.getContent()=="constructor") {
      matchTerminal("constructor")
      sub_routine_source="constructor"
    } else if(current.getContent()=="function") {
      matchTerminal("function")
     function_name=(s"$class_name.${next.getContent()}")
      sub_routine_source="function"
    }
    else if(current.getContent()=="method") {
      matchTerminal("method")
      symbolTable.defineMethod("this", class_name, Kind.ARG.toString);
      sub_routine_source="method"
    }
    if(current.getContent()=="void")
      matchTerminal("void")
    else
      type_()
    subroutineName()
    matchTerminal("(")
    parameterList()
    matchTerminal(")")
    subroutineBody()
  }
  def parameterList():Unit={

    if(firstType.contains(current.getContent()) || current.getOpenLabel()=="<identifier>" )//0 or 1 time ((type varName)(','type varName)*)?
    {
      symbolTable.define(next.getContent(), current.getContent(), Kind.ARG.toString)
      type_()
      varName()
      while (current.content == ",") //(','type varName)*
      {
        matchTerminal(",")
        symbolTable.define(next.getContent(), current.getContent(), Kind.ARG.toString)
        type_()
        varName()
      }

    }
  }
  def subroutineBody():Unit={
    matchTerminal("{")
    while(current.getContent()=="var") {
           varDec()
    }
    VM_Writer.writeFunction(function_name,num_param_list)
    if (sub_routine_source=="constructor") {
      VM_Writer.writePush(Segment.CONSTANT, symbolTable.varCount(Kind.FIELD.toString))
      VM_Writer.writeCall("Memory.alloc", 1)
     VM_Writer.writePop(Segment.POINTER, 0)
    }
    else if (sub_routine_source=="method") {
      VM_Writer.writePush(Segment.ARGUMENT, 0)
      VM_Writer.writePop(Segment.POINTER, 0)
    }
    statements()
    matchTerminal("}")
  }
  def varDec():Unit={
    if(current.getContent()=="var")
      matchTerminal("var")
    var typ=current.getContent();
    type_()
    var var_name=current.getContent()
    varName()
    symbolTable.define(var_name,typ,"var")
    num_param_list+=1
    while (current.content == ",") //(','type varName)*
    {
      matchTerminal(",")
      var var_name=current.getContent()
      varName()
      symbolTable.define(var_name,typ,"var")
      num_param_list+=1
    }
    matchTerminal(";")
  }
  def className():Unit={
    if(current.getOpenLabel()=="<identifier>") { //className
      matchTerminal("<identifier>")

    }
  }
  def subroutineName():Unit={
    if(current.getOpenLabel()=="<identifier>")
      matchTerminal("<identifier>")
  }
  def varName():Unit={
    if(current.getOpenLabel()=="<identifier>") //varName
         matchTerminal("<identifier>")
  }
  /* ***Expressions Parsing Functions*** */

  def expression():Unit={ //term (op term)*

    term()
    num_param_list=0
    num_param_list+=1
    while(firstOp.contains(current.getContent())) //(op term) => 0 or more times
    {
      val operator=current.getContent()
      next_token()
      term()
      result+=VM_Writer.writeArithmetic(Command.withName(operator))
      num_param_list+=1
    }

  }

  def term():Unit={// integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall |'(' expresison  ')'| unaryOp term

    if(current.getContent()=="~"||current.getContent()=="-") { //unaryOp term
      val operator=current.getContent()
      next_token()
      term()
      if(operator=="~")
        result+=VM_Writer.writeArithmetic(Command.NOT)
      else //operator= "-"
      result+=VM_Writer.writeArithmetic(Command.NEG)

    }
    else if(current.getContent()=="(")//'(' expresison  ')'
    {
      matchTerminal("(")
      expression()
      matchTerminal(")")
    }
    else if(current.getOpenLabel()=="<identifier>" & next.getContent()=="[") //varName '[' expression ']'
    { var var_name=current.getContent()
      varName()
      matchTerminal("[")
      expression()
      VM_Writer.writePush(Segment.withName(symbolTable.kindOf(var_name)),symbolTable.indexOf(var_name))
      VM_Writer.writeArithmetic(Command.ADD)
      VM_Writer.writePop(Segment.POINTER,1)
      VM_Writer.writePush(Segment.THAT,0)
      matchTerminal("]")
    }
    else if((current.getOpenLabel()=="<identifier>" & next.getContent()==".")||(current.getOpenLabel()=="<identifier>" & next.getContent()=="("))//subroutineCall
       subroutinecall()
    else if(current.getOpenLabel()=="<integerConstant>")//integerConstant
      {
        VM_Writer.writePush(Segment.CONSTANT, current.getContent().toInt)
        matchTerminal("<integerConstant>")
      }
    else if (current.getOpenLabel()=="<stringConstant>") {//stringConstant
      val value=current.getContent()
      result+=VM_Writer.writePush(Segment.CONSTANT,value.length)
      matchTerminal("<stringConstant>")
     result+=VM_Writer.writeCall("String.new",1)
      for( i <- 1 until value.length)  {
         var temp = value(i).toInt;
        result+=s"push constant $temp"
        result+=VM_Writer.writeCall("String.appendChar",2)
        }

    } else if(current.getOpenLabel()=="<keyword>")//keywordConstant
      {
        if(current.getContent()=="this") {
          result+=VM_Writer.writePush(Segment.POINTER,0)
        } else if(current.getContent()=="true"){
         result+=VM_Writer.writePush(Segment.CONSTANT,0)
         result+=VM_Writer.writeArithmetic(Command.NOT)
        }
        else if (current.getContent()=="false") {
            result+=VM_Writer.writePush(Segment.CONSTANT,0)
          }
        matchTerminal("<keyword>")
      }
    else //varName
      { var var_name=current.getContent()
        VM_Writer.writePush(Segment.withName(symbolTable.kindOf(var_name)),symbolTable.indexOf(var_name))
        matchTerminal("<identifier>")
      }
  }

  def subroutinecall():Unit={ //subroutineName '(' epressionList ')' | (className | varName) '.' subroutineName '(' epressionList ')'
    var sub_name_1=current.getContent();
    var sub_name_2=""
    var sub_name_3=""
    var counter = 1
    if(next.getContent()==".")
    {
      className()
      matchTerminal(".")
      sub_name_3=sub_name_1+"."
      sub_name_2=current.getContent()
      sub_name_3+=current.getContent()
      if(symbolTable.typeOf(sub_name_1)=="")
      {
        result+=VM_Writer.writePush(Segment.withName(symbolTable.kindOf(sub_name_1)), symbolTable.indexOf(sub_name_1))
        sub_name_3 = symbolTable.typeOf(sub_name_1) + "." +sub_name_2

      }
      else
      {
        counter=0
      }
      matchTerminal("(")
      parameterList()
      counter+=num_param_list
      matchTerminal(")")
      result+=VM_Writer.writeCall(sub_name_3,counter)
    }
    else {
      result+=VM_Writer.writePush(Segment.POINTER,0)
      sub_name_3=s"$class_name.$sub_name_1"
      parameterList()
      counter+=num_param_list
      matchTerminal(")")
      VM_Writer.writeCall(sub_name_3,counter)
    }
  }

  def expressionList():Unit={// (expression (',' expression)* )?
    if(current.getContent()!=")") //0 or 1 times
    {
      expression()
      num_param_list+=1
      while(current.getContent()==",") // ( ',' expression ) * => 0 or more times
      {
        matchTerminal(",")
        expression()
        num_param_list+=1
      }
    }
  }

  def op():Unit={// '=' | '+' | '*' | '/' | '&' | '|' | '<' | '>' | '-'
    if(current.getContent()=="=") {
      matchTerminal("=")
      result+=VM_Writer.term_to_vm_ops(Command.EQ.toString)
    } else if(current.getContent()=="+") {
      matchTerminal("+")
      result+=VM_Writer.term_to_vm_ops(Command.ADD.toString)
    } else if(current.getContent()=="*") {
      matchTerminal("*")
      result+=VM_Writer.expression_to_vm_ops("*")
    } else if(current.getContent()=="/") {
      matchTerminal("/")
      result+=VM_Writer.term_to_vm_ops("/")
    } else if(current.getContent()=="&amp;") {
      matchTerminal("&amp;")
      result+=VM_Writer.term_to_vm_ops(Command.AND.toString)
    } else if(current.getContent()=="|") {
      matchTerminal("|")
      result+=VM_Writer.term_to_vm_ops(Command.OR.toString)
    } else if(current.getContent()=="&gt;") {
      matchTerminal("&gt;")
      result+=VM_Writer.term_to_vm_ops(Command.GT.toString)
    } else if(current.getContent()=="&lt;") {
      matchTerminal("&lt;")
      result+=VM_Writer.term_to_vm_ops(Command.LT.toString)
    } else if(current.getContent()=="-") {
      matchTerminal("-")
      result+=VM_Writer.term_to_vm_ops(Command.NEG.toString)
    } else println("error in op\n")
  }
  def unaryOp():Unit={ // '~' | "'-'
    if(current.getContent()=="~") {
      matchTerminal("~")
     result+= VM_Writer.term_to_vm_ops(Command.NOT.toString)
    }
    else if(current.getContent()=="-") {
      matchTerminal("-")
      result+=VM_Writer.term_to_vm_ops(Command.NEG.toString)
    } else println("error in unaryOp\n")
  }

  def KeywordConstants():Unit= { // 'true' | 'false' | 'null' | 'this'
    if(current.getContent()=="true") {
      matchTerminal("true")
      result+=VM_Writer.term_to_vm_ops("TRUE")
    } else if(current.getContent()=="false") {
      matchTerminal("false")
      result+=VM_Writer.term_to_vm_ops("FALSE")
    } else if(current.getContent()=="null") {
      matchTerminal("null")
      result+=VM_Writer.term_to_vm_ops("null")
    } else if(current.getContent()=="this") {
      matchTerminal("this")
      result+=VM_Writer.writePush(Segment.POINTER, 0)
    } else println("error in KeywordsConstants\n")
  }

  /* ***Statements Parsing Functions*** */

  //statement*
  def statements():Unit={

    while(firstStatements.contains(current.getContent()))
      statement()

  }


  def statement():Unit={  //letStatement | ifStatement | doStatement | whileStatement| returnStatement
    if(current.content=="let")
      letStatement()
    else if(current.content=="if")
      ifStatement()
    else if(current.content=="do") {
      doStatement()
      VM_Writer.writePop(Segment.TEMP,0)
    } else if(current.content=="while")
      whileStatement()
    else if(current.content=="return")
      returnStatement()
    else println("error in \"statement\" \n")
  }

  def letStatement():Unit={ // 'let' varName ('[' expression ']')? '=' expression ';'
    matchTerminal("let")
    var name=current.getContent()
    varName()
    var one_time=false
    // ( '[' expression ']' )=> 0 or 1 times
    if(current.getContent()=="[")
    {
      matchTerminal("[")
      expression()
      VM_Writer.writePush(Segment.withName(symbolTable.kindOf(name)),symbolTable.indexOf(name))
      VM_Writer.writeArithmetic(Command.ADD)
      matchTerminal("]")
      one_time=true
    }
    matchTerminal("=")
    expression()
    if (!one_time) {
      VM_Writer.writePop(Segment.withName(symbolTable.kindOf(name)), symbolTable.indexOf(name))

    }
    else {
      VM_Writer.writePop(Segment.TEMP, 0)
      VM_Writer.writePop(Segment.POINTER, 1)
      VM_Writer.writePush(Segment.TEMP, 0)
      VM_Writer.writePop(Segment.THAT, 0)
      }
    matchTerminal(";")
  }
  def ifStatement():Unit={ // 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
    matchTerminal("if")
    matchTerminal("(")
    expression()
    result+=VM_Writer.writeIf(s"IF_TRUE$label_num_if")
    result+=VM_Writer.writeGoto(s"IF_FALSE$label_num_if")
    result+=VM_Writer.writeLabel(s"IF_TRUE$label_num_if")
    matchTerminal(")")
    matchTerminal("{")
    statements()
    matchTerminal("}")
    if(current.getContent()=="else")//('else' '{' statements '}') => 0 or 1 times
    {
      result+=VM_Writer.writeGoto(s"IF_END$label_num_if")
      result+=VM_Writer.writeLabel(s"IF_FALSE$label_num_if")
      matchTerminal("else")
      matchTerminal("{")
      statements()
      matchTerminal("}")
      result+=VM_Writer.writeLabel(s"IF_END$label_num_if")
    }
    else
      result+=VM_Writer.writeLabel(s"IF_END$label_num_if")
    label_num_if+=1//increase the lable counter by 1
  }

  def whileStatement():Unit={ // 'while' '(' expression ')' '{' statements '}'
    matchTerminal("while")
    matchTerminal("(")
    result+=VM_Writer.writeLabel(s"WHILE_EXP$label_num_while")
    expression()
    result+=VM_Writer.writeArithmetic(Command.NOT)
    result+=VM_Writer.writeIf(s"WHILE_END$label_num_while")
    matchTerminal(")")
    matchTerminal("{")
    statements()
   result+= VM_Writer.writeGoto(s"WHILE_EXP$label_num_while")
    result+=VM_Writer.writeLabel(s"WHILE_END$label_num_while")
    matchTerminal("}")
    label_num_while+=1
  }

  def doStatement():Unit={// 'do' subroutineCall ';'
    matchTerminal("do")
    subroutinecall()
    matchTerminal(";")

  }

  def returnStatement():Unit={ // 'return' expression? ';'
    matchTerminal("return")

    //expression => 0 or 1 times
    if(current.getContent()!=";") {
       expression()
      matchTerminal(";")
      result+=VM_Writer.writeReturn()
    }
    else //0 expressions
      {
        matchTerminal(";")
        result+=VM_Writer.writePush(Segment.CONSTANT,0)
        result+=VM_Writer.writeReturn()
      }
  }

}


