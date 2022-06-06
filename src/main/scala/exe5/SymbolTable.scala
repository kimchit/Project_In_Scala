package exe5

import scala.collection.mutable._

class SymbolTable (){
  //constructor begins

  var class_symbolTable=new HashMap[String,(String,String,Int)]; //(name) => (type, kind, number)
  var subroutine_symbolTable=new HashMap[String,(String,String,Int)]; //(nam
  var counter=new HashMap[Symbol.Value,Int]
  //class scope
  private var staticCounter=0
  private var fieldCounter=0

  //method scope
  private var argCounter=0
  private var varCounter=0

  //starts a subroutine ie, resets the Symbol table
  def startSubRoutine():Unit={
    counter.update(Symbol.VAR,0)
    counter.update(Symbol.ARG,0)
    subroutine_symbolTable.clear()
  }

   //defines a new identifier for a given name, type and kind and assign it a running index
    def define(name:String,_type:String,kind:String):Unit={
      val i = counter(Symbol.withName(kind.toUpperCase()))
      if (kind=="arg" || kind=="var")
      subroutine_symbolTable.addOne(name,(_type,kind,i))
      else
        class_symbolTable.addOne(name,(_type,kind,i))
    }
  def defineMethod(name:String,_type:String,kind:String):Unit={
    subroutine_symbolTable.addOne(name,(_type,kind,0))

  }

  //returns the number of variables of the given kind already defined in the current scope
  def varCount(kind:String): Int =
    {  return counter(Symbol.withName(kind))
    }

  //returns the kind of  the named identifier
  def kindOf(name:String): String =
  {
    if(class_symbolTable.contains(name)==true)
      return class_symbolTable(name)._2
    else if(subroutine_symbolTable.contains(name)==true)
      return subroutine_symbolTable(name)._2
    return Symbol.NONE.toString
  }

  //returns the type of  the named identifier
  def typeOf(name:String): String =
  {
    if(class_symbolTable.contains(name)==true)
      return class_symbolTable(name)._2
    else if(subroutine_symbolTable.contains(name)==true)
      return subroutine_symbolTable(name)._2
    println("index out of range")
    return ""
  }

  //returns the index assigned to the named identifier
  def indexOf(name:String): Int =
  {
    if(class_symbolTable.contains(name)==true)
      return class_symbolTable(name)._3
    else if(subroutine_symbolTable.contains(name)==true)
      return subroutine_symbolTable(name)._3
    println("index out of range")
    return -1
  }
}
