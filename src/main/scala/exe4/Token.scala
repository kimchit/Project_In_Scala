package exe4

class Token(var typeOpenLabel: String,var content:String, var typeEndLabel: String)
{
  def getOpenLabel():String={
    return typeOpenLabel
  }
  def getContent():String={
    return content
  }
  override def toString() : String = {
    return typeOpenLabel + content + typeEndLabel;
  }
}

