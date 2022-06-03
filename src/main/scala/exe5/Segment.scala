package exe5

object Segment extends Enumeration {
  type Segment=Value
  val CONSTANT,ARGUMENT,LOCAL,STATIC,THIS,THAT,POINTER,TEMP,NULL,CLASS =Value
}
