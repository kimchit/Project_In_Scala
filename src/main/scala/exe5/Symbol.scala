package exe5

import exe5.Command.Value

object Symbol extends Enumeration {
  type Symbol = Value
  val STATIC,FIELD,VAR,ARG,NONE = Value

}
