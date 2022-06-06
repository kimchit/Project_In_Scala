package exe5

import exe5.Segment.Value

object Kind extends Enumeration {
  type Kind=Value
  val STATIC,FIELD,CONSTANT,ARG,VAR,NONE =Value

}
