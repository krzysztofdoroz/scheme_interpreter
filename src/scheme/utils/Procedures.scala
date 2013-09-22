package scheme.utils

import java.util.HashMap

object Procedures {

  def apply(op : String, args : List[Any]) = {
   (op, args) match {
      case ("+", IntConst(a)::IntConst(b)::Nil) => a + b  
    }
  }
  
}