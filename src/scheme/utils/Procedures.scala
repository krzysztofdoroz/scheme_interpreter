package scheme.utils

import java.util.HashMap

object Procedures {

  def apply(op : String, args : List[Any]) = {
   (op, args) match {
      case ("+", IntConst(a)::IntConst(b)::Nil) => a + b  
      case _ => println("ERROR: no match for operation:" + op + "with args:" + args)
                 1
    }
  }
  
}