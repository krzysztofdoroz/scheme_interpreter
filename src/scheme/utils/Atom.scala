package scheme.utils

import java.util.HashMap

sealed abstract class Atom;
case class Define(v: VarName, expr : Any) extends Atom;
case class IntConst(x : Int) extends Atom;
case class VarName(x: String) extends Atom;
case class ProcName(x: String) extends Atom;
case class Lambda() extends Atom;
case class LambdaDef(parameters: List[VarName], expr: Any) extends Atom;
case class Quote(expr : StringConst) extends Atom;
case class If(cond : Any, then : Any, alt : Any) extends Atom;
case class StringConst(x : String) extends Atom;
case class FunctionCall(n : VarName, params: List[Any], context : HashMap[String, Any])
