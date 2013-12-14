package scheme.utils

import java.util.HashMap

sealed abstract class Token;
case class DEFINE(v: VarName, expr : Any) extends Token;
case class IntConst(x : Int) extends Token;
case class VarName(x: String) extends Token;
case class ProcName(x: String) extends Token;
case class Lambda() extends Token;
case class LambdaDef(parameters: List[VarName], expr: Any) extends Token;
case class QUOTE(expr : StringConst) extends Token;
case class IF(cond : Any, then : Any, alt : Any) extends Token;
case class StringConst(x : String) extends Token;
case class FunctionCall(n : VarName, params: List[Any], context : HashMap[String, Any])
