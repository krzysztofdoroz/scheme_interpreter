package scheme.utils

sealed abstract class Atom;
case class Define() extends Atom;
case class IntConst(x : Int) extends Atom;
case class VarName(x: String) extends Atom;
case class ProcName(x: String) extends Atom;
case class Lambda() extends Atom;
case class LambdaDef(parameters: List[VarName], expr: List[Any]) extends Atom;
case class Quote() extends Atom;