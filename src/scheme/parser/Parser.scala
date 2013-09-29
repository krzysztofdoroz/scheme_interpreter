package scheme.parser

import scheme.utils.Atom
import scheme.utils.IntConst
import scheme.utils._

object Parser {

  def parse(input: List[String]): List[Any] = {
    parse(input, List(), List(), List())
  }
  
  def parse(input: List[String], currentList : List[Any], stack : List[List[Any]], result: List[Any]): List[Any] = {

    (input, currentList, stack, result) match {
      case (Nil, ch :: ct, _, _) => result ++ currentList
      case (Nil, _, _, _) => result
      case ("" :: it, _, _, _) => parse(it, currentList, stack, result)
      case ("(" :: it, _, _, _) => parse(it, List(), if (currentList.isEmpty) { stack } else { stack :+ currentList }, result)
      case (")" :: it, _, Nil, _) => currentList
      case (")" :: it, _, _, _) => parse(input.tail, stack.last :+ currentList, stack.take(stack.size - 1), result)
      case _ => parse(input.tail, currentList :+ createAtom(input.head), stack, result)
    }
  }
  
  def tokenize(s : String) = {
    s.replace("(", " ( ").replace(")", " ) ").split("\\s+").toList
  }

  def createAtom(x: Any): Atom =

    x match {
      case "define" => new Define()
      case "lambda" => new Lambda()
      case x : String => {
        try {
          new IntConst(x.toInt)
        } catch {
          case _ => varOrProc(x)
		}
      } 
    }
  
  def varOrProc(name : String) = {
    val registeredProcs = Set("+")
    
    if (registeredProcs.contains(name)){
      new ProcName(name)
    } else {
      new VarName(name)
    }
    
  }
  
  
}