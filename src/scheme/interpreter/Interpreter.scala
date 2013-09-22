package scheme.interpreter

import scheme.parser.Parser
import scheme.utils.VarName
import scheme.utils.IntConst
import java.util.HashMap
import scheme.utils._




object Interpreter {

  def eval(input : List[Any], env : HashMap[String, Any]): Any = {
    
    input match  {
      case Define()::it => {
        val (VarName(x), exp) = (it.head, it.tail)
        env.put(x, eval(exp, env))
        println(env)
      } 
      case VarName(x)::nil => {
        println("query for:" + x)
        env.get(x)
      } 
      case IntConst(x)::nil => {
        x
      }
      case ProcName(proc)::args => {
        Procedures.apply(proc, args)
      }
    }
    
  }
  
  
  def run() = {
    
    val env = new HashMap[String, Any]
    
	  while (true) {
	    val input = readLine
	    //println(input)
	    println( eval(Parser.parse(Parser.tokenize(input)), env))
	    //println("ENV:" + env)
	  }
  }
  
  def main(args: Array[String]) {
      run
    }
  
}