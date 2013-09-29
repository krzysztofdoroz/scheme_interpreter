package scheme.interpreter

import scheme.parser.Parser
import scheme.utils.VarName
import scheme.utils.IntConst
import java.util.HashMap
import scheme.utils._

object Interpreter {

  val env = new HashMap[String, Any]
  
  def eval(input : List[Any], env : HashMap[String, Any]): Any = {
    
    input match {
      case Define()::it => {
        val (VarName(x), exp) = (it.head, it.tail)
        
        env.put(x, eval(exp, env))
        println("ENV:" + env)
      } 
      case VarName(x)::Nil => {
        println("getting var from env:" + x)
        env.get(x)
      } 
      case VarName(x)::args => {
        println("calling fun from env:" + x)
        eval(List(ProcName("inc"),args), env)
      }
      case IntConst(x)::nil => {
        x
      }
      case ProcName(proc)::args => {
        
        println("calling proc with:" + proc + "::" + args)
          
        if (env.get(proc) != null){
        //	 println("no match, WTF")
          
          (env.get(proc), unpackList(args)) match {
            case (f : Function1[Any ,Any], List(IntConst(x))) => {
              println("applying:" + x)  
              f.apply(x)
            	
            }
            case _ => println("no match, WTF:" + env.get(proc))
          }
        } else {
        	Procedures.apply(proc, args)
        }
      }
      case Lambda()::List(VarName(x))::it => {
        
        println("for fuck's sake:" + it)
       // val unpackedList = unpackList(it)
       // val (VarName(x)) = (unpackList(unpackedList.head))
      //   val (VarName(x), definition) = (head, unpackedList.tail)
        
         
         println("store a new proc" + env)
         createFunction(it)
      }
      case List(x) => {
        println("lambda:" + x)
        x match {
          case y : List[Any] => eval(y,env)
        }
      }
      case sth @ _ => {
        println ("couldn't parse:" + sth)
      }
        
    }
    
  }
  
  def unpackList(in : List[Any]) : List[Any] = {
    println("unpacking list:" + in)
    in match {
      case List(x) => {
        x match  {
          case y: List[Any] => y
        }
      }
    }
  }
  
  def createFunction(input : List[Any]) : Function[_ <: Any, _ <: Any] = {
    println("creating function from:" + input)
    
    val ProcName(op)::VarName(n)::IntConst(v)::Nil = unpackList(input)
    
    new Function1[Int, Int] {
      def apply(x : Int): Int = {
        
        println("applying function for:" + op + "," + List(IntConst(x),IntConst(v)))
        
        Procedures.apply(op, List(IntConst(x),IntConst(v)))
      }
    }
  }
  
  def singleRunWithEnv(input : String): Any = {
    eval(Parser.parse(Parser.tokenize(input)), env)
  }
  
  
  def run() = {
    
    val env = new HashMap[String, Any]
    
	  while (true) {
	    val input = readLine
	    println(eval(Parser.parse(Parser.tokenize(input)), env))
	    //println("ENV:" + env)
	  }
  }
  
  def main(args: Array[String]) {
      run
    }
  
}