package scheme.interpreter

import scheme.parser.Parser
import scheme.utils.VarName
import scheme.utils.IntConst
import java.util.HashMap
import scheme.utils._
import scheme.utils.IntConst
import scheme.parser.SchemeParser
import scala.util.parsing.combinator._

object Interpreter {

  val env = new HashMap[String, Any]

  def applyFunction(n: VarName, params: List[Any]): Any = {

    val evaluatedParams: List[IntConst] = evaluateParams(params)
    
    println("APPLY:" + n + "," + params + ", ENV=" + env)

    if (env.containsKey(n.x)) {
      env.get(n.x) match {
        case LambdaDef(symbols, FunctionCall(n, pars)) => {

          return evalPassedParams(symbols, params, pars)
        } 
        case LambdaDef(symbols, exp) => return exp 
      }
    } else {

   
      n.x match {
        case "<" => evaluatedParams(0).x < evaluatedParams(1).x
        case "+" => evaluatedParams.foldLeft(0)((acc, x) => acc + x.x)
      }
    }
  }

  def evalPassedParams(symbols: List[VarName], values: List[Any], args: List[Any]) = {

    val symbolValueMapping = Map(symbols.map(_.x).zip(values): _*)

    val symbolsToBeEvaluated = args.map(_ match {
      case VarName(x) => symbolValueMapping.get(x) match {
        case Some(IntConst(y)) => IntConst(y)
      }
      case i => i
    })

    symbolsToBeEvaluated
  }

  def evaluateParams(params: List[Any]): List[IntConst] = {
    params.map(x => x match {
      case VarName(v) => env.get(v) match {
        case i: Int => IntConst(i)
        case _ => throw new RuntimeException("no such value!!!:" + v)
      }
      case a @ IntConst(y) => a
    })
  }

  def eval(input: Any, env: HashMap[String, Any]): Any = {

    var expr = input
    
    while (true) {
      

      expr match {

        case Define(VarName(x), expr) => {
          env.put(x, eval(expr, env))
          println("ENV:" + env)
          return
        }
        case Quote(expr) => {

          println("we have a quote")

          expr
        }

        case If(cond, then, alt) => {

          println("COND:" + cond)

          return eval(cond, env) match {
            case true => eval(then, env)
            case false => eval(alt, env)
          }

        }
        case FunctionCall(name, args) => {
         expr = applyFunction(name, args)
        }
        case VarName(x) => {
          println("getting var from env:" + x)
          return env.get(x)
        }
        case VarName(x) :: args => {
          println("calling fun from env:" + x)
          eval(List(ProcName("inc"), args), env)
        }
        case IntConst(x) => {
          return x
        }
        case ProcName(proc) :: args => {

          println("calling proc with:" + proc + "::" + args)

          if (env.get(proc) != null) {
            //	 println("no match, WTF")

            (env.get(proc), unpackList(args)) match {
              case (f: LambdaDef, List(IntConst(x))) => {
                println("applying:" + x)
                env.put(f.parameters(0).x, x)
                println("calling proc with ENV=" + env)
                /*  
              val updated : List[Any] = f.expr.flatMap(_  match {
                case v : VarName => List(IntConst(env.get(v.x).asInstanceOf[Int]))
                case d : Any => List(d) 
              })
              
              println("updated:" + updated)
              */
                //eval(updated, env)

              }
              case _ => println("no match, WTF:" + env.get(proc))
            }
          } else {
            Procedures.apply(proc, args)
          }
        }
        case lambdaDef @ LambdaDef(params, expr) => {
          return lambdaDef
        }
        case Lambda() :: it => {

          println("for fuck's sake:" + it)
          /*
        return (it.head, it.tail) match {
          case (params: List[VarName], body: List[Any]) => LambdaDef(params, unpackList(body)) 
        }
        
   */

          println("store a new proc" + env)
          //   createFunction(it)
        }
        case List(x) => {
          println("lambda:" + x)
          x match {
            case y: List[Any] => eval(y, env)
          }
        }
        case sth @ _ => {
          println("couldn't parse:" + sth)
        }

      }
    }

  }
  def unpackList(in: List[Any]): List[Any] = {
    println("unpacking list:" + in)
    in match {
      case List(x) => {
        x match {
          case y: List[Any] => y
        }
      }
    }
  }

  def createFunction(input: List[Any]): Function[_ <: Any, _ <: Any] = {
    println("creating function from:" + input)

    val ProcName(op) :: VarName(n) :: IntConst(v) :: Nil = unpackList(input)

    op match {
      case "+" => {
        new Function1[Int, Int] {
          def apply(x: Int): Int = {

            println("applying function for:" + op + "," + List(IntConst(x), IntConst(v)))

            Procedures.apply(op, List(IntConst(x), IntConst(v)))
          }
        }
      }
    }
  }

  def singleRunWithEnv(input: String): Any = {
    // eval(Parser.parse(Parser.tokenize(input)), env)
    eval(SchemeParser.parse2(input), env)

  }

  def run() = {

    val env = new HashMap[String, Any]

    while (true) {
      val input = readLine
      println(eval(Parser.parse(Parser.tokenize(input)), env))
      println("NEW PARSER:")
      println(eval(SchemeParser.parse2(input), env))

      //println("ENV:" + env)
    }
  }

  def main(args: Array[String]) {
    run
  }

}