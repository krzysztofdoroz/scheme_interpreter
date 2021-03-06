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

  var env = new HashMap[String, Any]

  def applyFunction(n: VarName, params: List[Any], context : HashMap[String, Any]): Any = {

      println("APPLY:" + n + "," + params + ", ENV=" + env)

       context.putAll(env)
       val newContext = new HashMap[String, Any]
      newContext.putAll(env)
  
    val evaluatedParams: List[IntConst] = evaluateParams(params, context)

  //  var context = new HashMap[String, Any]()
   
    if (env.containsKey(n.x)) {
      env.get(n.x) match {
        case LambdaDef(symbols, FunctionCall(n, pars, con)) => {

          return FunctionCall(n,evalPassedParams(symbols, params, pars),con)
        } 
        case LambdaDef(symbols, exp) => {

          val symbolToValMap : List[(VarName,Any)] = symbols.zip(params)
         symbolToValMap.foreach(a => a match {
           case (VarName(n), IntConst(x)) => newContext.put(n ,  x)  
          case (VarName(n), fc @ FunctionCall(f,args,con)) => newContext.put(n, eval(fc, context)) 
       //   case _ => 
         } )
          
         println("EXP:" + exp)
         
     //    return exp
        return eval(exp,newContext) 
     
        } 
      }
    } else {

      println("built in functions:" + n.x)
   
     return n.x match {
        case "<" => evaluatedParams(0).x < evaluatedParams(1).x
        case "+" => evaluatedParams.foldLeft(0)((acc, x) => acc + x.x)
        case "-" => evaluatedParams.foldRight(0)((x, acc) => x.x - acc)
        case "*" => evaluatedParams.foldLeft(1)((acc, x) => acc * x.x)
        case "=" => evaluatedParams(0).x == evaluatedParams(1).x
        
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

  def evaluateParams(params: List[Any], context : HashMap[String, Any]): List[IntConst] = {
    params.map(x => x match {
      case VarName(v) => context.get(v) match {
        case i: Int => IntConst(i)
        case _ => throw new RuntimeException("no such value!!!:" + v)
      }
      case a @ IntConst(y) => a
      case fc @ FunctionCall(n,args,con) => eval(fc, context) match {
        case i : Int => IntConst(i)
      }
      case a @ _ => throw new RuntimeException("can't eval parameters:" + a)
    })
  }

  def eval(input: Any, context: HashMap[String, Any]): Any = {

    var expr = input
    var env = context
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
        case FunctionCall(name, args, context) => {
          
         expr = applyFunction(name, args, env)
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
        case lambdaDef @ LambdaDef(params, expr) => {
          return lambdaDef
        }
        case sth @ _ => {
          return sth
        }

      }
    }

  }

  def singleRunWithEnv(input: String): Any = {
    eval(SchemeParser.parse2(input), env)
  }

  def run() = {

    val env = new HashMap[String, Any]

    while (true) {
      val input = readLine
      println(eval(Parser.parse(Parser.tokenize(input)), env))
      println(eval(SchemeParser.parse2(input), env))

    }
  }

  def main(args: Array[String]) {
    run
  }

}