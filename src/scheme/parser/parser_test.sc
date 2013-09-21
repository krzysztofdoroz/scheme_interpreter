package scheme.parser

object parser_test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val s_1 = "(define x 2)"                        //> s_1  : String = (define x 2)
  val s_2 = "(define inc (lambda (x) (+ x 1)))"   //> s_2  : String = (define inc (lambda (x) (+ x 1)))
  val s_3 = "x"                                   //> s_3  : String = x
  
  
  val in = Parser.tokenize(s_1)                   //> in  : List[String] = List("", (, define, x, 2, ))
  
  val in_2 =Parser.tokenize(s_2 )                 //> in_2  : List[String] = List("", (, define, inc, (, lambda, (, x, ), (, +, x,
                                                  //|  1, ), ), ))
  val in_3 = Parser.tokenize(s_3)                 //> in_3  : List[String] = List(x)
  
  Parser.parse(in)                                //> res0: List[Any] = List(Define(), VarName(x), IntConst(2))
  
  Parser.parse(in_2)                              //> res1: List[Any] = List(Define(), VarName(inc), List(VarName(lambda), List(Va
                                                  //| rName(x)), List(VarName(+), VarName(x), IntConst(1))))
                   
  Parser.parse(in_3)                              //> res2: List[Any] = List(VarName(x))
  
  
}