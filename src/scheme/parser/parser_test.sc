package scheme.parser

object parser_test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val s_1 = "(define x 2)"                        //> s_1  : String = (define x 2)
  val s_2 = "(define inc (lambda (x) (+ x 1)))"   //> s_2  : String = (define inc (lambda (x) (+ x 1)))
  val s_3 = "x"                                   //> s_3  : String = x
  val s_4 = "(+ 1 2)"                             //> s_4  : String = (+ 1 2)
  
  val in = Parser.tokenize(s_1)                   //> in  : List[String] = List("", (, define, x, 2, ))
  
  val in_2 =Parser.tokenize(s_2 )                 //> in_2  : List[String] = List("", (, define, inc, (, lambda, (, x, ), (, +, x,
                                                  //|  1, ), ), ))
  val in_3 = Parser.tokenize(s_3)                 //> in_3  : List[String] = List(x)
  
  val in_4 = Parser.tokenize(s_4)                 //> in_4  : List[String] = List("", (, +, 1, 2, ))
  
  
  Parser.parse(in)                                //> res0: List[Any] = List(Define(), VarName(x), IntConst(2))
  
  Parser.parse(in_2)                              //> res1: List[Any] = List(Define(), VarName(inc), List(VarName(lambda), List(Va
                                                  //| rName(x)), List(ProcName(+), VarName(x), IntConst(1))))
                   
  Parser.parse(in_3)                              //> res2: List[Any] = List(VarName(x))
  
  Parser.parse(in_4 )                             //> res3: List[Any] = List(ProcName(+), IntConst(1), IntConst(2))
  
}