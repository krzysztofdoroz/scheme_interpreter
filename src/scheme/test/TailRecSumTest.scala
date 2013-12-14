package scheme.test
 
import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class TailRecSumTest {

  @Test
  def testTailRecSum() = {
   val defineSum = "(define sum (lambda (n acc) (if (= n 0) acc (sum (- n 1) (+ acc n) ))))"
   val callSum = "(sum 5 0)"		
      
    Interpreter.singleRunWithEnv(defineSum)  
      
    Assert.assertEquals(15, Interpreter.singleRunWithEnv(callSum))
  }
 
  
  
}