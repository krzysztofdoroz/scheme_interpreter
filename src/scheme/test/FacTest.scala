package scheme.test
 
import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class FibTest {

  @Test
  def testFac() = {
   val defineFac = "(define fac (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))"
   val callFac = "(fac 5)"		
      
    Interpreter.singleRunWithEnv(defineFac)  
      
    Assert.assertEquals(120, Interpreter.singleRunWithEnv(callFac))
  }
  
}