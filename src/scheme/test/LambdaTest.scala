package scheme.test
 
import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class LambdaTest {

  @Test
  def testLambda() = {
    val defineInc = "(define inc (lambda (x) (+ x 1)))"
    val callInc = "(inc 7)"		
      
    Interpreter.singleRunWithEnv(defineInc)  
      
    Assert.assertEquals(8, Interpreter.singleRunWithEnv(callInc))
  }
  
}