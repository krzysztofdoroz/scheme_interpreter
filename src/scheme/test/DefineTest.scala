package scheme.test

import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class DefineTest {
  
  @Test
  def testDefine() = {
    val input = "(define x 2)"
    val secondLine = "x"		
 
    Interpreter.singleRunWithEnv(input)  
      
    Assert.assertEquals(2, Interpreter.singleRunWithEnv(secondLine))
  }

}