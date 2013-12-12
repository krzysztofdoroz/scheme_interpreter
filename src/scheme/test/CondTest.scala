package scheme.test

import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class CondTest {

  @Test
  def testCond() = {
    val defineCond = "(if (< 1 2) 1 2)"		
    
    Assert.assertEquals(1, Interpreter.singleRunWithEnv(defineCond))
  }
  
}