package scheme.test

import org.junit.Test
import scheme.interpreter.Interpreter
import junit.framework.Assert

class BuiltInFunctionsTest {

  @Test
  def testPlusFunc() = {
    val input = "(+ 1 2)"

    Assert.assertEquals(3, Interpreter.singleRunWithEnv(input))
  }
  
}