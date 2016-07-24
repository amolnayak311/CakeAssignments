package net.caketest

import org.scalatest._
import net.caketest.Composition._

class CompositionTest extends FlatSpec with Matchers {
  
  "double(inc)(_) " should " increment the value by 2 " in {
    double(inc)(1) should be (3)
    double(inc)(-2) should be (0)
  }
  
  "double(square)(_)" should " raise the provided value by 4" in {
    double(square)(2) should be (16)
    double(square)(-3) should be (81)
    double(double(square)_)(5) should be (math.pow(5, 16))
  }
  
  "compose(square, inc)(_)" should " increment the number and then square it " in {
    compose(square,inc)(6) should be (49)
    compose(inc,square)(6) should be (37)
  }
  
  "repeated(f, n)(v)" should "call f(...f(v)...) n times" in {
    repeated(square, 2)(5) should be (625)
    repeated(square, 2)(2) should be (16)
    repeated(_ + 1, 10)(1) should be (11)
  }
  
  "repeated(f, 0)(v)" should " not invoke the function and return the input value v as is " in {
    repeated(square, 0)(10) should be (10)
  }
}