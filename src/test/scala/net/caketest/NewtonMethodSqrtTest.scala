package net.caketest

import org.scalatest._
import net.caketest.NewtonMethodSqrt._

class NewtonMethodSqrtTest extends FlatSpec with Matchers {
    
    "Square roots " should " not be off from the real ones by more than 0.001" in {
      assert(abs(3 - sqrt(9)) <= 0.001)
      assert(abs(5 - sqrt(25)) <= 0.001)
      assert(abs(4 - sqrt(16)) <= 0.001)
      assert(abs(11 - sqrt(121)) <= 0.001)
      assert(abs(math.sqrt(76826) - sqrt(76826)) <= 0.001)
    }
  
}