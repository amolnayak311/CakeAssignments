package net.caketest

import org.scalatest._
import net.caketest.ContinuousFraction._

class ContinuousFractionTest extends FlatSpec with Matchers {
  
    "Both Recursive and Iterative Versions" should " Yield same values for same input provided" in {
      assert(continuousFractionIter(_ => 1, _ => 1, 1) == continuousFraction(_ => 1, _ => 1, 1))
      assert(continuousFractionIter(_ => 1, _ => 1, 5) == continuousFraction(_ => 1, _ => 1, 5))
    }
    
    "Providing _ => 1 for both Numerator and Denominator function " should " yield inverse of golden ratio " in {
        val inverseGoldenRatio = 2 / (1 + math.sqrt(5))
        // 40 is good enough to get the derived value equal to inverse of golden ratio
        assert(inverseGoldenRatio == continuousFraction(_ => 1, _ => 1, 40))
    }
    
    "Value apprimimated by our e " should " be a very good approximation to the real value" in {
      assert(scala.math.E == e)
    }
}