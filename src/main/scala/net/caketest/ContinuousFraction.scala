package net.caketest

/**
 * The program is an implementation of the Exercise 1.38 at 
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html
 * 
 *  Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a
 *  memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2, 
 *  where e is the base of the natural logarithms. In this fraction, 
 *  the Ni are all 1, and the 
 *  Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... 
 *  Write a program that uses your cont-frac procedure 
 *  from exercise 1.37 to approximate e, based on Euler's expansion.
 *  
 *  
 */
object ContinuousFraction {
  
  def continuousFraction = continuousFractionIter _
  
  /**
   * Recursive version good enough for will throw StackOverflowError for large values of kTerm
   */
  def continuousFractionRecur(n: Int => Double, d: Int => Double, kTerm: Int) = {
      def cfRecur(n: Int => Double, d: Int => Double, currK: Int):Double = 
        if (currK > kTerm)
          0
        else
          n(currK)/ (d(currK) + cfRecur(n, d, currK + 1))
      
    cfRecur(n, d, 1)
  }
  
  /**
   * Iterative version which does the same work as the recursive counterpart without worrying
   * about StackOverflowError  
   */
  def continuousFractionIter(n: Int => Double, d: Int => Double, kTerm: Int) = 
    (kTerm to 1 by -1).foldLeft(0.0){
      case (accumulatedFraction, currentK) => n(currentK) / (d(currentK) + accumulatedFraction)
    }
  
  // Now that the continuous fractions are implemented, lets implement the 
  // continuous fraction to estimate the value of e
  // The Function for computing the Numerator is simple, it would be _ => 1
  // Lets look at the following for Denominator, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,...
  // Mapping the value of 1 gives us a value other than 1 at index 2, 5, 8, 11......, these indices are 
  // 1 based
  // Our formula to compute the Dr would be 
  //  d(1)    => 2 * (i + 1) / 3   if (i + 1) % 3 == 0
  //          => 1                 otherwise 
  
  val e = 2 + continuousFraction(
                  _ => 1,
                  (d) => if ((d + 1) % 3 == 0) 2 * (d + 1) / 3 else 1,
                  20                    //20 iteration should be enough to get a very good approximation
              )
  
  
  //See ContinuousFractionTest for test cases  
}