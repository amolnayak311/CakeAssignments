package net.caketest

/**
 * The Solution provided is the Scala implementation of the Problem 1.6 given in 
 * 
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html 
 * 
 * The Code Given in the problem statement is in Scheme Language
 * 
 * 
 * Exercise 1.6. Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
 *   
 *   (define (new-if predicate then-clause else-clause)
 *     (cond (predicate then-clause)
 *           (else else-clause)))
 *   
 *   Eva demonstrates the program for Alyssa:
 *   
 *   (new-if (= 2 3) 0 5)
 *   5
 *   
 *   (new-if (= 1 1) 0 5)
 *   0
 *   
 *   Delighted, Alyssa uses new-if to rewrite the square-root program:
 *   
 *   (define (sqrt-iter guess x)
 *     (new-if (good-enough? guess x)
 *             guess
 *             (sqrt-iter (improve guess x)
 *                        x)))
 *   
 *   What happens when Alyssa attempts to use this to compute square roots? Explain.
 *	
 * 
 *  
 */
object NewtonMethodSqrt {
  
  
  
  /**
   * Following is our own implementation of the if else conditions as we dont want to 
   * use the standard if else construct provided by the language Scala.
   * our implementation of if is called fi and else is called esle 
   */
  
  class IfElseByName[A](condition: => Boolean, ifTrue: => A) {
    def esle(ifFalse: => A)  = condition match {
      case true => ifTrue
      case false => ifFalse
    }
  }
  
  class IfElseByValue[A](condition: Boolean, ifTrue: A) {
    def esle(ifFalse: A)  = condition match {
      case true => ifTrue
      case false => ifFalse
    }
  }
  
  
  // Following are two functions that implement our user defined if else in a call by name
  // (normal-order evaluation) and call by value (applicative-order evaluation) respectively 
  
  /**
   * An Implementation of If Else condition that would take the if and else values by name
   */
  def fi[A](condition: => Boolean)(ifTrue: => A) = new IfElseByName(condition, ifTrue)
  
  
  
  //Comment out the above implementation of the fi function and uncomment the below
  //function to run the version where parameters are called by values.
  
  
  /**
   * An Implementation of If Else condition that would take the if and else blocks by value
   */
  //def fi[A](condition: Boolean)(ifTrue: A) = new IfElseByValue(condition, ifTrue)
  
  
  // We first implement all the functions helper functions similar to those defined in the 
  // Scheme code given in the  text
  // Not checking for invalid parameters like, say negative numbers
  
  def abs(a: Double) = if(a < 0) -a else a
  
  def sqrt(x:Double): Double = {     
    def square(s:Double) = s * s
    def goodEnough(guess:Double) = abs(square(guess) - x) < 0.001
    def average(f:Double, s:Double) = (f + s) / 2
    def improve(guess: Double) = average(guess, x / guess)
    def sqrtIter(guess: Double):Double = 
      fi(goodEnough(guess)) {
        guess 
      } esle {
        sqrtIter(improve(guess))
      }
    sqrtIter(1)
  }
  
 //See NewtonMethodSqrtTest For some test cases testing the correctness of the implementation
  
  
  //Conclusion: As we can see, the original program in Scheme failed with the new 
  //implementation of if-else as the value for the if and else condition are called by value 
  //(applicative-order evaluation). Since the else part is a recursive call, it runs
  //infinitely in Scheme (throws StackOverflowError in Scala and terminates). 
  //The crux of the custom if-else implementation is to have lazy evaluation of if else, based on the 
  //condition that gets evaluated, lazy evaluation can happen only when we have call-by-name, 
  //or normal-order evaluation
}