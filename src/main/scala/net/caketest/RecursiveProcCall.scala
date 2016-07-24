package net.caketest


/**
 * 
 * Program to solve the Exercise 1.34 given at 
 * 
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html
 * 
 * The problem statement is 
 * 
 * Exercise 1.34.  Suppose we define the procedure
 *
 *   (define (f g)
 *     (g 2))
 *   
 *   Then we have
 *   
 *   (f square)
 *   4
 *   
 *   (f (lambda (z) (* z (+ z 1))))
 *   6
 *   
 *   What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
 *
 *
 * 
 */
object RecursiveProcCall {
  
  /**
   * 
   */
  def f(g : Int => Int) = g(2)
  
  def square(x: Int) = x * x
  
  def main(args: Array[String]): Unit = {
    println("Applying Square function to f gives us " + f(square))
    println("Applying lambda function z * (z + 1) gives us " + f(z => z * (z + 1)))
    
    //Applying function f(f) doesnt work in Scala as it is a strongly typed language and 
    //gives us the following error 
    //type mismatch; found : (Int ⇒ Int) ⇒ Int required: Int ⇒ Int
    
    //Uncomment below line to see the error
    //println("Applying f(f) gives " + f(f))
    
    
    
    //Conclusion, We get compile time error in Scala as it is strongly typed language.
    //The book sample is in Scheme and we get an error at runtime. 
    //Lets explain this error we get in Scheme
    //
    //We will be evaluating the following Scheme code on https://repl.it/languages/scheme 
    //
    //  >> (define (f g)
    //    (g 2))
    //  
    //  >> (f f)
    //  Error: 2 is not a function [f, (anon), (anon)]
    //
    // Explanation for the error is
    //
    // Lets expand (f f)
    //  (f f) -> (f 2) -> (2 2)
    //
    // Which explains the error, 2 is not a function 
  }
}