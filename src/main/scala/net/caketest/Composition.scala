package net.caketest

/**
 * Following program is for the problem Exercise 1.41, Exercise 1.42 and Exercise 1.43
 * 
 * These problems are closely related to function composition 
 * thus we have them implemented in same Scala class
 * 
 * Exercise 1.41.  Define a procedure double that takes a procedure 
 * of one argument as argument and returns a procedure that applies the original procedure twice.
 * For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a
 * procedure that adds 2. What value is returned by
 * (((double (double double)) inc) 5)
 * 
 * Exercise 1.42.  Let f and g be two one-argument functions. The composition f after g is defined to be 
 * the function x  f(g(x)). Define a procedure compose that implements composition. For example, 
 * if inc is a procedure that adds 1 to its argument,
 * ((compose square inc) 6)
 * 49
 * 
 * Exercise 1.43.  If f is a numerical function and n is a positive integer, 
 * then we can form the nth repeated application of f, which is defined to be the function whose 
 * value at x is f(f(...(f(x))...)). For example, if f is the function x   x + 1, 
 * then the nth repeated application of f is the function x   x + n. If f is the operation 
 * of squaring a number, then the nth repeated application of f is the function that raises its
 * argument to the 2nth power. Write a procedure that takes as inputs a procedure that computes f and
 * a positive integer n and returns the procedure that computes the nth repeated application of f. 
 * Your procedure should be able to be used as follows:
 * 
 * ((repeated square 2) 5)
 * 625
 * 
 */
object Composition {
  
  
  def square(x: Double) = x * x
  
  def inc(x: Double) = x + 1
  
  /**
   * 
   */
  def double(f: Double => Double)(value:Double) = compose(f, f)(value)
  
  /**
   * 
   */
  def compose(f: Double => Double, g: Double => Double)(x: Double) = f(g(x))
  
  /**
   * 
   */
  def repeated(f: Double => Double, n: Int)(value: Double) =
        (1 to n).foldLeft(value) { case (accumulatedValue, _) => f(accumulatedValue) }
  
  
  // See CompositionTest for test cases
}