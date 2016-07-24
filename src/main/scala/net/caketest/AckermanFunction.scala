package net.caketest

/**
 * Program is an implementation of Exercise 1.10 given at  
 * 
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html
 * 
 * Exercise 1.10. The following procedure computes a mathematical function called Ackermann's function.
 * 
 *
 *   (define (A x y)
 *     (cond ((= y 0) 0)
 *           ((= x 0) (* 2 y))
 *           ((= y 1) 2)
 *           (else (A (- x 1)
 *                    (A x (- y 1))))))
 *   
 *   What are the values of the following expressions?
 *   
 *   (A 1 10)
 *   
 *   (A 2 4)
 *   
 *   (A 3 3)
 *   
 *   Consider the following procedures, where A is the procedure defined above:
 *   
 *   (define (f n) (A 0 n))
 *   
 *   (define (g n) (A 1 n))
 *   
 *   (define (h n) (A 2 n))
 *   
 *   (define (k n) (* 5 n n))
 *   
 *   Give concise mathematical definitions for the functions computed by the procedures 
 *   f, g, and h for positive integer values of n. For example, (k n) computes 5n2
 * 
 */
object AckermanFunction {
  
    def ackerman(x: BigInt, y: BigInt) : BigInt =
      if(y == 0) 0
      else if (x == 0) 2 * y
      else if (y == 1) 2
      else ackerman(x - 1, ackerman(x, y - 1))
    
    //By Definition of the problem we have following functions
    
    //Step wise expansion of this function yields the following, we will use A for ackerman function
    //A(0, n) matches the second if condition, this this will be same as 2n    
    def f(n:BigInt) = ackerman(0, n)    // == 2n 
    
    //Step wise expansion of this function gives the following
    //  A(1, n) =>  
    //  A(0, A(1, n - 1)) => 
    //  2 * A(1, n - 1) => 
    //  2 * 2 * A(0, A(1, n - 2)) =>
    //  2 * 2 * 2 ..... 2 * A(1, 0) =>
    //  2 * 2 * 2 ... 2 (n times) =>
    //  2 ^ n
    def g(n:BigInt) = ackerman(1, n)
    
    //Step wise expansion gives us the following
    //A(2, n) =>
    //A(1, A(2, n - 1)) => 
    //A(0, A(1, A(2, n - 1) - 1)) =>
    //2 * A(1, A(2, n - 1) - 1) =>
    //Substituting with y =  A(2, n - 1)
    // A(1, y) gets reduced to 2 * A(1, y - 1) and the recursion will keep going on 
    //This isn't very helpful to come up with a clear formula, lets try plugging in some number 
    //and come up with some explanation
    
    def h(n: BigInt) = ackerman(2, n)
    
    
    def main(args: Array[String]): Unit = {
        
      //Validate the correctness of our derivation
      println("f(2) should be 4 and is " + f(2))
      println("f(3) should be 6 and is " + f(3))
      
      println("g(2) should be 4 and is " + g(2))
      println("g(3) should be 8 and is " + g(3))
      println("g(4) should be 16 and is " + g(4))
      println("g(10) should be 1024 and is " + g(10))
      
      //Lets look at h(n)
      println("h(1) is " + h(1))  //2
      println("h(2) is " + h(2))  //4
      println("h(3) is " + h(3))  //16
      println("h(4) is " + h(4))  //65536
      
      //Based on these 4 values we can conclude the following formula 
      //h(n) = 2^ h(n - 1)      
      
      //Stack overflow for this number, must be pretty huge, by formula above its 2 ^ 65536
      //see http://www.mindspring.com/~jimvb/2aa5.htm for actual val;ue
      //println("h(5) is " + h(5))      
    }
  
}