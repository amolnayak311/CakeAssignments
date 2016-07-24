package net.caketest
/**
 * Solution Given is for the  problem 1.5 in 
 * 
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html
 * 
 * The Solution and an explanation is given by demonstrating the concepts using Scala Language
 * 
 * Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
 *
 *   (define (p) (p))
 *   
 *   (define (test x y)
 *     (if (= x 0)
 *         0
 *         y))
 *   
 *   Then he evaluates the expression
 *   
 *   (test 0 (p))
 *   
 *   What behavior will Ben observe with an interpreter that uses applicative-order evaluation? 
 *   What behavior will he observe with an interpreter that uses normal-order evaluation?
 *    Explain your answer. (Assume that the evaluation rule for the special form if is the same whether 
 *    the interpreter is using normal or applicative order: The predicate expression is evaluated first, 
 *    and the result determines whether to evaluate the consequent or the alternative expression.)
 * 	
 * 	On calling (test 0 (p)), the behavior of the program for applicative order and 
 * 	normal order interpreter is different in this case. The Goal of this program is to 
 *  
 *  In Scala we have call by name (normal order evaluation) and 
 *  call by value (applicative order evaluation)
 * 
 * 
 */
object EvalOrder {
  
  def p:Int = p
  
  /*
   * Following function when invoked, the values of the parameter are evaluated before the
   * function is applied on those operands
   */
  def testByValue(first: Int, second: Int):Int = if(first == 0) 0 else second
  
  /*
   * Following function when invoked, the values of the parameter are evaluated lazily
   * function is applied on those operands and they are evaluated only if needed
   */
  def testByName(first: Int, second: => Int): Int = if(first == 0) 0 else second
  
  def main(args: Array[String]): Unit = {
    
      //Let us now see the behavior of the above two functions when the second parameter, 
      //is a function which recursively calls itself without yielding any value
    
      //Case 1: Call by name, this ensures the second parameter is evaluated lazily when it needs to
      //In this case since the first parameter is 0 and the if condition in the body of the
      //test function satisfies, the value 0 is returned without evaluating the else block
      //The second parameter in this case is a function which calls itself recursively 
      println(testByName(0, p))
      
      //Case 2: Call by value, this evaluates both the params before invoking the test and thus 
      //never terminates, even though for this very scenario the value of the second parameter was
      //not needed, the second parameter will be evaluated before applying the function to 
      //the two operands passed to this function 
      //This program never terminates    
      println(testByValue(0, p))
  }
  
  //Conclusion
  //For the Question 1.5, the answer would be as follows
  
  //Depending on the Evaluation order of the interpreter, whether, applicative-order evaluation
  //or Normal-order evaluation, The program will behave differently for this scenario
  //In case of Applicative-Order evaluation, the program will never terminate as both the 
  //parameters will be evaluated before the function is applied. The second parameter is a recursive
  //call and thus never evaluates to a Int value causing the program to loop for ever
  
  //In case of Normal Order function evaluation the operands are lazily evaluated
  //Since the second parameter is needed only when the if condition fails, we dont need that value
  //in this scanario and the value of the first operand is returned. Had the value of the first operand 
  //be anything else apart from 0, the call by name varient of the function to would run infinitely
}