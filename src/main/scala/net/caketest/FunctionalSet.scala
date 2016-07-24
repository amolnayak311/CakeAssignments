package net.caketest

trait GenericFunctionalSet {
  
  type Set[T] = T => Boolean
  
  /**
   * Used to define a new set of 
   */
  final def newSet[T](elem:T) = (e:T) => e == elem
  
  /**
   * Merge Two sets together into one Set
   */
  final def merge[T](s1:Set[T], s2:Set[T]) = (e:T) => s1(e) || s2(e)
  
  final def add[T](set: Set[T], elem:T) = merge(set, newSet(elem))
  
  /**
   * Checks of the given element elem is present in the Set or not
   */
  final def contains[T](set:Set[T], elem:T):Boolean = set(elem)
  
}

/**
 * Specific implementation for Int Set
 */
object FunctionalIntSet extends GenericFunctionalSet {
  
    def toString(set: Set[Int]) = 
      (for(i <- (-1000 to 1000); if set(i))  yield i).mkString("IntSet: [", ",", "]")
  
    def forall(s:Set[Int], f: Int => Boolean): Boolean = {
      def iterLim(curr: Int): Boolean = {
         if(curr > 1000) true
         else if(contains(s, curr)) f(curr) && iterLim(curr + 1)
         else iterLim(curr + 1)
      }      
      iterLim(-1000)
    }
    
    //See FunctionalSetTest for test cases
    
    def main(args: Array[String]): Unit = {
       val set = (2 to 10).foldLeft(newSet(1)){case (set, elem) => add(set, elem)}
       println(toString(set))
    }
}