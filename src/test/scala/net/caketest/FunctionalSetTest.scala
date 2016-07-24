package net.caketest

import org.scalatest._

import net.caketest.FunctionalIntSet._

class FunctionalSetTest extends FlatSpec with Matchers {
  
  val set = (2 to 10).foldLeft(newSet(1)){case (set, elem) => add(set, elem)}
  
  "forall test to see if all numbers belong to range 1 to 10" should " return true" in {
    forall(set, x => x > 0 && x < 11) should be (true)  
  }
  
  "forall test to see if all numbers belong to range 1 to 5" should " return false" in {
    forall(set, x => x < 5) should be (false)  
  }
  
  "set(n) " should "return true for numbers existing in the set and false for others" in {
    set(2) should be (true)
    set(100) should be (false)
  }
  
  "adding a non existing number in a set " should " add the number in the set" in {
    set(100) should be (false)
    add(set, 100)(100) should be (true)
  }
  
  
  "merging two sets " should " be a union of two sets" in {
    val set1 = add(newSet(1), 2)
    val set2 = add(newSet(3), 4)
    set1(1) should be (true)
    set1(2) should be (true)
    set1(3) should be (false)
    set1(4) should be (false)
    
    set2(1) should be (false)
    set2(2) should be (false)
    set2(3) should be (true)
    set2(4) should be (true)
    
    val set3 = merge[Int](set1, set2)
    set3(1) should be (true)
    set3(2) should be (true)
    set3(3) should be (true)
    set3(4) should be (true)
  }  
}