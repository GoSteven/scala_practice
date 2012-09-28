package recfun
import common._
import collection.mutable._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  /** use implicit for factorial computation */
  def factorial(n: Int): Int =
    if (n == 0) 1
    else factorial(n - 1) * n
  class Factorizer(n: Int) {
    def ! = factorial(n)
  }
  implicit def int2fact(n: Int) = new Factorizer(n);

  def pascal(c: Int, r: Int): Int = c match  {
    case 0 =>  1
    case _ =>  ((r!)/((r-c)!)) / (c!)
  }
//  def pascal(c: Int, r: Int): Int = if (c==0 || c==r) return 1 else return pascal(c,r-1) + pascal(c-1,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val stack = new ArrayStack[Char]()
    for (c <- chars)  {
      if (c== '(') stack.push(c)
      else if (c == ')') {
        if (stack.isEmpty) return false
        stack.pop()
      }
    }
    if (stack.isEmpty) return true
    else return false
  }

  /**
   * Exercise 3
   */
//  def countChange(money: Int, coins: List[Int]): Int = {
//    var coinsSorted = coins.sortWith(_ > _)
//    return r(money, coinsSorted)
//  }
//  val r:((Int,List[Int])=>Int) = (money: Int, coins: List[Int]) => {
//    if (coins.length == 1) {
//      if (money % coins(0) == 0) 1
//      else 0
//    }
//    else if (coins.length == 0) 0
//    else {
//      if (coins(0) > money)  r(money, coins.tail)
//      else  r(money - coins(0),coins) + r(money, coins.tail)
//    }
//  }
  def countChange(money: Int, coins: List[Int]): Int = {
     r(money,coins)
  }
  val r:((Int,List[Int])=>Int) = (money: Int, coins: List[Int]) => {
    if(money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

}

