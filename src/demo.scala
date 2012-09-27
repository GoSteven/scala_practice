import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: silyou
 * Date: 9/20/12
 * Time: 1:31 PM
 * To change this template use File | Settings | File Templates.
 */
object demo extends  {
  def main(args: Array[String]) = {

    var p1 = pascalImproved(0,2)
    println(p1)
    p1 = pascalImproved(1,2)
    println(p1)
    p1 = pascalImproved(1,3)
    println(p1)
    var p2 = countChange(4,List(2,1))
    println(p2)

    var p3 = "(if (zero? x) max (/ 1 x))".toList
    println(balance(p3))
    p3 = ":-)".toList
    println(balance(p3))
  }


  def pascal(c: Int, r: Int): Int = if (c==0 || c==r) return 1 else return pascal(c,r-1) + pascal(c-1,r-1)

  /** use implicit for factorial computation */
  def factorial(n: Int): Int =
    if (n == 0) 1
    else factorial(n - 1) * n
  class Factorizer(n: Int) {
    def ! = factorial(n)
  }
  implicit def int2fact(n: Int) = new Factorizer(n);

  def pascalImproved(c: Int, r: Int): Int = c match  {
    case 0 =>  1
    case _ =>  ((r!)/((r-c)!)) / (c!)
  }


  def balance(chars: List[Char]): Boolean = {
    val stack = new mutable.ArrayStack[Char]()
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

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.length == 1) return 1
    if (coins(0) > money) return countChange(money, coins.tail)
    else return countChange(money - coins(0),coins) + countChange(money, coins.tail)
  }

}



