package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   * The following pattern of numbers is called Pascal’s triangle.
   *
   *    1        0, 0
   *   1 1       0,1 ; 1,1
   *  1 2 1      0,2; 1,2; 2,2
   * 1 3 3 1     0,3; 1,3; 2,3; 3,3
    1 4 6 4 1
   *  ...
   *  The numbers at the edge of the triangle are all 1,
   *  and each number inside the triangle is the sum of the two numbers above it.
   *  Write a function that computes the elements of Pascal’s triangle by means of a recursive process.
   *
   *  takes a column c and a row r,
   *  counting from 0 and returns the number at that spot in the triangle.
   *  For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
   *  @param c column
   *  @param r row
   *  @return pascal triangle number
   */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || c >= r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String. For example, the function should return true for the following strings:
   *
   *  - (if (zero? x) max (/ 1 x))
   *  - I told him (that it’s not (yet) done). (But he wasn’t listening)
   *
   *  The function should return false for the following strings:
   *
   *  - :-)
   *  - ())(
   *
   *  The last example shows that it’s not enough to verify that a string contains the same number of opening and closing parentheses.
   *
   *
   *  There are three methods on List[Char] that are useful for this exercise:
   *
   *  - chars.isEmpty: Boolean returns whether a list is empty
   *  - chars.head: Char returns the first element of the list
   *  - chars.tail: List[Char] returns the list without the first element
   *
   * *Hint**: you can define an inner function if you need to pass extra parameters to your function.
   *
   * *Testing**: You can use the toList method to convert from a String to aList[Char]: e.g. "(just an) example".toList.
   */
  def balance(chars: List[Char]): Boolean = {

    def _tail[T](list: List[T]): List[T] =
      if (list.isEmpty) list else list.tail

    @tailrec
    def balanceStep(head: Option[Char], tail: List[Char], balanceCounter: Int): Boolean = {
      if (balanceCounter < 0) // no closing brackets before opening brackets
        false
      else {
        head match {
          case None => balanceCounter == 0 // if no more chars, we should be balanced
          case Some('(') => balanceStep(tail.headOption, _tail(tail), balanceCounter + 1)
          case Some(')') => balanceStep(tail.headOption, _tail(tail), balanceCounter - 1)
          case _ => balanceStep(tail.headOption, _tail(tail), balanceCounter)
        }
      }
    }

    balanceStep(chars.headOption, chars.tail, 0)
  }

  /**
   * Exercise 3
   * how many ways can I make change?
   * takes an amount to change,
   * and a list of unique denominations for the coins
   *
   * 1. For each in coins, calculate countChange
   *
   * Dynamic programming:
   * Find a way to use / not to use a coin each time
   *
   * example: 3 ways to change a 4 using 1 and 2:
   * 2 + 2
   * 2 + 1 + 1
   * 1 + 1 + 1 + 1
   *
   *  Using only 1
   *  1 + 1 + 1 + 1
   *  how do we backtrack it?
   *  Well, we need to substract by 1 and see what we have there
   *
   *
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def _tail[T](list: List[T]) =
      if (list.isEmpty) list
      else list.tail

    def countChangeWith(money: Int, currentCoin: Option[Int], coins: List[Int]): Int = {
      money match {
        case money if money < 0 => 0
        case money if money == 0 => 1
        case _ =>
          currentCoin match {
            case Some(coin) =>
              countChangeWith(money - coin, currentCoin, coins) + // Try to use this coin
                countChangeWith(money, coins.headOption, _tail(coins)) // Try to use another coin
            case _ => 0 // No more coins -> we can't do anything, except when no money and we covered that
          }
      }
    }
    countChangeWith(money, coins.headOption, _tail(coins))
  }
}
