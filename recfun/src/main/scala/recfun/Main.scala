package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal4(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal2(c: Int, r: Int): Int = {
    // invalid parameters
    if (c < 0 || r < 0 || c > r)
      throw new NoSuchElementException

    if (c == 0 || c == r) {
      // sides of the triangle
      1
    } else {
      pascal2(c - 1, r - 1) + pascal2(c, r - 1)
    }
  }
  
  def pascal3(c: Int, r: Int): Int = {
    // invalid parameters
    if (c < 0 || r < 0 || c > r)
      throw new NoSuchElementException

//    @tailrec
    def pascal(c: Int, r: Int, acc: => Int): Int = {
      if (c == 0 || c == r) {
        // sides of the triangle
        1 + acc
      } else {
        pascal(c - 1, r - 1, acc + pascal(c, r - 1, 0))
      }
    }
     
    pascal(c, r, 0)
  }
  
  def pascal4(c: Int, r: Int): Int = {
    
    // invalid parameters
    if (c < 0 || r < 0 || c > r)
      throw new NoSuchElementException

    @tailrec
    def pascal(c: Int, r: Int, toCompute: List[(Int, Int)], acc: Int): Int = {
      if (c == 0 || c == r) {
        // sides of the triangle
        if (toCompute.isEmpty) {
          1 + acc
        } else {
          val head = toCompute.head
          pascal(head._1, head._2, toCompute.tail, 1 + acc)
        }
      } else {
        pascal(c - 1, r - 1, (c, r -1) :: toCompute, acc)
      }
    }
    
    pascal(c, r, Nil, 0)
  }
  
  def pascal(c: Int, r: Int): Int = {
    pascal4(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balance(chars: List[Char], soFar: Int): Boolean = {
      if (chars.isEmpty)
        soFar == 0
      else {
        val first = chars.head

        // adjust the count
        val newSoFar = if (first == '(')
          soFar + 1
        else if (first == ')')
          soFar - 1
        else
          soFar

        // check the rest of the list if not already bad
        if (newSoFar < 0)
          false
        else
          balance(chars.tail, newSoFar)
      }
    }

    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    // negative values are valid during the recursion
    def countChangeInternal(money: Int, coins: List[Int]): Int = {
      if (money < 0) {
        0
      } else if (money == 0) {
        1
      } else if (coins.isEmpty) {
        0
      } else {
        countChangeInternal(money - coins.head, coins) + countChangeInternal(money, coins.tail)
      }
    }

    if (money < 0) {
      throw new IllegalArgumentException
    } else {
      countChangeInternal(money, coins)
    }
  }
}

