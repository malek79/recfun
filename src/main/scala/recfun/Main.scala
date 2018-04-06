package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))

    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balancing(chars: List[Char], nbreopen: Int): Boolean = {
      if (chars.isEmpty) {
        nbreopen == 0
      } else {
        val open =
          if (chars.head == '(') nbreopen + 1
          else if (chars.head == ')') nbreopen - 1
          else nbreopen
        if (open >= 0) balancing(chars.tail, open)
        else false
      }
    }

    balancing(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)

    }
    count(money, coins.sorted)

  }

}
