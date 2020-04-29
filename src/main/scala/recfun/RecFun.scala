package recfun

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
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def verify(chars: List[Char], parentheses: Int = 0): Int = {
      if (chars.isEmpty || parentheses < 0)
        parentheses
      else {
        val char = chars.head
        verify(chars.tail, if (char.equals('(')) parentheses + 1 else if (char.equals(')')) parentheses - 1 else parentheses)
      }
    }

    verify(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def combinations(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        combinations(money - coins.head, coins) + combinations(money, coins.tail)
    }

    combinations(money, coins)
  }
}
