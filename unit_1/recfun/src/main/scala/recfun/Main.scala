package recfun

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
    def pascal(c: Int, r: Int): Int =
      if (r < 0 || c < 0) 0
      else if (r >= 0 && r < 2 && c == 0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isBalanced(list: List[Char], openParenthese: Int, closeParenthese: Int): Boolean =
        if (list.isEmpty) openParenthese == closeParenthese
        else if (list.head.equals('(')) isBalanced(list.tail, openParenthese+1, closeParenthese);
        else if (list.head.equals(')') && openParenthese-1 < closeParenthese) false
        else if (list.head.equals(')') && openParenthese-1 >= closeParenthese)
          isBalanced(list.tail, openParenthese, closeParenthese+1);
        else isBalanced(list.tail, openParenthese, closeParenthese);
      isBalanced(chars, 0, 0)
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countWays(money: Int, coins: List[Int], isFirstCall: Boolean): Int =
        if (money == 0 && !isFirstCall) 1
        else if (money > 0 && coins.nonEmpty)
          countWays(money - coins.head, coins, false) +
            countWays(money, coins.tail, false)
        else 0
      countWays(money, coins, true)
    }
  }
