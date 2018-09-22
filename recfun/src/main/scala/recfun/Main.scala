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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def helper(cnt: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) cnt == 0
      else if (cnt < 0) false
      else helper(updateCnt(cnt, chars.head), chars.tail)
    }

    def updateCnt(cnt: Int, currChar: Char): Int = {
      if (currChar == '(') cnt + 1
      else if (currChar == ')') cnt - 1
      else cnt
    }

    helper(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeSorted(money: Int, sorted_coins: List[Int]): Int = {
      if (money == 0) 1
      else if (sorted_coins.isEmpty) 0
      else {
        var sum = 0
        for (m <- money to 0 by -sorted_coins.head){
          sum += countChangeSorted(m, sorted_coins.tail)
        }
        sum
      }
    }

    countChangeSorted(money, coins.sorted)
  }

}
