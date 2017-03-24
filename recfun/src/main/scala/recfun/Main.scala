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
      if(c<0 || r<0) throw new IllegalArgumentException
        else if (c==0 || c==r) 1
      else pascal(c-1,r-1)+pascal(c,r-1)

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def darab(chars: List[Char], char: Char): Int = {
        chars.count(char => true)
      }
      if (chars.isEmpty) true
      else if (darab(chars,'(') != darab(chars,')')) false
      else if (chars.tail.isEmpty) false
        else if chars.tail
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
