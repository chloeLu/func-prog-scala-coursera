package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var counter = 0
    for (c <- chars) {
      c match {
        case '(' => counter += 1
        case ')' => counter -= 1
        case _ =>
      }
      if (counter < 0) return false
    }
    counter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /**
      * Returns the number of unmatched left brackets and unmatched right brackets
      */
    def traverse(from: Int, until: Int) = {
      var l = 0
      var r = 0
      var i = from
      while (i < until) {
        chars(i) match {
          case '(' => l += 1
          case ')' => if (l > 0) l -= 1 else r += 1
          case _ =>
        }
        i += 1
      }
      (l, r)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = (until - from) / 2 + from
        val ((l1: Int, r1: Int), (l2: Int, r2: Int)) = parallel(reduce(from, mid), reduce(mid, until))
        if (l1 >= r2) (l1 - r2 + l2, r1) else (l2, r2 - l1 + r1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
