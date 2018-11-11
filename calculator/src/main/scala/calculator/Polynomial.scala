package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal {
    val (a1, b1, c1) = (a(), b(), c())
    b1 * b1 - 4 * a1 * c1
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    def ~=(x: Double, y: Double, precision: Double = 0.0001) = {
      if ((x - y).abs < precision) true else false
    }

    Signal {
      val delta = computeDelta(a, b, c)
      if (delta().isNaN) Set[Double]()
      else if (~=(delta(), 0)) Set(-b() / 2 * a() )
      else Set(-b() + Math.sqrt(delta()) / (2 * a()),
        -b() - Math.sqrt(delta()) / (2 * a()))
    }
  }
}
