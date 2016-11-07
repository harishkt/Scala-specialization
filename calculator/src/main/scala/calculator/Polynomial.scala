package calculator
import Math._
object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal { pow(b(),2) - (4* a() * c()) }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else {
        Set(
          (-b() + sqrt(delta()))/ 2*a(),
          (-b() - sqrt(delta()))/ 2*a()
        )
      }
    }

  }
}
