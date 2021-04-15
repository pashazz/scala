
/**
 calculate a square root using newtons method
 */
object SquareRoot {

  def abs(x: Double) = if (x < 0) -x else x

  def isGoodEnough(root: Double, x: Double): Boolean = {
    abs(root*root - x) < x*0.0001
  }

  def improveGuess(guess: Double, x: Double): Double = // half + r
    (guess + x / guess) / 2

  def squareRoot(root: Double, x: Double): Double = {
    if (isGoodEnough(root, x)) root
    else squareRoot(improveGuess(root, x), x)
  }

  def squareRoot(x: Double) : Double =
    squareRoot(1, x)


}
SquareRoot.squareRoot(1e-6)
SquareRoot.squareRoot(1e60)