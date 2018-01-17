class Rational(num: Int, den: Int) {
  require(den != 0)

  private val g = gcd(num, den)

  val numer = num / g
  val denom = den / g


  def +(rational: Rational) = new Rational(numer * rational.denom + denom * rational.numer, denom * rational.denom)
  def -(rational: Rational) = this + -rational
  def unary_- = new Rational(-numer, denom)
  def unary_! = new Rational(denom, numer)
  def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) = this * !that

  override def equals(obj: Any): Boolean = if (obj.isInstanceOf[Rational]) {
    val other = obj.asInstanceOf[Rational]
    other.numer == numer && other.denom == denom
  } else {
    false
  }

  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
}
