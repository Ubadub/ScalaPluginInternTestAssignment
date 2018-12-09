package warmup

object Warmup {
  def f(x: Int): Int = {
    def go(x: Int, acc: Int): Int = {
      if (x == 0) acc
      else go(x - 1, acc * 2)
    }

    go(x, 1)
  }

  def ff(x: Int): Int = {
    def pow(a: Int, b: Int, acc: Int): Int = {
      if (b == 0) acc
      else if (b == 1) a * acc
      else if (b % 2 == 1) pow(a * a, (b-1)/2, a * acc)
      else pow(a * a, b/2, acc)
    }

    pow(2, x, 1)
  }

  def fff(x: Int): Int = 1 << x
}
