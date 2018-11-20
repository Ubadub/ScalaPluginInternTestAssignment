package warmup

object Warmup {
  def f(x: Int): Int = {
    def go(x: Int, acc: Int): Int = {
      if (x == 0) acc
      else go(x - 1, acc * 2)
    }

    go(x, 1)
  }

  def main(args: Array[String]): Unit = {
    println(f(0))
    println(f(1))
    println(f(5))
    println(f(10))
  }
}
