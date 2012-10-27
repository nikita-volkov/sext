package sext

object Benchmarking {

  /**
   * Execute a function and return its result paired with execution time
   * @param f The function to execute
   * @tparam Z Function execution result
   * @return A pair of function result and running time in nanoseconds
   */
  def benchmark [Z] ( f : => Z ) : (Z, Long)
    = {
      val start = System.nanoTime()
      val result = f
      val period = System.nanoTime() - start
      (result, period)
    }
  /**
   * Execute and get result of a function while printing the time it took to execute it
   * @param label A label to prepend the outputted time with
   * @param f A function to execute
   * @tparam Z Result of the executed function
   * @return Result of the executed function
   */
  def benchmarkAndPrint [Z] ( label : String ) ( f : => Z ) : Z
    = benchmark(f) match {case (r, p) => println(label + ": " + p); r}
  /**
   * Execute and get result of function `f` while executing a function `g` on nanoseconds it took to execute function `f`
   * @param g Function on nanoseconds
   * @param f Function to measure execution time of
   * @tparam Z Result of function `f`
   * @return Result of function `f`
   */
  def benchmarkDoing [Z] ( g : Long => Unit ) ( f : => Z ) : Z
    = benchmark(f) match {case (r, p) => g(p); r}

}
