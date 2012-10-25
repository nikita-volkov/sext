package sext

import embrace._

object Benchmarking {

  def benchmark [Z] ( f : => Z ) : (Z, Long)
    = {
      val start = System.nanoTime()
      val result = f
      val period = System.nanoTime() - start
      (result, period)
    }
  def benchmarkAndPrint [Z] ( label : String ) ( f : => Z ) : Z
    = benchmark(f) $$ {case (r, p) => println(label + ": " + p); r}
  def benchmarkDoing [Z] ( g : Long => Unit ) ( f : => Z ) : Z
    = benchmark(f) $$ {case (r, p) => g(p); r}

}
