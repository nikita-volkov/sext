package sext

import embrace._
import org.joda.time._

object Benchmarking {

  def benchmark [Z] ( f : => Z ) : (Z, Period)
    = {
      val start = Instant.now()
      val result = f
      val period = new Period(start, Instant.now())
      (result, period)
    }
  def benchmarkAndPrint [Z] ( label : String ) ( f : => Z ) : Z
    = benchmark(f) $$ {case (r, p) => println(label + ": " + p); r}
  def benchmarkDoing [Z] ( g : Period => Unit ) ( f : => Z ) : Z
    = benchmark(f) $$ {case (r, p) => g(p); r}

}
