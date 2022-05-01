import algorithms.*
import scala.util.Random
import algorithms.maximum_subarray.MaximumSubarray._

@main def main: Unit = 
  val maxArraySize = 1500
  val nbRunsPerSize = 100
  for (n <- 1 to maxArraySize by 100){
    var sumDACTime = 0L
    var sumBruteforceTime = 0L
    for(run <- 1 to nbRunsPerSize){
      val list = (1 to n).map(n => Random.between(-100, 101)).toList
      // val list = List(-1, -3, -7, -12)
      // println(f"Original list has n = $n elements")
      val before1 = System.currentTimeMillis
      val max1 = maximumSubarray(list)
      val after1 = System.currentTimeMillis
      val diff1 = after1 - before1
      sumDACTime += diff1
      // println(f"Max subarray D&C runtime = $diff1 ms; sum = ${max1.sum}")
      val before2 = System.currentTimeMillis
      // val max2 = bruteforceMaximumSubarray(list)
      val after2 = System.currentTimeMillis
      val diff2 = after2 - before2
      sumBruteforceTime += diff2
      // println(f"Bruteforce max subarray runtime = $diff2 ms; sum = ${max2.sum}")
      }
      val avgDACTime = sumDACTime.toDouble / nbRunsPerSize
      val avgBruteforceTime = sumBruteforceTime.toDouble / nbRunsPerSize
      println(f"$n,$avgDACTime,$avgBruteforceTime")
  }
