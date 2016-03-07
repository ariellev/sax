package de.norcom.sax

import java.text.SimpleDateFormat
import org.apache.spark.Partitioner

object Sax {

  class DatePartitioner[V](partitions: Int, from: String, to: String) extends Partitioner {
    val fmt = new SimpleDateFormat("yyyy-MM-dd")

    def getTime(date: String): Long = {
      fmt.parse(date).getTime
    }

    val fromTime: Long = getTime(from)
    val toTime: Long = getTime(to)

    val W: Long = (toTime - fromTime) / partitions

    def getPartition(key: Any): Int = {
      val d = key.asInstanceOf[String]    	
      return ((getTime(d) - fromTime) / W).toInt
    }

    def numPartitions(): Int = partitions
  }

  def main(args: Array[String]) = {
    if (args.length < 4) {
      println("usage: [master] [switch] [input] [output]")
      System.exit(1)
    }
  }
}