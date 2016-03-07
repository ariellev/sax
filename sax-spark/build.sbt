name := "sax-spark"
version := "1.0"
scalaVersion := "2.11.7"
libraryDependencies += "org.apache.spark" % "spark-core_2.11" % "1.6.0"
//libraryDependencies += "com.databricks" % "spark-csv_2.11" % "1.4.0"
libraryDependencies += "net.seninp" % "jmotif-sax" % "1.0.6"

mainClass in(Compile, run) := Some("de.norcom.sax.Sax")
mainClass in(Compile, packageBin) := Some("de.norcom.sax.Sax")