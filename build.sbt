name := "truffle-playground"

version := "0.2"

scalaVersion := "2.10.2"

scalaOrganization := "org.scala-lang.virtualized"

// tests are not thread safe
parallelExecution in Test := false


resolvers += "Truffle" at "http://lafo.ssw.uni-linz.ac.at/nexus/content/repositories/releases/"

libraryDependencies += "com.oracle" % "truffle" % "0.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.0"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

retrieveManaged := true

offline := false

