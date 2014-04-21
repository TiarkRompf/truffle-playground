Truffle Playground
==================

Playing with Truffle in Scala.

What do we get if we combine self-modifying AST interpreters ([Truffle](http://www.christianwimmer.at/Publications/Wimmer12b/)) 
and generative programming ([LMS](http://scala-lms.github.io))?

See 
[src/test/scala/TruffleLMSTest.scala](src/test/scala/TruffleLMSTest.scala) and
[src/main/scala/TruffleLMS.scala](src/main/scala/TruffleLMS.scala)
for some experiments.


### Background

- [Truffle FAQ and Guidelines](https://wiki.openjdk.java.net/display/Graal/Truffle+FAQ+and+Guidelines#TruffleFAQandGuidelines)

- [Truffle API tests](http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test)

- [JRuby/Truffle](http://www.chrisseaton.com/rubytruffle/), [code](https://github.com/jruby/jruby/wiki/Truffle)

### Running

Grab a [Graal VM binary](http://lafo.ssw.uni-linz.ac.at/builds/). I'm using (GraalVM-0.2 OpenJDK-8-b132)[].
Remember to set `JAVA_HOME`. Use `sbt` to compile and run.

### License

GPLv2

### Disclaimers

One or more authors are employees of Oracle Labs.
The views expressed here are their own and do not necessarily reflect the views of Oracle. 
