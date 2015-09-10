package sequencing.main

import sequencing.util._
import sequencing.alignment._

object Main extends App {
  
 val options = AlignmentOption.options(Map(), args.toList)
 val genome = new Parser(options.getOrElse("genome", null), 2)
 val read = new Parser(options.getOrElse("read", null))
 val readStr =  read.parseAll().substring(0,50)
 
 var s : AlignmentSeq = new AlignmentSeq(genome.parse(), readStr ,options.getOrElse("k", 0).toString().toInt)
 s.initMatrix()
 s.buildBacktrace()
 
 s = new AlignmentSeq(genome.parse(), readStr ,options.getOrElse("k", 0).toString().toInt)
 s.initMatrix()
 s.buildBacktrace()
 
 s = new AlignmentSeq(genome.parse(), readStr ,options.getOrElse("k", 0).toString().toInt)
 s.initMatrix()
 s.buildBacktrace()
}