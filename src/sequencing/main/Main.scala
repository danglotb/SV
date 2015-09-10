package sequencing.main

import sequencing.util._
import sequencing.alignment._

object Main extends App {
 //Not used yet
 val options = AlignmentOption.options(Map(), args.toList)
 val genome = Parser.parseFile(options.getOrElse("genome", null))
 val read = Parser.parseFile(options.getOrElse("read", null))
 //
 val s : AlignmentSeq = new AlignmentSeq( "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC", "CTGGAGCCGATAAACGCCGGGAA" ,options.getOrElse("k", 0).toString().toInt)
 s.initMatrix()
 print(s)
 s.buildBacktrace()
}