package sequencing.main

import sequencing.util._
import sequencing.alignment._

object Main extends App {
  
  val options = AlignmentOption.options(Map(), args.toList)
  
  val gen = new Parser(options.getOrElse("gen1", "input/genome")).parseAll
  
  val read = new Parser(options.getOrElse("gen2", "input/r")).parseAll
  
  val s = new AlignmentSeq(5, -4, -10, gen ,read , options.getOrElse("k", 3).toString.toInt, 40)
  
  val z = s.align
  
  s.printAlign(z)
}