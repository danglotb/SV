package sequencing.main

import sequencing.util._
import sequencing.alignment._

object Main extends App {
  
  val options = AlignmentOption.options(Map(), args.toList)
  
  val s = new AlignmentSeq(5, -4, -10, 
      options.getOrElse("gen1", "input/genome").toString 
      , options.getOrElse("gen2", "input/read").toString , options.getOrElse("k", 3).toString.toInt)
  
  s.align
}