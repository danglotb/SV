package sequencing.main

import sequencing.util._
import sequencing.alignment._

object Main extends App {

  val gen1 = new Parser("input/read").parseAll()
  val gen2 = new Parser("input/genome").parseAll()
  
//  val gen1 = new Parser("input/NC_002549.fna").parseAll()
//  val gen2 = new Parser("input/NC_006432.fna").parseAll()
  
  val s: AlignmentSeq = new AlignmentSeq(5, -4, -10, gen2, gen1, 1)
  s.computeMatrix
  println(s)
  s.buildBacktrace()
  
}