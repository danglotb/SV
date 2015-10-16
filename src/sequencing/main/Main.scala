package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._

object Main extends App {
  
  val options = OptionsAlignment.options(Map(), args.toList)
  
  val ref = new sequencing.util.Parser("input/shortGen").parseAll.toUpperCase()

  val reads = sequencing.util.ParserFASTQ.parse("input/shortRead.fastq")

  val b = new BurrowsWheelerTransform(ref + "$", 32)

  val sizeOfSeed = 2

  val arrayReadAligned = new Array[Boolean](reads.length)

  reads.foreach { r =>
    val read = r.toUpperCase
    for (i <- 0 until (read.length / sizeOfSeed)) {
      val seed = read.substring(i * sizeOfSeed, (i + 1) * sizeOfSeed)
      println("#" + i + "\t" + seed)
      val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
      indexSeed.foreach { s =>
        
        val endRef = math.min(s + read.substring(i * sizeOfSeed).length(), ref.length)
        val alignerRight = new Aligner(5, -4, -10, ref.substring(s, endRef),
            read.substring(i * sizeOfSeed), 0, 100)
        val alignmentRight = (alignerRight align)
        
        val refLeft = ref.substring(math.max(0, s - (read.length - read.substring(i * sizeOfSeed).length)), s + sizeOfSeed).reverse
        val readLeft = read.substring(0, (i + 1) * sizeOfSeed).reverse
        val alignerLeft = new Aligner(5, -4, -10, refLeft, readLeft, 0, 100)
        val alignmentLeft = (alignerLeft align)
        
        if ( !(alignmentLeft._1.equals("")) && !(alignmentRight._1.equals("") )) {
          val totalAlignment = AlignerUtil.mergeAlign(alignmentLeft,alignmentRight,sizeOfSeed)
          AlignerUtil.printAlign(totalAlignment)
          arrayReadAligned(reads.indexOf(read)) = true
        }
        
      }
    }
  }
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length)
}