package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._

object Main extends App {

  def align(read: String, s : Int, i : Int): Boolean = {

    var alignmentRight: (String, String, String) = ("", "", "")
    var alignmentLeft: (String, String, String) = ("", "", "")

    val endRef = math.min(s + read.substring(i * sizeOfSeed).length(), ref.length)
    val readRight = read.substring(i * sizeOfSeed)
    val refRight = ref.substring(s, endRef)
    val alignerRight = new Aligner((matchScore, mismatchScore, indelScore), ref.substring(s, endRef), read.substring(i * sizeOfSeed), 0)
    alignmentRight = (alignerRight align)

    if (i != 0) {
      val readLeft = read.substring(0,i*sizeOfSeed)
      val refLeft = ref.substring(s-readLeft.length(), s)
      val alignerLeft = new Aligner((matchScore, mismatchScore, indelScore), refLeft, readLeft, 0)
      alignmentLeft = (alignerLeft align)
    }

    if (alignmentLeft._1 != "" && alignmentRight._1 != "")
      alignmentRight = AlignerUtil.mergeAlign(alignmentLeft, alignmentRight, sizeOfSeed)
      
    if ((AlignerUtil.computeRatio(alignmentRight, indelScore, mismatchScore)) / (alignmentRight._1.length * (indelScore.toFloat)) <= ratioError) {
      AlignerUtil.printAlign(alignmentRight)
      true
    } else
      false
  }

  def run(read: String): Unit = {
    for (i <- 0 until (read.length / sizeOfSeed)) {
      val seed = read.substring(i * sizeOfSeed, (i + 1) * sizeOfSeed)
      println("#" + i + "\t" + seed)
      val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
      indexSeed.foreach { s =>
        if (align(read, s, i)) {
          if (reads.indexOf(read) != -1)
            arrayReadAligned(reads.indexOf(read)) = true
          else
            arrayReadAligned(reads.indexOf(read.reverse)) = true
        }
      }
    }

    //Getting the last seed
//        if ((read.length() % sizeOfSeed) != 0) {
//          val seed = read.substring(read.length() - (read.length() % sizeOfSeed))
//          val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
//          indexSeed.foreach { s =>
//    
//            val readAlign = read.reverse
//            val refAlign = ref.substring(s, s+1).reverse
//            val aligner = new Aligner((matchScore, mismatchScore, indelScore), refAlign,
//              readAlign, 0)
//            val alignment = (aligner align)
//    
//            if (AlignerUtil.computeRatio(alignment,indelScore,mismatchScore) / (alignment._1.length * (indelScore.toFloat)) <= ratioError) {
//              AlignerUtil.printAlign(alignment)
//              if (reads.indexOf(read) != -1)
//                arrayReadAligned(reads.indexOf(read)) = true
//              else
//                arrayReadAligned(reads.indexOf(read.reverse)) = true
//            }
//          }
//        }

  }

  val options = OptionsAlignment.options(Map(), args.toList)

  val (matchScore, mismatchScore, indelScore): (Int, Int, Int) = OptionsAlignment.toTuple(options.get("score"))

  val ref = Parser.parse(options.getOrElse("ref", "input/shortGen").toString)

  val reads = Parser.parseFASTQ(options.getOrElse("read", "input/shortRead.fastq").toString)

  val b = new BurrowsWheelerTransform(ref + "$", 16)

  val sizeOfSeed: Int = options.getOrElse("sseed", 25).toString.toInt

  val ratioError: Float = options.getOrElse("ratio", 0.5).toString.toFloat
  
  val arrayReadAligned = new Array[Boolean](reads.length)

  reads.foreach { r =>
//    val r = reads(2)
    val read = r.toUpperCase
    println("%" + reads.indexOf(r) + "\t" + read)
    run(read)
    run(read.reverse)
}
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length)
}