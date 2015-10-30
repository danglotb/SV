package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._

object Main extends App {

  def align(read: String, s: Int, i: Int): Boolean = {

    var alignmentRight: (String, String, String) = ("", "", "")
    var alignmentLeft: (String, String, String) = ("", "", "")
  
    val readRight = read.substring(i)
    val endRef = math.min(s + readRight.length(), ref.length)
    val refRight = ref.substring(s, endRef)
    val alignerRight = new Aligner((matchScore, mismatchScore, indelScore), refRight, readRight, 0)
    alignmentRight = (alignerRight align)

    if (i != 0) {
      val readLeft = read.substring(0, i)
      val refLeft = ref.substring(s - readLeft.length(), s)
      val alignerLeft = new Aligner((matchScore, mismatchScore, indelScore), refLeft, readLeft, 0)
      alignmentLeft = (alignerLeft align)
    }

    if (alignmentLeft._1 != "")
      alignmentRight = AlignerUtil.mergeAlign(alignmentLeft, alignmentRight, sizeOfSeed)

    if ((AlignerUtil.computeRatio(alignmentRight, indelScore, mismatchScore)) / (alignmentRight._1.length * (indelScore.toFloat)) <= ratioError) {
      if (print) AlignerUtil.printAlign(alignmentRight)
      true
    } else
      false
  }

  def run(read: String, index : Int): Unit = {
    var i = 0
    while(! arrayReadAligned(index) && i < read.length - sizeOfSeed) {
      val seed = read.substring(i, i + sizeOfSeed)
      val indexSeed = b.search(seed, sizeOfSeed-1 , nbError, 1, ref.length() - 1)
      indexSeed.foreach { s =>
        if (align(read, s, i)) {
        if (print) println("#" + i + "\t" + seed)
          if (reads.indexOf(read) != -1)
            arrayReadAligned(reads.indexOf(read)) = true
          else
            arrayReadAligned(reads.indexOf(read.reverse)) = true
        }
      }
      i = i + 1 
    }
  }

  val options = OptionsAlignment.options(Map(), args.toList)

  val (matchScore, mismatchScore, indelScore): (Int, Int, Int) = OptionsAlignment.toTuple(options.get("score"))

  val ref = Parser.parse(options.getOrElse("ref", "input/shortGen").toString)

  val reads = Parser.parseFASTQ(options.getOrElse("read", "input/shortRead.fastq").toString)

  val b = new BurrowsWheelerTransform(ref + "$", 16)

  val print : Boolean = options.getOrElse("print", false).toString().toBoolean

  val sizeOfSeed : Int = options.getOrElse("sseed", 25).toString.toInt

  val ratioError: Float = options.getOrElse("ratio", 1.0).toString.toFloat

  val nbError : Int = options.getOrElse("error", 1).toString.toInt
  
  val arrayReadAligned = new Array[Boolean](reads.length)

  val time = System.currentTimeMillis()

  reads.foreach { r =>
    val read = r.toUpperCase
    if (print)
      println("%" + reads.indexOf(r) + "\t" + read)
    run(read, reads.indexOf(r))
    run(read.reverse, reads.indexOf(r))
  }
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length + " en " + (System.currentTimeMillis() - time) + " ms")
}