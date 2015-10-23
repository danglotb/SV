package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._

object Main extends App {

  def align(read: String, s: Int, i: Int): Boolean = {
    
    val endRef = math.min(s + read.substring(i * sizeOfSeed).length(), ref.length)
    val alignerRight = new Aligner((matchScore, mismatchScore, indelScore), ref.substring(s, endRef),
      read.substring(i * sizeOfSeed), 0)
    val alignmentRight = (alignerRight align)
    
    val refLeft = ref.substring(math.max(0, s - (read.length - read.substring(i * sizeOfSeed).length)), s + sizeOfSeed).reverse
    val readLeft = read.substring(0, i * sizeOfSeed).reverse + ref.substring(s,s+sizeOfSeed)
    val alignerLeft = new Aligner((matchScore, mismatchScore, indelScore), refLeft, readLeft, 0)
    val alignmentLeft = (alignerLeft align)

    val totalAlignment = AlignerUtil.mergeAlign(alignmentLeft, alignmentRight, sizeOfSeed)
    
    if ((AlignerUtil.computeRatio(totalAlignment,indelScore,mismatchScore)) / (totalAlignment._1.length * (indelScore.toFloat)) <= ratioError) {
      AlignerUtil.printAlign(totalAlignment)
      return true
    }
    return false
  }

  def run(read: String): Unit = {
    for (i <- 0 until (read.length / sizeOfSeed)) {
      val seed = read.substring(i * sizeOfSeed, (i + 1) * sizeOfSeed)
      println("#" + i + "\t" + seed)
      val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
      indexSeed.foreach { s =>
        val aligned = align(read, s, i)
        if (aligned) {
          if (reads.indexOf(read) != -1)
            arrayReadAligned(reads.indexOf(read)) = true
          else
            arrayReadAligned(reads.indexOf(read.reverse)) = true
        }
      }
    }

    //Getting the last seed
    if ((read.length() % sizeOfSeed) != 0) {
      val seed = read.substring(read.length() - (read.length() % sizeOfSeed))
      val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
      indexSeed.foreach { s =>

        val readAlign = read.reverse
        val refAlign = ref.substring(s, read.length()).reverse
        val aligner = new Aligner((matchScore, mismatchScore, indelScore), refAlign,
          readAlign, 0)
        val alignment = (aligner align)

        if (AlignerUtil.computeRatio(alignment,indelScore,mismatchScore) / (alignment._1.length * (indelScore.toFloat)) <= ratioError) {
          AlignerUtil.printAlign(alignment)
          if (reads.indexOf(read) != -1)
            arrayReadAligned(reads.indexOf(read)) = true
          else
            arrayReadAligned(reads.indexOf(read.reverse)) = true
        }
      }
    }

  }

  val options = OptionsAlignment.options(Map(), args.toList)

  val (matchScore, mismatchScore, indelScore): (Int, Int, Int) = Util.toTuple(options.get("score"))

  val ref = Parser.parse(options.getOrElse("ref", "input/shortGen").toString)

  val reads = Parser.parseFASTQ(options.getOrElse("read", "input/shortRead.fastq").toString)

  val b = new BurrowsWheelerTransform(ref + "$", 16)

  val sizeOfSeed = 2

  val ratioError = options.getOrElse("ratio", 0.0).toString().toFloat

  val arrayReadAligned = new Array[Boolean](reads.length)

//  reads.foreach { r =>
  val r = reads(0)
    val read = r.toUpperCase
    run(read)
    run(read.reverse)
//  }
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length)
}