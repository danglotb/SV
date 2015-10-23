package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._

object Main extends App {

  def align(read: String, s: Int, i: Int) {
    val endRef = math.min(s + read.substring(i * sizeOfSeed).length(), ref.length)
    val alignerRight = new Aligner((matchScore, mismatchScore, indelScore), ref.substring(s, endRef),
      read.substring(i * sizeOfSeed), 0)
    val alignmentRight = (alignerRight align)

    val refLeft = ref.substring(math.max(0, s - (read.length - read.substring(i * sizeOfSeed).length)), s + sizeOfSeed).reverse
    val readLeft = read.substring(0, (i + 1) * sizeOfSeed).reverse
    val alignerLeft = new Aligner((matchScore, mismatchScore, indelScore), refLeft, readLeft, 0)
    val alignmentLeft = (alignerLeft align)

    val totalAlignment = AlignerUtil.mergeAlign(alignmentLeft, alignmentRight, sizeOfSeed)

    val ratioIndel = ((((totalAlignment._1.filter { x => x == '+' || x == '-' }.length +
      totalAlignment._2.filter { x => x == '+' || x == '-' }.length).toFloat) / totalAlignment._1.length.toFloat).toFloat) * (indelScore.toFloat)

    val ratioMM = ((((totalAlignment._2.filter { x => x != '|' }).length).toFloat / totalAlignment._2.length.toFloat).toFloat) * (mismatchScore.toFloat)

    if ((ratioIndel + ratioMM) / totalAlignment._1.length * (indelScore.toFloat) < ratioError) {
      AlignerUtil.printAlign(totalAlignment)
      arrayReadAligned(reads.indexOf(read)) = true
    }
  }

  def run(read: String): Unit = {
    for (i <- 0 until (read.length / sizeOfSeed)) {
      val seed = read.substring(i * sizeOfSeed, (i + 1) * sizeOfSeed)
      println("#" + i + "\t" + seed)
      val indexSeed = b.search(seed, seed.length() - 1, 0, 1, ref.length() - 1)
      indexSeed.foreach { s =>
        align(read, s, i)
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

        val ratioIndel = ((((alignment._1.filter { x => x == '+' || x == '-' }.length +
          alignment._2.filter { x => x == '+' || x == '-' }.length).toFloat) / alignment._1.length.toFloat).toFloat) * (indelScore.toFloat)

        val ratioMM = ((((alignment._2.filter { x => x != '|' }).length).toFloat / alignment._2.length.toFloat).toFloat) * (mismatchScore.toFloat)

        if ((ratioIndel + ratioMM) / alignment._1.length * (indelScore.toFloat) < ratioError) {
          AlignerUtil.printAlign(alignment)
          arrayReadAligned(reads.indexOf(read)) = true
        }
      }
    }

  }

  val options = OptionsAlignment.options(Map(), args.toList)

  val (matchScore, mismatchScore, indelScore): (Int, Int, Int) = Util.toTuple(options.get("score"))

  val ref = Parser.parse(options.getOrElse("ref", "input/NC_002549.fna").toString)

  val reads = Parser.parseFASTQ(options.getOrElse("read", "input/SRR1930021.fastq").toString)

  val b = new BurrowsWheelerTransform(ref + "$", 16)

  val sizeOfSeed = 25

  val ratioError = options.getOrElse("ratio", 1.0).toString().toFloat

  val arrayReadAligned = new Array[Boolean](reads.length)

  reads.foreach { r =>
    val read = r.toUpperCase
    run(read)
    run(read.reverse)
  }
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length)
}