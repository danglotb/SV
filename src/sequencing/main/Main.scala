package sequencing.main

import sequencing.util._
import sequencing.alignment._
import sequencing.seeding._


/**
 * Option for Alignment
 */
object OptionParsing {
  
  type Options = Map[String, Any]

  /**
   * method to parse and build a map of the options
   */
  def options(opt: Options, args: List[String]): Options = {
    if (args.isEmpty) return opt
    args match {
      case "-m" :: score :: tail => options(opt++Map("match" -> score), tail)
      case "-mm" :: score :: tail => options(opt++Map("mismatch" -> score), tail)
      case "-i" :: score :: tail => options(opt++Map("indel" -> score), tail)
      case "-k" :: k :: tail => options(opt++Map("k" -> k), tail)
      case "-f" :: gen1 :: gen2 :: tail => options( (opt++Map("gen1" -> gen1))++Map("gen2" -> gen2), tail)
      case "-h" :: tail => usage() ; opt
      case _ => usage() ; opt
     }
   }

   /**
    * print in stdout available option
    */
   def usage() : Unit = {
     print("Options availables : \n")
     print("\t-m <IntValue> to specify the score of a match(5 if not)\n")
     print("\t-mm <IntValue> to specify the score of a mismatch(-4 if not)\n")
     print("\t-i <IntValue> to specify the score of a indel(-10 if not, deletion/insertion)\n")
     print("\t-k <IntValue> to specify the numbers of errors(3 if not)\n")
     print("\t-file <pathToFile> <pathToFile> to specify files of gen you align.\n")
     System.exit(1)
   }
}

object Main extends App {

  println("Parse Genome File...")
  
  val ref = new sequencing.util.Parser("input/Homo_sapiens.GRCh38.dna.chromosome.21.fa").parse
  
  println("Parse Read File...")
  
  val reads = sequencing.util.ParserFASTQ.parse("input/DRR000547.20K.fastq")
  
  val b = new BurrowsWheelerTransform(ref + "$", 64)

  val sizeOfSeed = 25

  val ratioError = 0.1

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
          read.substring(i * sizeOfSeed), 0)
        val alignmentRight = (alignerRight align)

        val refLeft = ref.substring(math.max(0, s - (read.length - read.substring(i * sizeOfSeed).length)), s + sizeOfSeed).reverse
        val readLeft = read.substring(0, (i + 1) * sizeOfSeed).reverse
        val alignerLeft = new Aligner(5, -4, -10, refLeft, readLeft, 0)
        val alignmentLeft = (alignerLeft align)
        val totalAlignment = AlignerUtil.mergeAlign(alignmentLeft, alignmentRight, sizeOfSeed)

        val ratioIndel = ((((totalAlignment._1.filter {x => x == '+' || x == '-'}.length + 
        totalAlignment._2.filter {x => x == '+' || x == '-'}.length).toFloat) / totalAlignment._1.length.toFloat).toFloat)*(-10.toFloat)
        
        val ratioMM = ((((totalAlignment._2.filter { x => x != '|' }).length).toFloat / totalAlignment._2.length.toFloat).toFloat)*(-4.toFloat)
        
        if ( (ratioIndel+ratioMM)/totalAlignment._1.length*(-10.toFloat) < ratioError) {
          AlignerUtil.printAlign(totalAlignment)
          arrayReadAligned(reads.indexOf(read)) = true
        }
      }
    }
  }
  println(arrayReadAligned.toList.filter { x => x == true }.length + " / " + reads.length)
}