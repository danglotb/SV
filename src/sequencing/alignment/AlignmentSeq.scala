
package sequencing.alignment

/**
 * @author danglot
 * Class for the alignment of sequences
 */

/**
 * Option for Alignment
 */
object AlignmentOption {
  
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

/**
 * Class to align to sequence
 */
class AlignmentSeq(matchScore : Int, mismatchScore : Int,
    indelScore : Int, genX : String, genY : String, k : Int) {
  
  /**
   * border of the x 
   */
  private val borderX : Int = genX.length()
  
  /**
   * border of the y
   */
  private val borderY : Int = genY.length()

  /**
   * matrix of scores
   */
  private val matrix = Array ofDim[Int](borderX, borderY)

  /**
   * Compute the matrix
   */
  def compute : Unit = {
    for (x <- 0 until borderX)
      matrix(x)(0) = -x
    for (y <- 0 until borderY)
      matrix(0)(y) = -y
    matrix(0)(0) = if(this.genX.charAt(0) == genY.charAt(0)) matchScore else mismatchScore
    for (y <- 1 until borderY)
      for (x <- Math.max(1, Math.abs(y-k)) until borderX) {
          if (genX.charAt(x) == genY.charAt(y))
            matrix(x)(y) = matrix(x-1)(y-1)+matchScore
          else 
            matrix(x)(y) = Math.max(matrix(x)(y-1)+indelScore, Math.max(matrix(x-1)(y-1)+mismatchScore,matrix(x-1)(y)+indelScore))
      }
  }
  
  /**
   * build a string of the operations made to align
   */
  private def buildBacktrace(start : (Int, Int)) : String = {
    var x : Int = start._1
    var y : Int = start._2
    var alignment : String = "-" * (borderX - start._1 -1)
    while (x != 0 || y != 0) {
     if (x == 0) {//insertion 
         alignment = "+"+alignment
         y -= 1
     } else if (y == 0) {//deletion
         alignment =  "-"+alignment
         x -= 1
     } else if(matrix(x)(y) == matrix(x-1)(y-1)+matchScore) {//match
         alignment = "|"+alignment
         y -= 1
         x -= 1
      } else if(matrix(x)(y) == matrix(x-1)(y-1)+mismatchScore) {//mismatch
         alignment = " "+alignment
         y -= 1
         x -= 1
      } else if(matrix(x)(y) == matrix(x)(y-1)+indelScore) {//insertion 
         alignment = "+"+alignment
         y -= 1
      } else if(matrix(x)(y) == matrix(x-1)(y)+indelScore) {//deletion
          alignment =  "-"+alignment
         x -= 1
      }
    }
    if(genX.charAt(x) == genY.charAt(y))
         return "|"+alignment//match
    else
         return " "+alignment//mismatch
  }
  
  
  /**
   * Method to build the backtrace after the computation of the matrix and provide a print in stdout of the alignment
   */
  private def backtrace() : Unit = {
    var max = -borderX
    var coord : (Int, Int) = (0,0)
    for (x <- 0 until borderX)
      if (matrix(x)(borderY-1) > max) {
        max = matrix(x)(borderY-1) 
        coord = (x,borderY-1)
      }
       println("Max Score : " + max)
       var indexGen : Int = 0
       var indexRead : Int = 0
       var genomeAligned : String = ""
       var readAligned : String = ""
       var alignment : String = ""
       val alignmentVal : String = buildBacktrace( coord )
       alignmentVal.foreach { a =>
        a match {
        case '|' => {
          genomeAligned = genomeAligned + genX.charAt(indexGen)
          readAligned = readAligned + genY.charAt(indexRead)
          alignment = alignment + "|"
          indexRead = indexRead + 1
          indexGen = indexGen + 1
        }
        case ' ' => {
          genomeAligned = genomeAligned + genX.charAt(indexGen)
          readAligned = readAligned + genY.charAt(indexRead)
          alignment = alignment + " "
          indexRead = indexRead + 1
          indexGen = indexGen + 1
        }
        case '-' => {
          genomeAligned = genomeAligned + genX.charAt(indexGen)
          readAligned = readAligned + "-"
          alignment = alignment + " "
          indexGen = indexGen + 1
        }
        case '+' => {
          genomeAligned = genomeAligned + "+"
          readAligned = readAligned + genY.charAt(indexRead)
          alignment = alignment + " "
          indexRead = indexRead + 1
        }
        }
      }
      println(genomeAligned)
      println(alignment)
      println(readAligned)
      println("numbers of match : " + (alignment.filter { x => x == '|' }).length())
  }
  
  def align() : Unit = {
    compute
    backtrace
  }
   
  /**
   * print on the stdout the matrix of score
   */
  override def toString() : String = {
    var str : String = ""
    for (y <- 0 until genY.length()) {
      for (x <- 0 until borderX) {
        str += "\t" + matrix(x)(y)
      }
      str += "\n"
      }
    str
  }
}


