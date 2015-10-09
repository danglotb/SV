
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
    indelScore : Int, genX : String, genY : String, k : Int, nbError : Int) {
  
  private val borderX : Int = genX.length
  
  private val borderY : Int = genY.length
  
  /**
   * matrix of scores
   */
  private val matrix = Array ofDim[Int](borderX, borderY)
  
  /**
   * Compute the matrix
   */
  def compute : Unit = {
    for (x <- 0 until borderX)
      matrix(x)(0) = x * indelScore
    for (y <- 0 until borderY)
      matrix(0)(y) = y * indelScore
    matrix(0)(0) = if(this.genX.charAt(0) == genY.charAt(0)) matchScore else mismatchScore
    for (x <- 1 until borderX) {
      for (y <- 1 until borderY) {
          if (genX.charAt(x) == genY.charAt(y))
            matrix(x)(y) = matrix(x-1)(y-1)+matchScore
          else 
            matrix(x)(y) = Math.max(matrix(x)(y-1)+indelScore, Math.max(matrix(x-1)(y-1)+mismatchScore,matrix(x-1)(y)+indelScore))
      }
    }
  }
  
  /**
   * build a string of the operations made to align
   */
  private def buildBacktraceStr(x : Int, y : Int,
      alignment : String) : String = {
    if (x == 0) {
      if (y == 0) {
        if(genX.charAt(x) == genY.charAt(y))
         return "|"+alignment//match
        else
         return " "+alignment//mismatch
      } else 
        buildBacktraceStr(0 , y-1, "+"+alignment)
      } else if (y == 0) {
        buildBacktraceStr(x-1, 0, "-"+alignment)
      } else if(matrix(x)(y) == matrix(x)(y-1)+indelScore) {//insertion 
        buildBacktraceStr(x,y-1, "+"+alignment)
      } else if(matrix(x)(y) == matrix(x-1)(y)+indelScore) {//deletion
        buildBacktraceStr(x-1,y, "-"+alignment)
      } else if(matrix(x)(y) == matrix(x-1)(y-1)+matchScore) {//match
        buildBacktraceStr(x-1,y-1, "|"+alignment)
      } else {
        buildBacktraceStr(x-1,y-1, " "+alignment)
      }
  }
  
  /**
   * build 3 string from the backtrace string
   */
  private def buildAlignmentStr(backtrace : String, cursor : Int, strGen : String, indexGen : Int,
      strAlign : String, strRead : String, indexRead : Int) : (String,String,String) = {
    if (cursor >= backtrace.length())
       (strGen,strAlign,strRead)
    else {
      backtrace.charAt(cursor) match {
          case '|' => {
            buildAlignmentStr(backtrace, cursor+1, (strGen+genX.charAt(indexGen)), indexGen+1,
                (strAlign+"|"), (strRead+genY.charAt(indexRead)) , indexRead+1)
          }
          case ' ' => {
             buildAlignmentStr(backtrace, cursor+1, (strGen+genX.charAt(indexGen)), indexGen+1,
                (strAlign+" "), (strRead+genY.charAt(indexRead)) , indexRead+1)
          }
          case '-' => {
             buildAlignmentStr(backtrace, cursor+1, (strGen+genX.charAt(indexGen)), indexGen+1,
                (strAlign+" "), (strRead+"-") , indexRead)
          }
          case '+' => {
            buildAlignmentStr(backtrace, cursor+1, (strGen+"+"), indexGen,
                (strAlign+" "), (strRead+genY.charAt(indexRead)) , indexRead+1)
          }
      }
    }
  }
  
  private def getMax(max : Int, x : Int, xMax : Int) : (Int, Int) = {
    if (x == borderX)
      return (max, xMax)
    if (matrix(x)(borderY-1) > max) {
      getMax(matrix(x)(borderY-1), x+1, x)
    }  else
      getMax(max, x+1, xMax)
  }
  
  /**
   * Method to build the backtrace after the computation of the matrix and provide a print in stdout of the alignment
   */
  def backtrace : (String, String, String) = {
    val max = getMax(Int.MinValue, 0 , 0)
    val alignmentVal : String = buildBacktraceStr( max._2, borderY-1,  "")
//  if ( ((alignmentVal.filter{ x => (x == '-' || x == '+' || x.isSpaceChar)}.length)*100)/borderX > nbError) {
    if ( ((alignmentVal.filter{ x => (x == '-' || x == '+' )}.length)*100)/borderX > nbError) {
      return ("", "", "")
    }
    buildAlignmentStr(alignmentVal, 0, "", 0, "", "", 0)
  }
  
  def align : (String, String, String) = {
   compute
   backtrace
  }
  
  def printAlign(alignmentStr : (String, String, String)) = {
    println(alignmentStr._1)
    println(alignmentStr._2)
    println(alignmentStr._3)
    println("numbers of match : " + (alignmentStr._2.filter { x => x == '|' }).length())
    println("numbers of gaps : " + (alignmentStr._1.filter { x => x == '+'}.length() + alignmentStr._3.filter {x => x == '-'}.length))
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


