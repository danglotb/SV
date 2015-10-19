
package sequencing.alignment

/**
 * @author danglot
 * Class for the alignment of sequences
 */

object AlignerUtil {
   def mergeAlign(alignmentLeft : (String, String, String),
      alignmentRight : (String, String, String), sizeOfSeed : Int) : (String, String, String) = {
    (alignmentLeft._1+alignmentRight._1.substring(sizeOfSeed),alignmentLeft._2+alignmentRight._2.substring(sizeOfSeed),
    alignmentLeft._3+alignmentRight._3.substring(sizeOfSeed))
  }
  
  def printAlign(alignmentStr : (String, String, String)) = {
    val sizeOfChunk = 100
    for (i <- 0 until ((alignmentStr._1.length) / sizeOfChunk)) {
      println(alignmentStr._1.substring(i*sizeOfChunk, (i+1)*sizeOfChunk))
      println(alignmentStr._2.substring(i*sizeOfChunk, (i+1)*sizeOfChunk))
      println(alignmentStr._3.substring(i*sizeOfChunk, (i+1)*sizeOfChunk))
      println()
    }
    println(alignmentStr._1.substring(alignmentStr._1.length-(alignmentStr._1.length%sizeOfChunk), alignmentStr._1.length))
    println(alignmentStr._2.substring(alignmentStr._2.length-(alignmentStr._1.length%sizeOfChunk), alignmentStr._1.length))
    println(alignmentStr._3.substring(alignmentStr._3.length-(alignmentStr._1.length%sizeOfChunk), alignmentStr._1.length))
    println()
    println("numbers of match : " + (alignmentStr._2.filter { x => x == '|' }).length())
    println("numbers of gaps : " + (alignmentStr._1.filter { x => x == '+'}.length() + alignmentStr._3.filter {x => x == '-'}.length))
  }
}

/**
 * Class to align to sequence
 */
class Aligner(matchScore : Int, mismatchScore : Int,
    indelScore : Int, genX : String, genY : String, k : Int) {
  
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
    for (y <- 1 until borderY) {
      for (x <- Math.max(1, y-k) until Math.min(borderX-(k-y),borderX)) {
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
    buildAlignmentStr(alignmentVal, 0, "", 0, "", "", 0)
  }
  
  def align : (String, String, String) = {
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


