
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

  def options(opt: Options, args: List[String]): Options = {
    if (args.isEmpty) return opt
    args match {
      case "-g" :: genome :: tail => options(opt++Map("genome" -> genome), tail)
      case "-r" :: read :: tail => options(opt++Map("read" -> read), tail)
      case "-k" :: k :: tail => options(opt++Map("k" -> k), tail)
      case "-h" :: tail => usage() ; opt
      case _ => usage() ; opt
     }
   }
   
   def usage() : Unit = {
     print("Options availables : \n")
     print("\t-g <pathToGenomeFile> to specify the path the genome file.\n")
     print("\t-r <pathToReadFile> to specify the path the read file.\n")
     print("\t-k <IntValue> to specify the numbers of errors.\n")
     System.exit(1)
   }
}

class AlignmentSeq(matchScore : Int, mismatchScore : Int,
    indelScore : Int, genY : String, genX : String, k : Int) {
  
  /**
   * border of the x 
   */
  val borderX : Int = genX.length()
  
  /**
   * border of the y
   */
  val borderY : Int = genY.length()

  /**
   * matrix of scores
   */
  val matrix = Array ofDim[Int](borderX, borderY)

  /**
   * Method init to fill the 1rst row/col and launch the computation at (1,1)
   */
  def initMatrix() : Unit = {
    for (x <- 0 until borderX)
      matrix(x)(0) = -x
    for (y <- 0 until borderY)
      matrix(0)(y) = -y
    matrix(0)(0) = if(this.genX.charAt(0) == genY.charAt(0)) 1 else 0
//    computeMatrix(1,1)
    computeMatrixLoop
  }
  
  /**
   * The real method for compute the whole matrix of score
   */
  def computeMatrix(x : Int, y : Int) : Unit = {
    if (y == borderY) 
      return
    else if (x == borderX) 
      computeMatrix(1,y+1)
    else if (math.abs(x-y) > k)
      if (x > y)
        computeMatrix(1,y+1)
      else
        computeMatrix(x+1,y)
    else {
      if (genX.charAt(x) == genY.charAt(y))
        matrix(x)(y) = matrix(x-1)(y-1)+matchScore
      else {
        matrix(x)(y) = Math.max(matrix(x-1)(y-1)+mismatchScore,Math.max(matrix(x-1)(y)+indelScore,matrix(x)(y-1)+indelScore))
      }
      computeMatrix(x+1,y)
   }
  }
  
  def computeMatrixLoop() : Unit = {
        
    for (y <- 1 until borderY) {
      for (x <- 1 until borderX) {
        if (genX.charAt(x) == genY.charAt(y))
        matrix(x)(y) = matrix(x-1)(y-1)+matchScore
      else 
        matrix(x)(y) = Math.max(matrix(x-1)(y-1)+mismatchScore,Math.max(matrix(x-1)(y)+indelScore,matrix(x)(y-1)+indelScore))
      }
    }
  }
  
  /**
   * Method to build the backtrace from the matrix
   */
  def backtrace(x : Int, y : Int, alignment : String) : String = {
    if (x == 0 && y == 0) {
       if(genX.charAt(x) == genY.charAt(y))
         return "|"+alignment//match
       else
         return " "+alignment//mismatch
     } else if (x == 0) {
       backtrace(x, y-1, "+"+alignment)//insertion 
     } else if (y == 0) {
       backtrace(x-1, y, "-"+alignment)//deletion
     } else {
       if(matrix(x)(y) == matrix(x-1)(y-1)+matchScore)//match
         backtrace(x-1,y-1, "|"+alignment)
       else if(matrix(x)(y) == matrix(x-1)(y-1)+mismatchScore)//mismatch
         backtrace(x-1,y-1, " "+alignment)
       else if(matrix(x)(y) == matrix(x-1)(y)+indelScore)//deletion
         backtrace(x-1, y, "-"+alignment)
       else if(matrix(x)(y) == matrix(x)(y-1)+indelScore)//insertion 
         backtrace(x,y-1,"+"+alignment)
       else
         return alignment
     }
    
  }
  
  /**
   * Method to build the backtrace after the computation of the matrix and provide a print in stdout of the alignment
   */
  def buildBacktrace() : Unit = {
    val listCoordMax : scala.collection.mutable.ListBuffer[(Int, Int)] = new scala.collection.mutable.ListBuffer[(Int, Int)]()
    var max = -borderX
    for (x <- 0 until borderX)
      if (matrix(x)(borderY-1) > max)
        max = matrix(x)(borderY-1) 
    
    for (x <- 0 until borderX)
      if (matrix(x)(borderY-1) == max) 
        listCoordMax += ( (x, (borderY-1)) )
    println("Max Score : " + max )
    listCoordMax.foreach { c =>
       var indexGen : Int = 0
       var indexRead : Int = 0
       var genomeAligned : String = ""
       var readAligned : String = ""
       var alignment : String = ""
       val alignmentVal : String = backtrace(c._1,c._2, "")
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
          readAligned = readAligned + " "
          alignment = alignment + "-"
          indexGen = indexGen + 1
        }
        case '+' => {
          genomeAligned = genomeAligned + " "
          readAligned = readAligned + genY.charAt(indexRead)
          alignment = alignment + "+"
          indexRead = indexRead + 1
        }
        }
      }
      println(genomeAligned)
      println(alignment)
      println(readAligned)
    }
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


