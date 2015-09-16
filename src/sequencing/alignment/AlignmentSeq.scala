
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
    indelScore : Int, genX : String, genY : String, k : Int) {
  
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
    computeMatrix
  }
  
  def computeMatrix : Unit = {
		var y : Int = 1
		var x : Int = 1
		while (x != borderX-1 || y != borderY-1) {
      if (x == borderX) {
        x = 1
        y += 1
      } else {
        if (genX.charAt(x) == genY.charAt(y))
  		    matrix(x)(y) = matrix(x-1)(y-1)+matchScore
  			else 
  			  matrix(x)(y) = Math.max(matrix(x-1)(y-1)+mismatchScore, Math.max(matrix(x-1)(y)+indelScore,matrix(x)(y-1)+indelScore))
        x += 1
      }
	  }
  }
  
  def backtrace(start : (Int, Int)) : String = {
    var x : Int = start._1
    var y : Int = start._2
    var alignment : String = ""
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
      } else if(matrix(x)(y) == matrix(x-1)(y)+indelScore) {//deletion
          alignment =  "-"+alignment
         x -= 1
      } else if(matrix(x)(y) == matrix(x)(y-1)+indelScore) {//insertion 
         alignment = "+"+alignment
         y -= 1
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
       val alignmentVal : String = backtrace( (c._1,c._2) )
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


