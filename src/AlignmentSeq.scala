
/**
 * @author danglot
 * Class for the alignment of sequences
 */

object AlignmentOption {
   def option(args : Array[String]): Map[String, String] = {
    var opt = Map[String,String]()
    opt
   }
   
   def usage() : Unit = {
     println("")
   }
}

class AlignmentSeq(genome : String, read : String) {
  
  /**
   * border of the x 
   */
  val maxSize : Int = Math.max(genome.length()/2 ,read.length())

  /**
   * matrix of scores
   */
  var matrix = Array ofDim[Int](maxSize,read.length())
  
  /**
   * return the matrix of score of the alignment between the genome and the read.
   */
  def computeMatrix(x : Int, y : Int): Array[Array[Int]] = {
    
    //End of the alignment
    if (y == read.length)
      return matrix
      
    //End of row
    if (x == maxSize) {
      computeMatrix(0, y+1) 
      return matrix
    }
    
    //First case
    if (x == 0 && y == 0) {
        if(genome.charAt(x) == read.charAt(y)) {
          matrix(x)(y) = 1
          computeMatrix(x+1, y+1)
        } else {
          matrix(x)(y) = 0
          computeMatrix(x+1, y)
        }
    } else {
     //first row
     if (y == 0) {
      if(genome.charAt(x) == read.charAt(y)) {
            matrix(x)(y) = matrix(x-1)(y)+1//match
            computeMatrix(x+1, y+1)
        } else
          matrix(x)(y) = matrix(x-1)(y)-1//indel on i//indel on i
      //first column
      } else if (x == 0) {
        if(genome.charAt(x) == read.charAt(y)) {
            matrix(x)(y) = matrix(x)(y-1)+1//match
            computeMatrix(x+1, y+1)
        } else
          matrix(x)(y) = matrix(x)(y-1)-1
      } else if(genome.charAt(x) == read.charAt(y)) {
        matrix(x)(y) = matrix(x-1)(y-1)+1
        computeMatrix(x+1, y+1)
      } else if(matrix(x)(y-1)-1 > matrix(x-1)(y)-1) {//indel on j
        matrix(x)(y) = matrix(x)(y-1)-1
      } else if (matrix(x-1)(y)-1 > matrix(x-1)(y-1)) {//indel on i
        matrix(x)(y) = matrix(x-1)(y)-1
      } else {//mismatch
        matrix(x)(y) = matrix(x-1)(y-1)
      }
      computeMatrix(x+1, y)
    }
  }
  
  /**
   * rebuild the "path" to have the greatest score
   */
  def backtrace() : Unit = {
        
  }
   
  /**
   * print on the stdout the matrix of score
   */
  override def toString() : String = {
    var str : String = ""
    var spaces : String = ""
    for (y <- 0 until read.length()) {
      for (x <- 0 until maxSize) {
        for (i <- 0 until 5 - y.toString().length())
           spaces += " "
        str += spaces + matrix(x)(y)
        spaces = ""
      }
      str += "\n"
      }
    str
  }
}

object Main extends App {
 val s : AlignmentSeq = new AlignmentSeq("AAAA","AAAA")
 s.computeMatrix(0, 0)
 print(s)
}

