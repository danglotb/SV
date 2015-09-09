
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
  def backtrace(x : Int, y : Int, alignment : String) : String = {
    
    //reach origin
    if (x == 0 && y == 0) {
       if(genome.charAt(x) == read.charAt(y))
         return "M"+alignment
       else 
         return "m"+alignment
    }
    
    if(genome.charAt(x) == read.charAt(y))
      return backtrace(x-1,y-1,"M"+alignment)
    else {
      
      if(y != 0  && matrix(x)(y) == matrix(x)(y-1)-1) {
        return backtrace(x,y-1,"-"+alignment)
      }
    
      if(x != 0 && matrix(x)(y) == matrix(x-1)(y)-1) {
        return backtrace(x-1,y,"-"+alignment)
      }
      
      if ((x != 0 && y != 0) && matrix(x)(y) == matrix(x-1)(y-1)) {
        return backtrace(x-1,y-1,"m"+alignment)
      }
      
    }
    alignment
  }
  
  def buildBacktrace() : Unit = {
    var t : (Int, Int) = (-1,-1)
    var max = -maxSize
    for (x <- 0 until maxSize) {
      if (matrix(x)(read.length()-1) > max) {
        max = matrix(x)(read.length()-1) 
        t=(x,read.length()-1)
      }
    }
    println(backtrace(t._1,t._2, ""))
  }
   
  /**
   * print on the stdout the matrix of score
   */
  override def toString() : String = {
    var str : String = ""
    for (y <- 0 until read.length()) {
      for (x <- 0 until maxSize) {
        str += "\t" + matrix(x)(y)
      }
      str += "\n"
      }
    str
  }
}

object Main extends App {
 val s : AlignmentSeq = new AlignmentSeq("AAACATCGTTACAAAACATCGATGATACGATATGAC","ATGCAATAT")
 s.computeMatrix(0, 0)
 s.buildBacktrace()
 print(s)
}

