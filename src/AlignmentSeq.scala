
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
      return computeMatrix(0, y+1)
    }
    
    //First case
    if (x == 0 && y == 0) {
        if(genome.charAt(x) == read.charAt(y)) {
          matrix(x)(y) = 1
          computeMatrix(x+1, y)
        } else {
          matrix(x)(y) = 0
          computeMatrix(x+1, y)
        }
    } else {
     //first row
     if (y == 0) {
      if(genome.charAt(x) == read.charAt(y)) {
            matrix(x)(y) = matrix(x-1)(y)+1//match
            computeMatrix(x+1, y)
        } else {
          matrix(x)(y) = matrix(x-1)(y)-1//indel on i
          computeMatrix(x+1, y)
        }
      //first column
      } else if (x == 0) {
        if(genome.charAt(x) == read.charAt(y)) {
            matrix(x)(y) = matrix(x)(y-1)+1//match
            computeMatrix(x+1, y)
        } else {
          matrix(x)(y) = matrix(x)(y-1)-1
          computeMatrix(x+1, y)
        }
      } else if(genome.charAt(x) == read.charAt(y)) {
        matrix(x)(y) = matrix(x-1)(y-1)+1
        computeMatrix(x+1, y)
      } else if(matrix(x)(y-1)-1 > matrix(x-1)(y)-1) {//indel on j
        matrix(x)(y) = matrix(x)(y-1)-1
        computeMatrix(x+1, y)
      } else if (matrix(x-1)(y)-1 > matrix(x-1)(y-1)) {//indel on i
        matrix(x)(y) = matrix(x-1)(y)-1
        computeMatrix(x+1, y)
      } else {//mismatch
        matrix(x)(y) = matrix(x-1)(y-1)
        computeMatrix(x+1, y)
      }
    
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
    } else {
        if(genome.charAt(x) == read.charAt(y))
          return backtrace(Math.max(x-1,0),Math.max(y-1,0),"M"+alignment)
        else {
          if(y == 0) {
            return backtrace(x-1,y,"-"+alignment)
          } else if(x == 0) {
            return backtrace(x,y-1,"+"+alignment)
          } else if(matrix(x)(y) == matrix(x)(y-1)-1) {
            return backtrace(x,y-1,"-"+alignment)
          } else if(matrix(x)(y) == matrix(x-1)(y)-1) {
            return backtrace(x-1,y,"+"+alignment)
          } else if (matrix(x)(y) == matrix(x-1)(y-1)) {
            return backtrace(x-1,y-1,"m"+alignment)
          }
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
    println(t)
    val alignment : String = backtrace(t._1,t._2, "")
    var index : Int = 0
    var genomeAligned : String = ""
    println(alignment)
    alignment.foreach { x =>
      x match {
        case 'M' => genomeAligned = genomeAligned+genome.charAt(index)
        case 'm' => genomeAligned = genomeAligned+genome.charAt(index)
        case '-' => genomeAligned = genomeAligned+"-"
        case '+' => genomeAligned = genomeAligned+"+"
      }
      index += 1
    }
    println(genomeAligned)
    println(read)
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
 val s : AlignmentSeq = new AlignmentSeq("gaattcgagatgcgaatgagcagcagccattttgatgttgtgagcatcggaacgtttctg","ggcacgaggc")
 s.computeMatrix(0, 0)
 s.buildBacktrace()
 print(s)
}

