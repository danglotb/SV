
/**
 * @author danglot
 */
class AlignmentSeq(genome : String, read : String) {
  
  val maxSize : Int = genome.length()/2

  var matrix = Array ofDim[Int](maxSize,read.length())
  
  alignment(0,0).foreach {x => 
    x.foreach { y => 
      print(y) ; 
      }
    print("\n")
  }
  
  def alignment(i : Int, j : Int) : Array[Array[Int]] = {
    
    if (j == read.length)
      return matrix
      
    if (i == maxSize) {
      alignment(0,j+1) 
      return matrix
    }
      
    if (i == 0 && j == 0) {
        if(genome.charAt(i) == read.charAt(j)) {
          matrix(i)(j) = 1
          alignment(i+1,j+1)
        } else {
          matrix(i)(j) = 0
          alignment(i+1,j)
        }
    } else {
     if (j == 0) {
      if(genome.charAt(i) == read.charAt(j)) {
            matrix(i)(j) = matrix(i-1)(j)+1
            alignment(i+1,j+1)
        } else
          matrix(i)(j) = matrix(i-1)(j)-1
      } else if (i == 0) {
        if(genome.charAt(i) == read.charAt(j)) {
            matrix(i)(j) = matrix(i)(j-1)+1
            alignment(i+1,j+1)
        } else
          matrix(i)(j) = matrix(i)(j-1)-1
      } else if(matrix(i)(j-1)-1 > matrix(i-1)(j)-1) {//indel on j
        matrix(i)(j) = matrix(i)(j-1)-1
      } else if (matrix(i-1)(j)-1 > matrix(i-1)(j-1)) {//indel on i
        matrix(i)(j) = matrix(i-1)(j)-1
      } else {//mismatch
        matrix(i)(j) = matrix(i-1)(j-1)
      }
      alignment(i+1,j)
    }
  }
}

object Main extends App {
 val s : AlignmentSeq = new AlignmentSeq("TACGATCA","AAAA")
 
}

