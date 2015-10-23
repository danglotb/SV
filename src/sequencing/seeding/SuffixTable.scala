package sequencing.seeding

import scala.collection.mutable.ListBuffer

/**
 * @author danglot
 */
object SuffixTable {
  def buildSuffixTable(ref: String): Array[Int] = {
    val suffixIndex = new Array[Int](ref.length)
    for (i <- 0 until ref.length) suffixIndex(i) = i
    val sorted  = suffixIndex.sortWith { (i,j) => sorting(ref,i,j,0)}
    sorted
  }
  
  def sorting(ref : String, i : Int, j : Int, x : Int) : Boolean = {
    if (i+x >= ref.length || j+x >= ref.length)
      i < j
    else if (ref.charAt(i+x) != ref.charAt(j+x))
       ref.charAt(i+x) < ref.charAt(j+x)
    else
      sorting(ref,i,j,x+1)
  }
  
}
