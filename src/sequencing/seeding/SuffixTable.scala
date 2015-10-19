package sequencing.seeding

import scala.collection.mutable.ListBuffer
/**
 * @author danglot
 */
object SuffixTable {
  def buildSuffixTable(ref: String): Array[Int] = {
    val suffixIndex = new Array[Int](ref.length)
    for (i <- 0 until ref.length)
      suffixIndex(i) = i
    suffixIndex.sortBy { x => ref.substring(x) }
  }
}
