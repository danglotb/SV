package sequencing.seeding

import scala.collection.mutable.ListBuffer
/**
 * @author danglot
 */
object SuffixTable {
  def buildSuffixTable(ref: String) : Array[Int] = {
    val suffixTable = new Array[String](ref.length)
    val suffixIndex = new Array[Int](ref.length)
    for (i <- 0 until ref.length) {
      suffixTable(i) = ref.substring(i)
      suffixIndex(i) = i
    }
  suffixIndex.sortBy { x => suffixTable(x) }
  }
}
