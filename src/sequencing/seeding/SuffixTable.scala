package sequencing.seeding

import scala.collection.mutable.ListBuffer
/**
 * @author danglot
 */
object SuffixTable {
  def buildSuffixTable(ref: String) : (Array[Int], Array[String]) = {
    val suffixTable = new Array[String](ref.length)
    val suffixIndex = new Array[Int](ref.length)
    for (i <- 0 until ref.length) {
      suffixTable(i) = ref.substring(i)
      suffixIndex(i) = i
    }
  val indexes = suffixIndex.sortBy { x => suffixTable(x) }
  (indexes, suffixTable.sortBy{x => x})
  }
}
