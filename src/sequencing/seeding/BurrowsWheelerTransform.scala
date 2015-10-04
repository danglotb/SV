package sequencing.seeding

import scala.collection.mutable.ListBuffer

import sequencing.alignment._

/**
 * @author danglot
 */
class BurrowsWheelerTransform(ref: String) {

  val SAMPLE: Int = 16

  val burrowsWheeler: Array[Char] = new Array[Char](ref.length())
  val suffixTable: Array[Int] = SuffixTable.buildSuffixTable(ref)
  val c: Array[Int] = new Array[Int](5)
  val ranks = new Array[Array[Int]]((ref.length() / SAMPLE) + 2)

  init()

  def cToI(l: Char): Int = {
    l.toUpper match {
      case '$' => 4
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
    }
  }

  def init(): Unit = {

    val tmpRank = new Array[Int](4)
    var currentLetter = '$'

    for (i <- 0 until ranks.length)
      ranks(i) = new Array[Int](4)

    for (i <- 0 until burrowsWheeler.length) {
      burrowsWheeler(i) = if ((suffixTable(i) - 1) >= 0) ref.charAt((suffixTable(i) - 1)) else '$'

      if (ref.charAt(suffixTable(i)) != currentLetter) {
        currentLetter = ref.charAt(suffixTable(i))
        c(cToI(currentLetter)) = i
      }

      if (burrowsWheeler(i) != '$') {
        tmpRank(cToI(burrowsWheeler(i))) += 1
        if (i % SAMPLE == 0) {
          for (z <- 0 until 4)
            ranks(i / SAMPLE)(z) = tmpRank(z)
        }
      }
    }

    for (z <- 0 until 4)
      ranks(ranks.length - 1)(z) = tmpRank(z)
  }

  override def toString(): String = {
    var str = ""
    burrowsWheeler.foreach { x => str += x }
    str += "\n"
    c.foreach { x => str += x + "\n" }
    ranks.foreach { x =>
      x.foreach { r =>
        str += r + "\t"
      }
      str += "\n"
    }
    str
  }

  /**
   * Function rank to determine how many t the BWT have before the i letter
   * /!\ Something wrong with rank init at -1
   */
  def rank(t: Char, i: Int): Int = {
    var rank = 0
    var current = i
    if (i % SAMPLE != 0) {
      current -= 1
      while (current % SAMPLE != 0) {
        if (burrowsWheeler(current).toUpper == t.toUpper)
          rank += 1
        current -= 1
      }
    }
    rank + ranks(current / SAMPLE)(cToI(t))
  }

  /**
   * Function search of the pattern with z error see BWA
   */
  def search(w: String, i: Int, z: Int, k: Int, l: Int): ListBuffer[Int] = {
    if (z < 0)
      return new ListBuffer[Int]()
    if (i < 0) {
      val ret: ListBuffer[Int] = new ListBuffer[Int]()
      for (x <- k until l + 1)
        ret += suffixTable(x)
      return ret
    }
    val ret = search(w, i - 1, z - 1, k, l) //indel
    val letters: List[Char] = List('a', 'c', 'g', 't')
    for (j <- 0 until letters.length) {
      val nk: Int = rMin(letters(j), k)
      val nl: Int = rMax(letters(j), l)
      if (nk <= nl) {
        ret ++= search(w, i, z - 1, nk, nl) //indel
        if (w.charAt(i) == letters(j))
          ret ++= search(w, i - 1, z, nk, nl) //match
        else
          ret ++= search(w, i - 1, z - 1, nk, nl) //mismatch
      }
    }
    ret.distinct
  }

  /**
   * Function exactSearch
   */
  def exactSearch(w: String, i: Int, k: Int, l: Int): ListBuffer[Int] = {
    if (i < 0) {
      val ret: ListBuffer[Int] = new ListBuffer[Int]()
      for (x <- k until l+1) {
        print(x+":")
        print(suffixTable(x)+":")
        println(ref.substring(suffixTable(x)))
        ret += suffixTable(x)
      }
      return ret
    }
    val nk: Int = rMin(w(i), k)
    val nl: Int = rMax(w(i), l)
    exactSearch(w, i-1, nk, nl)
  }

  def rMin(t: Char, i: Int): Int = c(cToI(t)) + rank(t, i - 1) + 1
  def rMax(t: Char, i: Int): Int = c(cToI(t)) + rank(t, i)

}

object Main extends App {
  val gen = "tgggatggatcaaccctaacagtggtggcacaaactatgcacagaagtttcagggcagggtcaccatgaccagggacacgtccatcagcacagcctacatggagctgagcaggctgagatctgacgacacggccgtgtattactgtgcgagaga$"
  val b = new BurrowsWheelerTransform(gen)
//  println(b)
  val str = "ggga"
  val s = b.exactSearch(str, str.length()-1, 1, gen.length()-1)
  s.foreach { x =>
    println(gen.substring(x))
  }
}