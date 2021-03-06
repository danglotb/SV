package sequencing.seeding

import scala.collection.mutable.ListBuffer

import sequencing.util.Util
import sequencing.alignment._

/**
 * @author danglot
 */
class BurrowsWheelerTransform(ref: String, SAMPLE: Int) {

  val burrowsWheeler: Array[Char] = new Array[Char](ref.length())
  
  print("Compute Suffix Table... ")
  val time = System.currentTimeMillis()
  val suffixTable: Array[Int] = SuffixTable.buildSuffixTable(ref)
  println(System.currentTimeMillis()-time + " ms")
  
  val c: Array[Int] = new Array[Int](5)
  val ranks = new Array[Array[Int]]((ref.length() / SAMPLE) + 1)

  print("Initialisation BWT... ")
  init()
  
   def charToInt(l : Char): Int = {
    l.toUpper match {
      case '$' => 4
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
    }
  }

  def init(): Unit = {

    val tmpRank = new Array[Int](5)
    var currentLetter = '$'

    for (i <- 0 until ranks.length)
      ranks(i) = new Array[Int](5)

    val time = System.currentTimeMillis()
      
    for (i <- 0 until ref.length) {
      burrowsWheeler(i) = if ((suffixTable(i) - 1) >= 0) ref.charAt((suffixTable(i) - 1)) else ref.charAt(ref.length() - 1)

      if (ref.charAt(suffixTable(i)) != currentLetter) {
        currentLetter = ref.charAt(suffixTable(i))
        c(charToInt(currentLetter)) = i - 1
      }

      tmpRank(charToInt(burrowsWheeler(i))) += 1

      if (i % SAMPLE == 0) {
        for (z <- 0 until 5)
          ranks(i / SAMPLE)(z) = tmpRank(z)
      }
    }
    
    println(System.currentTimeMillis() - time + " ms")
    
  }

  /**
   * Function rank to determine how many t the BWT have before the i letter
   */
  def rank(t: Char, i: Int, rankOfT: Int): Int = {
    if (i % SAMPLE == 0)
      return rankOfT + ranks(i / SAMPLE)(charToInt(t))
    else {
      if (burrowsWheeler(i).toUpper == t.toUpper)
        rank(t, i - 1, rankOfT + 1)
      else
        rank(t, i - 1, rankOfT)
    }
  }

  /**
   * Function search of the pattern with z error see BWA
   */
  def search(w: String, i: Int, z: Int, k: Int, l: Int): ListBuffer[Int] = {
    if (z < 0)
      return new ListBuffer[Int]()
    if (i < 0) {
      val ret: ListBuffer[Int] = new ListBuffer[Int]()
      for (x <- k to l)
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
        if (w.charAt(i).toUpper == letters(j).toUpper)
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
      for (x <- k to l) {
        ret += suffixTable(x)
      }
      return ret
    }
    val nk: Int = rMin(w(i), k)
    val nl: Int = rMax(w(i), l)
    exactSearch(w, i - 1, nk, nl)
  }

  def rMin(t: Char, i: Int): Int = c(charToInt(t)) + rank(t, i - 1, 0) + 1
  def rMax(t: Char, i: Int): Int = c(charToInt(t)) + rank(t, i, 0)

}
