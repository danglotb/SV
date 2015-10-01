package sequencing.seeding

/**
 * @author danglot
 */
class BurrowsWheelerTransform(ref:String){
  
  val SAMPLE : Int = 16

  val burrowsWheeler : Array[Char] = new Array[Char](ref.length())
  val suffixTable : Array[Int] = SuffixTable.buildSuffixTable(ref)
  val c : Array[Int] = new Array[Int](5)
  val ranks = new Array[Array[Int]]( (ref.length()/ SAMPLE) + 1)
  
  init()
  
  def cToI(l : Char) : Int = {
    l.toUpper match {
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
      case '$' => 4
    }
  }
  
  def init() : Unit = {
    
    val tmpRank = new Array[Int](4)
    var currentLetter = '$'
    var cptSample = 0
    
    for (i <- 0 until (ref.length()/ SAMPLE) + 1)
      ranks(i) = new Array[Int](4)
      
    for (i <- 0 until burrowsWheeler.length) {
      burrowsWheeler(i) =  if( (suffixTable(i)-1) >= 0) ref.charAt((suffixTable(i)-1)) else '$'
      
      if (ref.charAt(suffixTable(i)) != currentLetter) {
        currentLetter = ref.charAt(suffixTable(i))
        c(cToI(currentLetter)) = i
      }
      
      if (burrowsWheeler(i) != '$') {
        tmpRank(cToI(burrowsWheeler(i))) += 1
        if (i % SAMPLE == 0) {
          ranks(cptSample)(cToI(burrowsWheeler(i))) = tmpRank(cToI(burrowsWheeler(i)))
          cptSample += 1
        }
      }
      
    }
   
    val l : List[(Int, Int)] = List((1,1))
    
   println(search("ggga", "ggga".length-1, 1, 1, ref.length()-1, l))
   
  }
  
  override def toString() : String = {
    var str = ""
    burrowsWheeler.foreach {x => str += x}
    str += "\n"
    c.foreach {x => str += x + "\n"}
    ranks.foreach { x => 
      x.foreach { r =>
        str += r + "\t"
      }
      str += "\n"
    }
    str
  }
  
  def rank(t : Char, i : Int) : Int = {
    var current = i
    var rank = 0
    while(current % SAMPLE != 0) {
      if (burrowsWheeler(current) == t)
        rank += 1
      current -= 1
    }
    rank+ranks(current%SAMPLE)(cToI(t))  
  }

  def search(w : String, i : Int, z : Int, k : Int, l : Int, indexes : List[(Int, Int)]) : List[(Int, Int)] = {
    if (z < 0)
     return indexes
    if (i < 0) 
     return ((k,l) :: indexes) 
   
    search(w, i-1, z-1, k, l, indexes)
    val letters : List[Char] = List('A', 'C', 'G', 'T')
    for (i <- 0 until letters.length) {
      val nk : Int = rMin(letters(i), k)
      val nl : Int = rMax(letters(i), l)
      if (k <= l) {
         search(w, i-1, z-1, nk, nl, indexes)
        if (w.charAt(i) == letters(i))
         search(w, i-1, z, nk, nl, indexes)
        else
         search(w, i-1, z-1, k, l, indexes)
      }
    }
    indexes
  }
  
  def rMin(t : Char, i : Int) : Int = c(cToI(t)) + rank(t, i-1) + 1
  def rMax(t : Char, i : Int) : Int = c(cToI(t)) + rank(t, i)
  
}

object Main extends App {
  val b = new BurrowsWheelerTransform("tgggatggatcaaccctaacagtggtggcacaaactatgcacagaagtttcagggcagggtcaccatgaccagggacacgtccatcagcacagcctacatggagctgagcaggctgagatctgacgacacggccgtgtattactgtgcgagaga$")
}