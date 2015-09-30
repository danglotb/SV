package sequencing.seeding

/**
 * @author danglot
 */
class BurrowsWheelerTransform(ref:String){

  val burrowsWheeler : Array[Char] = new Array[Char](ref.length()+1)
  val suffixTable : (Array[Int], Array[String]) = SuffixTable.buildSuffixTable(ref+"$")
  val c : scala.collection.mutable.HashMap[Char,Int] = new scala.collection.mutable.HashMap[Char,Int]()
  val ranks = new Array[Array[Int]]((ref.length()+1/16))
  
  init()
  
  implicit def convert(l : Char) : Int = {
    l.toUpper match {
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
    }
  }
  
  def init() : Unit = {
    
    val tmpRank = new Array[Int](4)
    
    for (i <- 0 until (ref.length()+1/16))
      ranks(i) = new Array[Int](4)
    
    for (i <- 0 until suffixTable._1.length) {
      burrowsWheeler(i) =  if( (suffixTable._1(i)-1) >= 0) ref.charAt((suffixTable._1(i)-1)) else '$'
      
      tmpRank(burrowsWheeler(i)) += 1
      
      if (i % 16 == 0)
        ranks(i)(burrowsWheeler(i)) = tmpRank(burrowsWheeler(i))
        
    }
    c += ('$' -> 0)
    c += ('A' -> 1)
    var cpt : Int = 1
    while((suffixTable._2(cpt).charAt(0)).toUpper == 'A')
      cpt += 1
    c += ('C' -> cpt)
    while((suffixTable._2(cpt).charAt(0)).toUpper == 'C')
      cpt += 1
    c += ('G' -> cpt)
    while((suffixTable._2(cpt).charAt(0)).toUpper == 'G')
      cpt += 1
    c += ('T' -> cpt)
  }
  
  def rank(t : Char, i : Int) : Int = {
    0
  }
  
}

object Main extends App {
  val b = new BurrowsWheelerTransform("tgggatggatcaaccctaacagtggtggcacaaactatgcacagaagtttcagggcagggtcaccatgaccagggacacgtccatcagcacagcctacatggagctgagcaggctgagatctgacgacacggccgtgtattactgtgcgagaga")
}