package sequencing.util

import sequencing.main.BuildRandomGen

object ParserFASTQ {
  
  def replaceNtoNt(seq : Array[Char], cursor : Int) : String = {
    if (cursor == seq.length)
      new String(seq)
    else if (seq(cursor) == 'N') {
      val r = new java.util.Random
      val nextChar = BuildRandomGen.IntToC(r.nextInt(4))
      seq(cursor) = nextChar.charAt(0)
      replaceNtoNt( seq, cursor + 1)
    } else
      replaceNtoNt(seq, cursor + 1)
  }
  
  
  def read4(it : Iterator[String]) : String = {
    it.next
    val sequence = replaceNtoNt((it.next).toCharArray(),0)
    it.next
    it.next
    return sequence
  }
  
  def parse(pathname : String) : scala.collection.mutable.ListBuffer[String] = {
    val source = scala.io.Source.fromFile(pathname)
    
    val iterator = source getLines
    
    val listOfSequencies = new scala.collection.mutable.ListBuffer[String]()
    
    while (iterator hasNext)
      listOfSequencies += ParserFASTQ.read4(iterator)
   
    return listOfSequencies
      
  }
}
