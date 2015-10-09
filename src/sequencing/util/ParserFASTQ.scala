package sequencing.util

object ParserFASTQ {
  
  def read4(it : Iterator[String]) : String = {
    it.next
    val sequence = it.next
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
