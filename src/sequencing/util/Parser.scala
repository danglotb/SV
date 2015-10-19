package sequencing.util;

import sequencing.util._

class Parser(pathname : Any, sizeOfChunk : Int = 5) {

  if (pathname == null) {
     usage()
     System.exit(1)
  }
  
  val source = scala.io.Source.fromFile(pathname.toString)
  
  val iterator : Iterator[String] = source getLines
  
  if (pathname.toString.endsWith(".fasta") || pathname.toString.endsWith(".fna") || pathname.toString.endsWith(".fa"))
   (iterator next)
   
  def usage() : Unit = {
     print("This applications need at least this two options : \n")
     print("\t-g <pathToGenomeFile> to specify the path the genome file.\n")
     print("\t-r <pathToReadFile> to specify the path the read file.\n")
   }
  
  /**
   * return a part of the file
   */
//  def parse() : String = {
//    var str : String = ""
//    for (i <- 0 until sizeOfChunk) {
//      if (iterator hasNext)
//        str += (iterator next)
//    }
//    return str
//  }
  
  def parse() : String = {
    return ParserFASTQ.replaceNtoNt( (iterator mkString).toCharArray , 0)
  }
  
  /**
   * use in case of read
   */
  def parseAll() : String = {
    var str : String = ""
    while(iterator hasNext) {
      str += (ParserFASTQ.replaceNtoNt((iterator next).toCharArray,0)).toUpperCase
    }
    return str
  }
  
  def hasNext() : Boolean = iterator hasNext
  
}
