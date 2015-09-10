package sequencing.util;

object Parser {
  
  def usage() : Unit = {
     print("This applications need at least this two options : \n")
     print("\t-g <pathToGenomeFile> to specify the path the genome file.\n")
     print("\t-r <pathToReadFile> to specify the path the read file.\n")
   }
  
 def parseFasta(pathname : String) : String = {
  val source = scala.io.Source.fromFile(pathname.toString)
  
  val iterator = source getLines
  
  //Trash the first line : isn't used
  (iterator next)
  
  var read : String = "" 
  
  for (i <- 0 until 1) 
    read += (iterator next)
    
  read
 }
 
 def parse(pathname : String) : String = {
     val source = scala.io.Source.fromFile(pathname.toString)
     val iterator = source getLines
     var read : String = "" 
      while (iterator hasNext)
      read += (iterator next)
  read
 }
  
 def parseFile(pathname : Any) : String = {
   if (pathname == null) {
     usage()
     System.exit(1)
   }
   if (pathname.toString.endsWith(".fasta"))
     return parseFasta(pathname.toString())
   else
     return parse(pathname.toString())
 }
}
