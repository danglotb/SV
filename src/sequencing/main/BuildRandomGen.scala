package sequencing.main

/**
 * @author danglot
 */
object BuildRandomGen extends App {
  
  def IntToC(i : Int) : String = {
    i match {
      case 0 => "A"
      case 1 => "C"
      case 2 => "G"
      case 3 => "T"
    }
  }
  
  val pathname = "input/read"

  val number = 1000
  
  val writer = new java.io.FileWriter(pathname, true)
  
  var i : Int = 0
  
  val r = new java.util.Random()
  
  while (i != number) {
   writer.write(IntToC(r.nextInt(4)))
   i += 1
   if (i % 1000 == 0)
     writer.write("\n")
  }
  
  writer.close
  
  
  
}