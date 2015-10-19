package sequencing.main

/**
 * @author danglot
 */
object BuildRandomGen extends App {

  val pathname = "input/read"

  val number = 1000
  
  val writer = new java.io.FileWriter(pathname, true)
  
  var i : Int = 0
  
  val r = new java.util.Random()
  
  while (i != number) {
   writer.write(sequencing.util.Util.IntToC(r.nextInt(4)))
   i += 1
   if (i % 1000 == 0)
     writer.write("\n")
  }
  
  writer.close
  
  
  
}