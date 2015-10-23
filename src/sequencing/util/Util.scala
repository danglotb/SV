package sequencing.util

object Util {
  
  def IntToChar(i: Int): Char = {
    i match {
      case 0 => 'A'
      case 1 => 'C'
      case 2 => 'G'
      case 3 => 'T'
      case 4 => '$'
    }
  }

  def replaceNtoNt(seq: Array[Char], cursor: Int): String = {
    if (cursor == seq.length) 
      return new String(seq)
    else if (seq(cursor) == 'N') 
      seq(cursor) = IntToChar( (new java.util.Random()).nextInt(4))
    replaceNtoNt(seq, cursor + 1)
  }

  def toTuple(tuple : Any) : (Int,Int,Int) = {
    tuple match {
      case (a : Int, b: Int, c: Int) => (a,b,c)
      case _ => (5,-4,-10)
    }
  }
  
}