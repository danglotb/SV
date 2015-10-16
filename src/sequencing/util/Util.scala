package sequencing.util

object Util {
  
  def IntToC(i: Int): Char = {
    i match {
      case 0 => 'A'
      case 1 => 'C'
      case 2 => 'G'
      case 3 => 'T'
    }
  }

  def replaceNtoNt(seq: Array[Char], cursor: Int): String = {
    if (cursor == seq.length)
      return new String(seq)
    else if (seq(cursor) == 'N') {
      val r = new java.util.Random
      seq(cursor) = IntToC(r.nextInt(4))
    }
    replaceNtoNt(seq, cursor + 1)
  }

}