package sequencing.util;

object Parser {

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

  def read4(it: Iterator[String]): String = {
    it.next
    val sequence = replaceNtoNt((it.next).toCharArray, 0)
    it.next
    it.next
    return sequence
  }

  def parseFASTQ(pathname: String): scala.collection.mutable.ListBuffer[String] = {
    val source = scala.io.Source.fromFile(pathname)

    val iterator = source getLines

    val listOfSequencies = new scala.collection.mutable.ListBuffer[String]()

    while (iterator hasNext)
      listOfSequencies += read4(iterator)

    return listOfSequencies

  }

  def parse(pathname: String): String = {

    val source = scala.io.Source.fromFile(pathname)

    val iterator = source getLines

    iterator next

    new String(replaceNtoNt((iterator mkString).toCharArray, 0))
  }

}
