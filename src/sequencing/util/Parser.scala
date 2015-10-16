package sequencing.util;

object Parser {

  def read4(it: Iterator[String]): String = {
    it.next
    val sequence = Util.replaceNtoNt((it.next).toCharArray, 0)
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

    new String(Util.replaceNtoNt((iterator mkString).toCharArray, 0))
  }

}
