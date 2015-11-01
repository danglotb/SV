package sequencing.main

object OptionsAlignment {
  
  def toTuple(tuple : Any) : (Int,Int,Int) = {
    tuple match {
      case (a : Int, b: Int, c: Int) => (a,b,c)
      case _ => (5,-4,-10)
    }
  }

  type Options = Map[String, Any]

  /**
   * method to parse and build a map of the options
   */
  def options(opt: Options, args: List[String]): Options = {
    if (args.isEmpty) opt
    else {
      args match {
        case "-sc" :: matchScore :: mismacthScore :: indelScore :: tail =>
          options(opt ++ Map("score" -> (matchScore, mismacthScore, indelScore)), tail)
        case "-err" :: err :: tail       => options(opt ++ Map("error" -> err), tail)
        case "-f" :: ref :: read :: tail => options((opt ++ Map("ref" -> ref)) ++ Map("read" -> read), tail)
        case "-r" :: ratio :: tail       => options(opt ++ Map("ratio" -> ratio), tail)
        case "-sseed" :: sseed :: tail   => options(opt ++ Map("sseed" -> sseed), tail)
        case "-p" :: tail                => options(opt ++ Map("print" -> true), tail)
        case "-h" :: tail                => usage(); opt
        case _                           => usage(); opt
      }
    }
  }

  /**
   * print in stdout available option
   */
  def usage(): Unit = {
    print("Options availables : \n")
    print("\t-sc <matchScore> <mismatchScore> <indelScore> to specify the score of a match( (5, -4, -10) if not)\n")
    print("\t-file <pathToRefFile> <pathToReadFile> to specify files of gen you align.\n")
    print("\t-r <FloatValue> to specify the ratio of errors accepted (weigthed by the score (0.5 by default)).\n")
    print("\t-err <IntValue> to specify the number of errors in the seed researching(0 by default).\n")
    print("\t-sseed <IntValue> to specify the size (in number of nt) of seeds.\n")
    print("\t-p to print alignment.\n")
    print("\t-h to print this help.\n")
    System.exit(1)
  }
}