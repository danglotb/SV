package sequencing.main

object OptionsAlignment {

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
        case "-k" :: k :: tail           => options(opt ++ Map("k" -> k), tail)
        case "-f" :: ref :: read :: tail => options((opt ++ Map("ref" -> ref)) ++ Map("read" -> read), tail)
        case "-r" :: ratio :: tail       => options(opt ++ Map("ratio" -> ratio), tail)
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
    print("\t-k <IntValue> to specify the numbers of errors(3 if not)\n")
    print("\t-file <pathToRefFile> <pathToReadFile> to specify files of gen you align.\n")
    print("\t-r <FloatValue> to specify the ratio of errors accepted (weigthed by the score (0.5 by default)).\n")
    print("\t-h to print this help.\n")
    System.exit(1)
  }
}