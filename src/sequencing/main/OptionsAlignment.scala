package sequencing.main

object OptionsAlignment {
  
  type Options = Map[String, Any]

  /**
   * method to parse and build a map of the options
   */
  def options(opt: Options, args: List[String]): Options = {
    if (args.isEmpty) return opt
    args match {
      case "-sc" :: matchScore :: mismacthScore :: indelScore :: tail =>
        options(opt++Map("score" -> (matchScore,mismacthScore,indelScore )), tail)
      case "-k" :: k :: tail => options(opt++Map("k" -> k), tail)
      case "-f" :: ref :: read :: tail => options( (opt++Map("ref" -> ref))++Map("read" -> read), tail)
      case "-h" :: tail => usage() ; opt
      case _ => usage() ; opt
     }
   }

   /**
    * print in stdout available option
    */
   def usage() : Unit = {
     print("Options availables : \n")
     print("\t-sc <matchScore> <mismatchScore> <indelScore> to specify the score of a match( (5, -4, -10) if not)\n")
     print("\t-k <IntValue> to specify the numbers of errors(3 if not)\n")
     print("\t-file <pathToRefFile> <pathToReadFile> to specify files of gen you align.\n")
     System.exit(1)
   }
}