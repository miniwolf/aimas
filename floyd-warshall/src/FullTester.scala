

/**
  * Created by miniwolf on 03-05-2016.
  */
/*object FullTester extends App {
  override def main(args: Array[String]) {
    val listOfFiles = recursiveListFiles(new File("base_project/levels"))
    System.err.close()
    listOfFiles.foreach { case file =>
      println(s"map: ${file.getName}")
      try {
        LearnClient.test(Source.fromFile(file).getLines().toList)
      } catch {
        case re:RuntimeException =>
          println("gave error")
      }
      Node.clear()
      println()
    }
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles.filter(file => !file.getPath.contains("multi_agent"))
    these.filter(!_.isDirectory) ++ these.filter(file => file.isDirectory && !file.getPath.contains("multi_agent")).flatMap(recursiveListFiles)
  }
} */
