package client.multiagent

import java.io.{BufferedReader, InputStreamReader}
import java.util.function.ToIntFunction

import core.Node
import searchclient.SearchClient

/**
  * Created by miniwolf on 11-05-2016.
  */
object LearnClient extends App {
  override def main(args: Array[String]) {
    val serverMessages: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    System.err.println("SearchClient initializing. I am sending this using the error output stream.")

    val lines: java.util.List[String] = new java.util.ArrayList[String]
    var line: String = serverMessages.readLine
    while ( !line.equals("") ) {
      lines.add(line)
      line = serverMessages.readLine
    }
    Node.MAX_COLUMN = lines.stream().mapToInt(new ToIntFunction[String] {
      override def applyAsInt(value: String): Int = value.length
    }).max.getAsInt
    Node.MAX_ROW = lines.size
    val client: SearchClient = new SearchClient(lines)
  }
}
