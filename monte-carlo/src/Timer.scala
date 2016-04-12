/**
  * Created by miniwolf on 22-03-2016.
  */
class Timer {
  var time = 0l
  def start(): Unit = time = System.currentTimeMillis()

  def stop(): Unit = time = 0l

  def checkDuration(maxTime: Long): Boolean = {
    if ( time == 0l ) {
      System.err.println("Clock has not been started yet")
      false
    } else {
      val timeDiff = System.currentTimeMillis() - time
      timeDiff > maxTime
    }
  }
}
