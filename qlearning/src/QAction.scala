import searchclient.Command

/**
  * Created by miniwolf on 18-03-2016.
  */
protected case class QAction(command: Command) {
  require(command.actType != null, s"QAction: ${command.toString} must have legal action")

  override def toString: String = s"\nAction: state (${command.toActionString})"
}
