import java.io.{File, PrintWriter}
import upickle.default.{read, writeTo}

object StateManager {
  def saveState(state: AppState, fileName: String): Unit = {
    try {
      val writer = new PrintWriter(new File(fileName))

      try {
        writeTo(state, writer, indent = 2)
        println("Состояние сохранено успешно.")
      } finally {
        writer.close()
      }
    } catch {
      case exception: Exception =>
        println("Ошибка при сохранении состояния: " + exception.getMessage)
    }
  }

  def loadState(fileName: String): Option[AppState] = {
    val file = new File(fileName)

    if (!file.exists()) {
      println("Файл состояния не найден.")
      None
    } else {
      try {
        val state = read[AppState](file)
        println("Состояние успешно загружено.")
        Some(state)
      } catch {
        case exception: Exception =>
          println("Ошибка при загрузке состояния: " + exception.getMessage)
          None
      }
    }
  }
}
