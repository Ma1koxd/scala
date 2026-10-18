import java.io.File
import upickle.default.read

object ConfigManager {
  val defaultConfig: AppConfig = AppConfig(
    logFile = "planner.log",
    stateFile = "planner-state.json",
    reportDirectory = "reports"
  )

  def loadConfig(fileName: String, fallbackConfig: AppConfig = defaultConfig): AppConfig = {
    val file = new File(fileName)

    if (!file.exists()) {
      println("Файл конфигурации не найден. Используются настройки по умолчанию.")
      fallbackConfig
    } else {
      try {
        val config = read[AppConfig](file)
        println("Конфигурация успешно загружена.")
        config
      } catch {
        case exception: Exception =>
          println("Ошибка при загрузке конфигурации: " + exception.getMessage)
          println("Используется последняя/изначальная корректная конфигурация.")
          fallbackConfig
      }
    }
  }
}