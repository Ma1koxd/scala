package io

import java.io.{FileWriter, PrintWriter}
import java.time.LocalDateTime

object AppLogger {
  def log(fileName: String, message: String): Unit = {
    try {
      val writer = new PrintWriter(
        new FileWriter(fileName, true)
      )

      try {
        writer.println("[" + LocalDateTime.now() + "] " + message)
      } finally {
        writer.close()
      }
    } catch {
      case exception: Exception =>
        println("Ошибка при записи лога: " + exception.getMessage)
    }
  }
}