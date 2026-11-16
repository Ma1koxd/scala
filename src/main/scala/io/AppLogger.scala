package io

import model.StorageError

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.LocalDateTime
import scala.util.control.NonFatal

object AppLogger {
  // дописываем сообщение в лог вместе с текущими датой и временем
  // возвращаем Right(()) после записи и закрытия файла, при ошибке — Left
  def logResult(fileName: String, message: String): Either[StorageError, Unit] = {
    try {
      val writer = Files.newBufferedWriter(
        Path.of(fileName),
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )

      try {
        writer.write("[" + LocalDateTime.now() + "] " + message)
        writer.newLine()
      } finally {
        writer.close()
      }

      Right(())
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.FILE_WRITE_FAILED(fileName, exception.toString))
    }
  }
}