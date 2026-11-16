package io

import model.{AppState, StorageError}
import upickle.default.{read, writeTo}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path}
import scala.util.control.NonFatal // нужна, чтобы пропускать критические сбои

object StateManager {
  def parseStateResult(text: String, fileName: String): Either[StorageError, AppState] = {
    try {
      Right(read[AppState](text))
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.INVALID_STATE_FORMAT(fileName, exception.toString))
    }
  }

  def saveState(state: AppState, fileName: String): Boolean = {
    try {
      val writer = new PrintWriter(new File(fileName))

      try {
        writeTo(state, writer, indent = 2)
        println("Состояние сохранено успешно.")
      } finally {
        writer.close()
      }
      true
    } catch {
      case exception: Exception =>
        println("Ошибка при сохранении состояния: " + exception.getMessage)
        false
    }
  }

  // читаем файл сохранения потом парсируем, если файла нету, то возвращается Right(None),
  // если состояние загрузится, то возвращается Right(Some(state))
  // все ошибки чтения у нас возвращаются через FILE_READ_FAILED, ошибки формата файла
  // через INVALID_STATE_FORMAT с помощью parseStateResult
  def loadStateResult(fileName: String): Either[StorageError, Option[AppState]] = {
    try {
      val text = Files.readString(Path.of(fileName), StandardCharsets.UTF_8)

      parseStateResult(text, fileName).map(state => Some(state))
    } catch {
      // у нас в программе при первом запуске нету еще файла сохранения
      // поэтому мы исключаем этот вариант событий как ошибку
      case _: NoSuchFileException =>
        Right(None)
      // сюда попадают почти все ошибки, кроме фатальных (например, нехватка памяти там)
      case NonFatal(exception) =>
        Left(StorageError.FILE_READ_FAILED(fileName, exception.toString))
    }
  }
}
