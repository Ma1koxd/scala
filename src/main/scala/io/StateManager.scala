package io

import model.{AppState, StorageError}
import upickle.default.{read, writeTo}

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

  // записываем состояние в файл сохранения
  // возвращаем Right(()) после успешной записи и закрытия файла
  // при ошибке возвращается Left с FILE_WRITE_FAILED
  def saveStateResult(state: AppState, fileName: String): Either[StorageError, Unit] = {
    try {
      val writer = Files.newBufferedWriter(Path.of(fileName), StandardCharsets.UTF_8)

      try {
        writeTo(state, writer, indent = 2)
      } finally {
        writer.close()
      }

      Right(())
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.FILE_WRITE_FAILED(fileName, exception.toString))
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
