package io

import model.{AppConfig, StorageError}
import upickle.default.read

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path}
import scala.util.control.NonFatal

object ConfigManager {
  val defaultConfig: AppConfig = AppConfig(
    logFile = "planner.log",
    stateFile = "planner-state.json",
    reportDirectory = "reports"
  )

  // преобразуем содержимое файла в конфигурацию
  // если разобрать содержимое не получилось, возвращаем ошибку формата
  def parseConfigResult(text: String, fileName: String): Either[StorageError, AppConfig] = {
    try {
      // Option превращает null от библиотеки uPickle в None
      // toRight возвращает полученную конфигурацию или ошибку формата для None
      Option(read[AppConfig](text))
        .toRight(StorageError.INVALID_CONFIG_FORMAT(
          fileName,
          "Ожидался объект конфигурации, получено null"
        ))
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.INVALID_CONFIG_FORMAT(fileName, exception.toString))
    }
  }

  // читаем конфиг, при отсутствии файла возвращаем Right(None)
  // при успешной загрузке возвращаем Right(Some(config)), при ошибке - Left
  def loadConfigResult(fileName: String): Either[StorageError, Option[AppConfig]] = {
    try {
      val text = Files.readString(Path.of(fileName), StandardCharsets.UTF_8)

      parseConfigResult(text, fileName).map(config => Some(config))
    } catch {
      case _: NoSuchFileException =>
        Right(None)
      case NonFatal(exception) =>
        Left(StorageError.FILE_READ_FAILED(fileName, exception.toString))
    }
  }
}