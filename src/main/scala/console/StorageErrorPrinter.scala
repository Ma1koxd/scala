package console

import model.StorageError

object StorageErrorPrinter {
  def printStorageError(error: StorageError): Unit = {
    error match {
      case StorageError.FILE_READ_FAILED(fileName, details) =>
        println("Ошибка чтения файла " + fileName + ": " + details)
      case StorageError.INVALID_STATE_FORMAT(fileName, details) =>
        println("Некорректный формат файла состояния " + fileName + ": " + details)
      case StorageError.INVALID_CONFIG_FORMAT(fileName, details) =>
        println("Некорректный формат файла конфигурации " + fileName + ": " + details)
      case StorageError.FILE_WRITE_FAILED(fileName, details) =>
        println("Ошибка записи файла " + fileName + ": " + details)
    }
  }
}