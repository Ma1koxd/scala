// ADT описывает причины ошибок при работе с файлами
// details хранит пояснение ошибки

package model

enum StorageError {
  case FILE_READ_FAILED(fileName: String, details: String)
  case INVALID_STATE_FORMAT(fileName: String, details: String)
  case INVALID_CONFIG_FORMAT(fileName: String, details: String)
  case FILE_WRITE_FAILED(fileName: String, details: String)
  case DIRECTORY_CREATION_FAILED(directoryName: String, details: String)
}