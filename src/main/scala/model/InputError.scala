// ADT описывает ошибки пользовательского ввода

package model

enum InputError {
  case INVALID_EDIT_CHOICE(choice: Int)
  case INVALID_INTEGER(input: String)
  case INVALID_DOUBLE(input: String)
  case INVALID_DATE(input: String)
  case INVALID_TIME(input: String)
}