// ADT описывает ошибки пользовательского ввода

package model

enum InputError {
  case INVALID_EDIT_CHOICE(choice: Int)
  case INVALID_INTEGER(input: String)
}