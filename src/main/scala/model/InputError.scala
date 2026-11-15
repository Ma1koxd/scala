// вспомогательный файл для парсера выбора действия в изменении дела о том, что ввели
// некорректное число

package model

enum InputError {
  case INVALID_EDIT_CHOICE(choice: Int)
}