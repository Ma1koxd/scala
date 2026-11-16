package console

import model.InputError

object InputParser {
  def parseInt(input: String): Either[InputError, Int] = {
    input.toIntOption.toRight(InputError.INVALID_INTEGER(input))
  }
}