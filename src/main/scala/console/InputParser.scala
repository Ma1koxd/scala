package console

import model.InputError

object InputParser {
  def parseInt(input: String): Either[InputError, Int] = {
    input.toIntOption.toRight(InputError.INVALID_INTEGER(input))
  }

  def parseDouble(input: String): Either[InputError, Double] = {
    input.toDoubleOption.toRight(InputError.INVALID_DOUBLE(input))
  }
}