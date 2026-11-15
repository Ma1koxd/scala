package console

import model.{InputError, TaskEditAction}

object TaskEditActionParser {
  def parse(choice: Int): Either[InputError, TaskEditAction] = {
    choice match {
      case 1 =>
        Right(TaskEditAction.CHANGE_NAME)
      case 2 =>
        Right(TaskEditAction.CHANGE_TOTAL_HOURS)
      case 3 =>
        Right(TaskEditAction.CHANGE_COMPLETED_HOURS)
      case 4 =>
        Right(TaskEditAction.CHANGE_START_DATE)
      case 5 =>
        Right(TaskEditAction.CHANGE_DEADLINE)
      case 6 =>
        Right(TaskEditAction.CHANGE_PRIORITY)
      case 7 =>
        Right(TaskEditAction.CHANGE_CAN_SPLIT)
      case 8 =>
        Right(TaskEditAction.CHANGE_MIN_BLOCK_MINUTES)
      case 0 =>
        Right(TaskEditAction.FINISH_EDITING)
      case _ =>
        Left(InputError.INVALID_EDIT_CHOICE(choice))
    }
  }
}