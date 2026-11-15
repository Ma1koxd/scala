package validation

import model.{FreeTimeError, FreeTimeSlot}

object FreeTimeValidator {
  // эти 2 метода в связи с использованием Either переделываем также, как и валидатор задач
  def validateFreeTimeSlotResult(slot: FreeTimeSlot): Either[FreeTimeError, FreeTimeSlot] = {
    if (!slot.endTime.isAfter(slot.startTime)) {
      Left(FreeTimeError.INVALID_TIME_RANGE)
    } else {
      Right(slot)
    }
  }

  def validateNoOverlap(slot: FreeTimeSlot, freeTimeSlots: List[FreeTimeSlot]): Either[FreeTimeError, FreeTimeSlot] = {
    if (hasOverlap(slot, freeTimeSlots)) {
      Left(FreeTimeError.OVERLAPPING_SLOT)
    } else {
      Right(slot)
    }
  }

  def hasOverlap(slot: FreeTimeSlot, freeTimeSlots: List[FreeTimeSlot]): Boolean = {
    freeTimeSlots.exists(existingSlot => existingSlot.date == slot.date &&
    slot.startTime.isBefore(existingSlot.endTime) &&
    slot.endTime.isAfter(existingSlot.startTime))
  }
}
