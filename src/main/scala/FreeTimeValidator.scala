object FreeTimeValidator {
  def validateFreeTimeSlot(slot: FreeTimeSlot): Boolean = {
    if (!slot.endTime.isAfter(slot.startTime)){
      println("Ошибка: время окончания должно быть позже времени начала.")
      false
    } else {
      true
    }
  }

  def hasOverlap(slot: FreeTimeSlot, freeTimeSlots: List[FreeTimeSlot]): Boolean = {
    freeTimeSlots.exists(existingSlot => existingSlot.date == slot.date &&
    slot.startTime.isBefore(existingSlot.endTime) &&
    slot.endTime.isAfter(existingSlot.startTime))
  }
}
