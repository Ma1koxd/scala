object Scheduler {
  def canFitWholeTask(task: Task, slot: FreeTimeSlot): Boolean = {
    task.remainingMinutes <= slot.durationMinutes
  }

  def isSlotWithinTaskDates(task: Task, slot: FreeTimeSlot): Boolean = {
    !slot.date.isBefore(task.startDate) && !slot.date.isAfter(task.deadline)
  }
}
