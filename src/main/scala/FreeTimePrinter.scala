object FreeTimePrinter {

  def printFreeTimeSlot(slot: FreeTimeSlot): Unit = {
    println("Дата: " + slot.date)
    println("Начало: " + slot.startTime)
    println("Окончание: " + slot.endTime)
    println("Продолжительность: " + slot.durationMinutes + " минут\n")
  }

  def printFreeTimeSlots(freeTimeSlots: List[FreeTimeSlot]): Unit = {
    if (freeTimeSlots.isEmpty) {
      println("Свободное время не задано.\n")
    } else {
      println("\nСвободное время:\n")

      for (slot <- freeTimeSlots) {
        printFreeTimeSlot(slot)
      }

      printTotalFreeTime(freeTimeSlots)
    }
  }

  def printTotalFreeTime(freeTimeSlots: List[FreeTimeSlot]): Unit = {
    var totalMinutes = 0L

    for (slot <- freeTimeSlots) {
      totalMinutes += slot.durationMinutes
    }

    println("Общее свободное время: " + totalMinutes + " минут")
  }
}