object FreeTimePrinter {

  def printFreeTimeSlot(slot: FreeTimeSlot): Unit = {
    println("Дата: " + slot.date)
    println("Начало: " + slot.startTime)
    println("Окончание: " + slot.endTime)
    println()
  }

  def printFreeTimeSlots(freeTimeSlots: List[FreeTimeSlot]): Unit = {
    if (freeTimeSlots.isEmpty) {
      println("Свободное время не задано.\n")
    }
    else {
      println("\nСвободное время:\n")

      for (slot <- freeTimeSlots) {
        printFreeTimeSlot(slot)
      }
    }
  }
}