package console

import model.FreeTimeSlot

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
    // map создаёт список продолжительностей интервалов в минутах
    // sum складывает эти значения; для пустого списка результат - 0L
    val totalMinutes = freeTimeSlots.map(slot => slot.durationMinutes).sum

    println("Общее свободное время: " + totalMinutes + " минут")
  }
}