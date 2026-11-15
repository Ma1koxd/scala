package console

import console.InputUtils.{readDate, readTime}
import model.FreeTimeSlot

object FreeTimeInput {
  def readFreeTimeSlot(): FreeTimeSlot = {
    println("Введите дату свободного времени (ГГГГ-ММ-ДД):")
    val date = readDate()

    println("Введите время начала (ЧЧ:ММ):")
    val startTime = readTime()

    println("Введите время окончания (ЧЧ:ММ):")
    val endTime = readTime()

    FreeTimeSlot(
      date,
      startTime,
      endTime
    )
  }
}