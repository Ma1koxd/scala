import FreeTimeValidator.{hasOverlap, validateFreeTimeSlot}
import InputUtils.{readDate, readTime}

object FreeTimeManager {
  def createFreeTimeSlot(): FreeTimeSlot = {
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

  def addFreeTimeSlot(freeTimeSlots: List[FreeTimeSlot]): List[FreeTimeSlot] = {
    val slot = createFreeTimeSlot()

    if (!validateFreeTimeSlot(slot)){
      freeTimeSlots
    } else if (hasOverlap(slot, freeTimeSlots)) {
      println("Ошибка: свободное время пересекается с уже существующим промежутком.")
      freeTimeSlots
    } else {
      val updatedFreeTimeSlots = freeTimeSlots :+ slot
      println("Свободное время добавлено успешно.")
      updatedFreeTimeSlots
    }
  }

  def deleteFreeTimeSlot(freeTimeSlots: List[FreeTimeSlot]): List[FreeTimeSlot] = {
    if (freeTimeSlots.isEmpty){
      println("Свободное время не задано.")
      freeTimeSlots
    } else {
      println("Введите дату свободного времени для удаления (ГГГГ-ММ-ДД): ")
      val date = readDate()

      println("Введите время начала удаляемого промежутка (ЧЧ:ММ): ")
      val startTime = readTime()

      val oldSize = freeTimeSlots.length

      val updatedFreeTimeSlots = freeTimeSlots.filter(slot => !(slot.date ==
        date && slot.startTime == startTime))

      if (updatedFreeTimeSlots.length < oldSize){
        println("Свободное время успешно удалено.")
      } else {
        println("Такой промежуток свободного времени не найден.")
      }

      updatedFreeTimeSlots
    }
  }
}
