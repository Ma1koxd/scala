package service

import console.InputUtils.{readDate, readTime}
import model.{FreeTimeError, FreeTimeSlot}
import validation.FreeTimeValidator.{validateFreeTimeSlotResult, validateNoOverlap}

object FreeTimeManager {
  // проверяем границы времени -> если все хорошо передаем интервал времени
  // в validateNoOverlap, который проверяет пересечения, если проверка провалилась, то дальше
  // передается ошибка. после 2 успешных проверок создается новый список свободного времени
  // flatmap здесь я использовал потому, что validateNoOverlap у нас уже выдает Either,
  // так что применение map вылилось бы в вложенный тип Either[что-то, Either],
  // а использовав flatmap мы сохраняем проверки в один Either
  def addFreeTimeSlotResult(freeTimeSlots: List[FreeTimeSlot], slot: FreeTimeSlot): Either[FreeTimeError, List[FreeTimeSlot]] = {
    validateFreeTimeSlotResult(slot)
      .flatMap(validSlot => validateNoOverlap(validSlot, freeTimeSlots))
      .map(validSlot => freeTimeSlots :+ validSlot)
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
