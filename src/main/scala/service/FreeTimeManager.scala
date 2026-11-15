package service

import model.{FreeTimeError, FreeTimeSlot}
import validation.FreeTimeValidator.{validateFreeTimeSlotResult, validateNoOverlap}

import java.time.{LocalDate, LocalTime}

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

  // freeTimeSlots.find() возвращает Option, который имеет 2 варианта Some() и None. toRight
  // превращает найденный интервал Some() в Right(), а None - в Left() c ошибкой SLOT_NOT_FOUND
  // после чего map при успешной проверке создает список без интервалов с найденной
  // датой и временем начала
  def deleteFreeTimeSlotResult(
                                freeTimeSlots: List[FreeTimeSlot],
                                date: LocalDate,
                                startTime: LocalTime
                              ): Either[FreeTimeError, List[FreeTimeSlot]] = {
    freeTimeSlots.find(slot => slot.date == date && slot.startTime == startTime)
      .toRight(FreeTimeError.SLOT_NOT_FOUND(date, startTime))
      .map(foundSlot =>
        freeTimeSlots.filter(slot =>
          !(slot.date == foundSlot.date && slot.startTime == foundSlot.startTime)
        )
      )
  }
}
