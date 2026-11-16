package console

import console.InputUtils.{readDateOption, readTimeOption}
import model.FreeTimeSlot

object FreeTimeInput {
  def readFreeTimeSlot(): Option[FreeTimeSlot] = {
    println("Введите дату свободного времени (ГГГГ-ММ-ДД):")

    readDateOption().flatMap(date => {
      println("Введите время начала (ЧЧ:ММ):")

      readTimeOption().flatMap(startTime => {
        println("Введите время окончания (ЧЧ:ММ):")

        readTimeOption().map(endTime =>
          FreeTimeSlot(date, startTime, endTime)
        )
      })
    })
  }
}