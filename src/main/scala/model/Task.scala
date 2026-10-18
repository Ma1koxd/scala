package model

import io.JsonFormats.given
import upickle.default.ReadWriter

import java.time.LocalDate

case class Task (id: Int,
                name: String,
                totalHours: Double,
                completedHours: Double,
                startDate: LocalDate,
                deadline: LocalDate,
                priority: Priority,
                canSplit: Boolean,
                minBlockMinutes: Int,
                status: TaskStatus,
               ) derives ReadWriter {
  def remainingMinutes: Long = {
    math.round((totalHours-completedHours)*60)
  }
}

object Task {
  def getInitialStatus(totalHours: Double, completedHours: Double): TaskStatus = {
    if (completedHours == 0) {
      TaskStatus.PLANNED
    }
    else if (completedHours == totalHours) {
      TaskStatus.COMPLETED
    }
    else {
      TaskStatus.IN_PROGRESS
    }
  }
}