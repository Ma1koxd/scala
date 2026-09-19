import java.time.LocalDate

case class Task(id: Int,
                name: String,
                description: String,
                totalHours: Double,
                completedHours: Double,
                startDate: LocalDate,
                deadline: LocalDate,
                priority: Priority,
                canSplit: Boolean,
                minBlockMinutes: Int,
                status: TaskStatus,
               )

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