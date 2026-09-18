import java.time.LocalDate

case class Task(id: Int,
                name: String,
                description: String,
                totalHours: Int,
                completedHours: Int,
                startDate: LocalDate,
                deadline: LocalDate,
                priority: Priority,
                canSplit: Boolean,
                status: TaskStatus,
               )