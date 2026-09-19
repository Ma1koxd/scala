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