package model

// Хранит полный результат построения расписания.
case class ScheduleResult(
                           blocks: List[ScheduledBlock],
                           remainingFreeTimeSlots: List[FreeTimeSlot],
                           unplacedTasks: List[Task],
                           updatedTasks: List[Task]
                         )
