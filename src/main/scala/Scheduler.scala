import java.time.LocalDate

object Scheduler {
  def canFitWholeTask(task: Task, slot: FreeTimeSlot): Boolean = {
    task.remainingMinutes <= slot.durationMinutes
  }

  def isSlotWithinTaskDates(task: Task, slot: FreeTimeSlot): Boolean = {
    !slot.date.isBefore(task.startDate) && !slot.date.isAfter(task.deadline)
  }

  def canPlaceWholeTask(task: Task, slot: FreeTimeSlot): Boolean = {
    isSlotWithinTaskDates(task,slot) && canFitWholeTask(task,slot)
  }

  def canUseSlotForSplitTask(task: Task, slot: FreeTimeSlot, remainingMinutes: Long): Boolean = {
    val requiredMinutes = math.min(remainingMinutes, task.minBlockMinutes.toLong)

    isSlotWithinTaskDates(task,slot) && slot.durationMinutes >= requiredMinutes
  }

  def createWholeTaskBlock(task: Task, slot: FreeTimeSlot): ScheduledBlock = {
    val endTime = slot.startTime.plusMinutes(task.remainingMinutes)

    ScheduledBlock(task.id, slot.date, slot.startTime, endTime)
  }

  def createSplitTaskBlock(task: Task, slot: FreeTimeSlot, remainingMinutes: Long): ScheduledBlock = {
    val blockMinutes = math.min(remainingMinutes, slot.durationMinutes)

    val endTime = slot.startTime.plusMinutes(blockMinutes)

    ScheduledBlock(task.id, slot.date, slot.startTime, endTime)
  }

  def sortFreeTimeSlots(freeTimeSlots: List[FreeTimeSlot]): List[FreeTimeSlot] = {
    freeTimeSlots.sortWith((slot1, slot2) =>
      slot1.date.isBefore(slot2.date) ||
      (slot1.date == slot2.date && slot1.startTime.isBefore(slot2.startTime))
    )
  }

  def getRemainingSlot(slot: FreeTimeSlot, block: ScheduledBlock): Option[FreeTimeSlot] = {
    if (block.endTime.isBefore(slot.endTime)) {
      Some(FreeTimeSlot(slot.date, block.endTime, slot.endTime))
    } else {
      None
    }
  }

  def updateFreeTimeSlots(freeTimeSlots: List[FreeTimeSlot],
                          usedSlot: FreeTimeSlot,
                          block:ScheduledBlock
                         ): List[FreeTimeSlot] = {
    var updatedSlots = List.empty[FreeTimeSlot]

    for (slot <- freeTimeSlots) {
      if (slot == usedSlot) {
        getRemainingSlot(slot, block) match {
          case Some(remainingSlot) =>
            updatedSlots = updatedSlots :+ remainingSlot
          case None =>
        }
      } else {
        updatedSlots = updatedSlots :+ slot
      }
    }
    sortFreeTimeSlots(updatedSlots)
  }

  def scheduleWholeTask(
                         task: Task,
                         freeTimeSlots: List[FreeTimeSlot]
                       ): (Option[ScheduledBlock], List[FreeTimeSlot]) = {
    val sortedSlots = sortFreeTimeSlots(freeTimeSlots)

    val suitableSlot = sortedSlots.find(slot => canPlaceWholeTask(task, slot))

    suitableSlot match {
      case Some(slot) =>
        val block = createWholeTaskBlock(task, slot)

        val updatedFreeTimeSlots = updateFreeTimeSlots(freeTimeSlots, slot, block)

        (Some(block), updatedFreeTimeSlots)
      case None =>
        (None, freeTimeSlots)
    }
  }

  def scheduleSplitTask(
                         task: Task,
                         freeTimeSlots: List[FreeTimeSlot]
                       ): (Option[List[ScheduledBlock]], List[FreeTimeSlot]) = {
    var remainingMinutes = task.remainingMinutes
    var blocks = List.empty[ScheduledBlock]

    val sortedSlots = sortFreeTimeSlots(freeTimeSlots)
    var updatedFreeTimeSlots = freeTimeSlots

    for (slot <- sortedSlots) {
      if (remainingMinutes > 0 && canUseSlotForSplitTask(task, slot, remainingMinutes)) {
        val block = createSplitTaskBlock(task, slot, remainingMinutes)

        blocks = blocks :+ block
        remainingMinutes -= block.durationMinutes

        updatedFreeTimeSlots = updateFreeTimeSlots(updatedFreeTimeSlots, slot, block)
      }
    }

    if (remainingMinutes == 0) {
      (Some(blocks), updatedFreeTimeSlots)
    } else {
      (None, freeTimeSlots)
    }
  }

  def scheduleTask(
                    task: Task,
                    freeTimeSlots: List[FreeTimeSlot]
                  ): (Option[List[ScheduledBlock]], List[FreeTimeSlot]) = {
    if (task.canSplit) {
      scheduleSplitTask(task, freeTimeSlots)
    } else {
      val (blockOption, updatedFreeTimeSlots) = scheduleWholeTask(task, freeTimeSlots)
      blockOption match {
        case Some(block) =>
          (Some(List(block)), updatedFreeTimeSlots)
        case None =>
          (None, updatedFreeTimeSlots)
      }
    }
  }

  def sortTasks(tasks: List[Task]): List[Task] = {
    tasks.sortWith((task1, task2) =>
      if (task1.priority.value != task2.priority.value) {
        task1.priority.value > task2.priority.value
      } else if (task1.deadline != task2.deadline) {
        task1.deadline.isBefore(task2.deadline)
      } else {
        task1.id < task2.id
      })
  }

  def shouldScheduleTask(task: Task): Boolean = {
    task.remainingMinutes > 0 &&
      task.status != TaskStatus.COMPLETED &&
      task.status != TaskStatus.CANCELLED
  }

  def isTaskOverdue(task: Task, currentDate: LocalDate): Boolean = {
    task.remainingMinutes > 0 &&
      task.status != TaskStatus.COMPLETED &&
      task.status != TaskStatus.CANCELLED &&
      task.deadline.isBefore(currentDate)
  }

  def scheduleAllTasks(
                        tasks: List[Task],
                        freeTimeSlots: List[FreeTimeSlot],
                        currentDate: LocalDate
                      ): (List[ScheduledBlock], List[FreeTimeSlot], List[Task], List[Task]) = {
    val sortedTasks = sortTasks(tasks)

    var allBlocks = List.empty[ScheduledBlock]
    var updatedFreeTimeSlots = freeTimeSlots.filter(slot => !slot.date.isBefore(currentDate))
    var unplacedTasks = List.empty[Task]
    var updatedTasks = tasks

    for (task <- sortedTasks) {
      if (isTaskOverdue(task, currentDate)) {
        val overdueTask = task.copy(status = TaskStatus.OVERDUE)

        updatedTasks = updatedTasks.map(currentTask =>
          if (currentTask.id == task.id) {
            overdueTask
          } else {
            currentTask
          })
      } else if (shouldScheduleTask(task)) {
        val (blocksOption, newFreeTimeSlots) = scheduleTask(task, updatedFreeTimeSlots)

        blocksOption match {
          case Some(blocks) =>
            allBlocks = allBlocks ++ blocks
            updatedFreeTimeSlots = newFreeTimeSlots

            val updatedTask = task.copy(status = Task.getInitialStatus(task.totalHours, task.completedHours))
            updatedTasks = updatedTasks.map(currentTask =>
              if (currentTask.id == task.id) {
                updatedTask
              } else {
                currentTask
              })
          case None =>
            val unplacedTask = task.copy(status = TaskStatus.UNPLACED)
            unplacedTasks = unplacedTasks :+ unplacedTask
            updatedTasks = updatedTasks.map(currentTask =>
            if (currentTask.id == task.id) {
              unplacedTask
            } else {
              currentTask
            })
        }
      }
    }

    (allBlocks, updatedFreeTimeSlots, unplacedTasks, updatedTasks)
  }
}
