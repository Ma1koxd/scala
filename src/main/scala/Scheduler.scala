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

  def canUseSlot(task: Task, slot: FreeTimeSlot): Boolean = {
    if (task.canSplit){
      canUseSlotForSplitTask(task,slot, task.remainingMinutes)
    } else {
      canPlaceWholeTask(task, slot)
    }
  }

  def findSuitableSlots(task: Task, freeTimeSlots: List[FreeTimeSlot]): List[FreeTimeSlot] = {
    freeTimeSlots.filter(slot => canUseSlot(task,slot))
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

  def scheduleWholeTaskWithUpdatedSlots(
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

  def scheduleSplitTaskWithUpdatedSlots(
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
      scheduleSplitTaskWithUpdatedSlots(task, freeTimeSlots)
    } else {
      val (blockOption, updatedFreeTimeSlots) = scheduleWholeTaskWithUpdatedSlots(task, freeTimeSlots)
      blockOption match {
        case Some(block) =>
          (Some(List(block)), updatedFreeTimeSlots)
        case None =>
          (None, updatedFreeTimeSlots)
      }
    }
  }
}
