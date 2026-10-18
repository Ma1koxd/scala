package model

import upickle.default.ReadWriter

case class AppState(
                     tasks: List[Task],
                     freeTimeSlots: List[FreeTimeSlot],
                     nextId: Int
                   ) derives ReadWriter