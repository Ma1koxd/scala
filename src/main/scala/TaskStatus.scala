import upickle.default.ReadWriter

enum TaskStatus derives ReadWriter{
  case PLANNED, IN_PROGRESS, COMPLETED, OVERDUE, CANCELLED, UNPLACED
}