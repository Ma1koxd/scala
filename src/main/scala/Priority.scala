enum Priority(val value: Int){
  case LOW extends Priority(10)
  case NORMAL extends Priority(20)
  case HIGH extends Priority(30)
  case CRITICAL extends Priority(40)
}