package model

import upickle.default.ReadWriter

enum Priority(val value: Int) derives ReadWriter{
  case LOW extends Priority(10)
  case NORMAL extends Priority(20)
  case HIGH extends Priority(30)
  case CRITICAL extends Priority(40)
}