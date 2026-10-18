package model

import upickle.default.ReadWriter

case class AppConfig(
                      logFile: String,
                      stateFile: String,
                      reportDirectory: String
                    ) derives ReadWriter