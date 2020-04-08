# Enable or disable logging
loggingEnabled <- TRUE;
printLog <- function (msg) {
  if (loggingEnabled) {
    print(msg);
  }
}