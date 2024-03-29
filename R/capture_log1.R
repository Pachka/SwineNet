#' A function rather aimed at developers
#' @description A function that does blabla, blabla.
#' @keywords internal
#' @export

capture_log1 <- function(f) {
  logs <- list()
  add_log <- function(type, message) {
    new_l <- logs
    new_log <- list(timestamp = format(Sys.time(), tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                    type = type,
                    message =  message)
    new_l[[length(new_l) + 1]]  <- new_log
    logs <<- new_l
  }
  res <- withCallingHandlers(
    tryCatch(f, error=function(e) {
      add_log("error", conditionMessage(e))
      NULL
    }), warning=function(w) {
      add_log("warning", conditionMessage(w))
      invokeRestart("muffleWarning")
    }, message = function(m) {
      add_log("message", conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(res, logs = logs)
}