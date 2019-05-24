#' Function to execute an SS run
#' @description This function runs stock synthesis in the specified directory for the model name given. It is copied almost verbatim from \code{ss3sim::run_ss3model()}.
#' @param species species name
#' @param model_dir directory containing the model files
#' @param ss_name name of the ss model contained within
#' @param admb_options options for running admb, like "-nohess", "-noest"
#' @param ignore.stdout Logical: passed on to
#'   \code{\link[base]{system}}.
#' @param show_output  Passed to \code{system}. If \code{TRUE} then ADMB
#'   output is not printed on screen. This will be slightly faster. Set to
#'   \code{FALSE} to help with debugging.
#' @param iteration_pause how long to pause between model iterations if this function is called in a loop
#' @param ... Anything else to pass to \code{\link[base]{system}}
#' @export
run_stocksynthesis <- function(os, species, model_dir, admb_options = " -noest", ignore.stdout = TRUE, show_output = FALSE, iteration_pause = 0.05,...){

  os <- .Platform$OS.type

  message(paste0("Running ss EM for species: ", species))
if(os == "unix") {
  system(paste0("cd ",
                paste0("./inst/extdata/", model_dir),";","ss ", admb_options), ignore.stdout = ignore.stdout, ...)

} else {
  wd <- getwd()
  setwd(paste0("./inst/extdata/",model_dir))
  system(paste0("ss ",
                admb_options),
         invisible = TRUE, ignore.stdout = ignore.stdout,
         show.output.on.console = show_output, ...)
  setwd(wd)

}
  #Pause in between iterations - required on some systems
  Sys.sleep(iteration_pause)
}
