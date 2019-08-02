#' Get the current git commit
#'
#' Return the hash of the current git commit.
#' We use this for adding to the report or the database.
#'
#' @param docker If `TRUE` the default, will make sure you
#'   are in the Git repository before checking for the
#'   latest hash. If `FALSE`, we assume you are there.
#' @return The SHA-1 hash of the current Git Commit
#' @examples
#' current_git_commit()
#' @export
current_git_commit <- function(docker = TRUE) {
  git_command <- dplyr::if_else(docker, 'cd /home/rstudio/nsse2017/; git rev-parse HEAD',
                         'git rev-parse HEAD')
  system(git_command, intern = TRUE)
}
