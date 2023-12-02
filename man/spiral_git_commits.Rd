\name{spiral_git_commits}
\alias{spiral_git_commits}
\title{
Visualize git commits
}
\description{
Visualize git commits
}
\usage{
spiral_git_commits(repo = ".", show_legend = TRUE,
    start = NULL, end = Sys.Date(),
    pt_range = c(2, 16), commits_range = c(2, 20))
}
\arguments{

  \item{repo}{Path of the git repo. The value can be a single repo or a vector of repos.}
  \item{show_legend}{Whether to show the legend.}
  \item{start}{Start date. By default it is the first date of the commit. The value can be a string such as "2022-01-01" or a \code{Date} object.}
  \item{end}{End date. By default it is the current date. The value can be a string such as "2022-01-01" or a \code{Date} object.}
  \item{pt_range}{Range of the point sizes.}
  \item{commits_range}{Range of the numbers of commits.}

}
\examples{
# There is no example
NULL

}
