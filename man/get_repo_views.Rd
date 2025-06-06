% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessGitHubAPI.R
\name{get_repo_views}
\alias{get_repo_views}
\title{get_repo_views}
\usage{
get_repo_views(token.pathname, user, repo, ...)
}
\arguments{
\item{token.pathname}{Path name to the GitHub API access token.}

\item{user}{Username of a GitHub user with read and write access to the GitHub repository.}

\item{repo}{GitHub repository name to be accessed.}

\item{...}{arguments passed to \code{\link[data.table]{fwrite}}}
}
\value{
Saves the repository views to a CSV file in a specific repository folder.
}
\description{
Extract repository views using the GitHub API.
}
\details{
Extract repository views using the GitHub API.
}
\examples{
\dontrun{
 get_repo_views(
     token.pathname = "Full path to token",
     user = "bhelsel",
     repo = "agcounts"
 )
}
}
\seealso{
\code{\link[httr]{GET}},
\code{\link[httr]{stop_for_status}},
\code{\link[httr]{content}}
\code{\link[jsonlite]{toJSON, fromJSON}}
\code{\link[data.table]{fwrite}}
\code{\link[utils]{read.table}}
}
