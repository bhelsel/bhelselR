% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessGitHubAPI.R
\name{authorize_github_app}
\alias{authorize_github_app}
\title{authorize_github_app}
\usage{
authorize_github_app(
  directory,
  appname,
  key,
  secret = NULL,
  redirect_uri = httr::oauth_callback()
)
}
\arguments{
\item{directory}{Directory location where the access tokens and data should be stored.}

\item{appname}{name of the application.  This is not used for OAuth, but is
used to make it easier to identify different applications.}

\item{key}{consumer key, also sometimes called the client ID}

\item{secret}{consumer secret, also sometimes called the client secret.
Despite its name, this does not necessarily need to be protected like a
password, i.e. the user still has to authenticate themselves and grant the
app permission to access resources on their behalf. For example, see
Google's docs for \href{https://developers.google.com/identity/protocols/oauth2}{OAuth2 for installed applications}.}

\item{redirect_uri}{The URL that user will be redirected to after
authorisation is complete. You should generally leave this as the default
unless you're using a non-standard auth flow (like with shiny).}
}
\value{
Saves your access token to a tokens folder in the directory and returns the GitHub token.
}
\description{
Use your GitHub key and secret to authorize your app for GitHub API use.
This includes full control of private repositories.
}
\details{
DETAILS
}
\examples{
\dontrun{
 authorize_github_app(
   directory = "Path to directory",
   appname = "myapp",
   key = Sys.getenv("GITHUB_KEY"),
   secret = Sys.getenv("GITHUB_SECRET")
 )
}
}
\seealso{
\code{\link[httr]{oauth_callback}},
\code{\link[httr]{oauth_endpoints}},
\code{\link[httr]{oauth_app}},
\code{\link[httr]{oauth2.0_token}}
}
