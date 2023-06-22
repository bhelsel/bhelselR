#' @title authorize_github_app
#' @description Use your GitHub key and secret to authorize your app for GitHub API use.
#' This includes full control of private repositories.
#' @param directory Directory location where the access tokens and data should be stored.
#' @inheritParams httr::oauth_app
#' @return Saves your access token to a tokens folder in the directory and returns the GitHub token.
#' @details DETAILS
#' @examples 
#' \dontrun{
#'  authorize_github_app(
#'    directory = "Path to directory", 
#'    appname = "myapp", 
#'    key = Sys.getenv("GITHUB_KEY"), 
#'    secret = Sys.getenv("GITHUB_SECRET")
#'  )
#' }
#' @seealso 
#'  \code{\link[httr]{oauth_callback}}, 
#'  \code{\link[httr]{oauth_endpoints}}, 
#'  \code{\link[httr]{oauth_app}}, 
#'  \code{\link[httr]{oauth2.0_token}}
#' @rdname authorize_github_app
#' @export 
#' @importFrom httr oauth_callback oauth_endpoints oauth_app oauth2.0_token

authorize_github_app <- function(directory, appname, key, secret = NULL, redirect_uri = httr::oauth_callback()){
  endpoint <- httr::oauth_endpoints("github")
  myapp <- httr::oauth_app(appname = appname, key = key, secret = secret)
  token <- httr::oauth2.0_token(endpoint, myapp, scope = "repo")
  # Save Token
  if(!dir.exists(paste0(directory, "/tokens"))){
    dir.create(paste0(directory, "/tokens"))
  }
  saveRDS(token, file = paste0(directory, "/tokens/",   token$app$appname, "_token.RData"))
  return(token)
}

#' @title get_repo_views
#' @description Extract repository views using the GitHub API.
#' @param token.pathname Path name to the GitHub API access token.
#' @param user Username of a GitHub user with read and write access to the GitHub repository.
#' @param repo GitHub repository name to be accessed.
#' @param ... arguments passed to \code{\link[data.table]{fwrite}}
#' @return Saves the repository views to a CSV file in a specific repository folder.
#' @details Extract repository views using the GitHub API.
#' @examples 
#'\dontrun{
#'  get_repo_views(
#'      token.pathname = "Full path to token", 
#'      user = "bhelsel", 
#'      repo = "agcounts"
#'  )
#' }
#' @seealso 
#'  \code{\link[httr]{GET}}, 
#'  \code{\link[httr]{stop_for_status}}, 
#'  \code{\link[httr]{content}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[utils]{read.table}}
#' @rdname get_repo_views
#' @export 
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom data.table fwrite
#' @importFrom utils read.csv

get_repo_views <- function(token.pathname, user, repo, ...){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  url <- sprintf("https://api.github.com/repos/%s/%s/traffic/views", user, repo)
  request <- httr::GET(url, token)
  httr::stop_for_status(request)
  data <- jsonlite::fromJSON(httr::content(request, as = "text"))$views
  
  
  if(is.null(dim(data))) {
    message(sprintf("No data found after the GitHub API request for the %s repository", repo))
  } else {
    data$timestamp <- as.Date(data$timestamp)
    colnames(data) <- c("Date", "Views", "Unique.Views")
    
    data <- 
      seq(data$Date[1], Sys.Date(), 1) %>%
      as.character(.) %>%
      setdiff(as.character(data$Date)) %>%
      {data.frame(
        Date = as.Date(.), Views = rep(0, length(.)), 
        Unique.Views = rep(0, length(.))
      )} %>%
      rbind(data) %>%
      .[order(.$Date), ] %>%
      structure(., row.names = seq(nrow(.)))
    
    
    if(!repo %in% list.files(directory)){
      dir.create(paste0(directory, "/", repo))
    }
    views_file_location <- paste0(directory, "/", repo, "/", "views.csv")
    if(!file.exists(views_file_location)){
      data.table::fwrite(
        x = data,
        file = views_file_location,
        dateTimeAs = "write.csv",
        ...
      )
    } else{
      existing_data <- utils::read.csv(views_file_location, colClasses = c(Date = "Date"))
      existing_data <- existing_data[1:(nrow(existing_data)-1), ]
      data <- data[!data$Date %in% existing_data$Date, ]
      data <- rbind(x = existing_data, y = data)
      data.table::fwrite(
        x = data,
        file = views_file_location,
        dateTimeAs = "write.csv",
        ...
      )
    }
  }
}


#' @title get_repo_clones
#' @description Extract repository clones using the GitHub API.
#' @param token.pathname Path name to the GitHub API access token.
#' @param user Username of a GitHub user with read and write access to the GitHub repository.
#' @param repo GitHub repository name to be accessed.
#' @param ... arguments passed to \code{\link[data.table]{fwrite}}
#' @return Saves the repository clones to a CSV file in a specific repository folder.
#' @details Extract repository clones using the GitHub API.
#' @examples 
#' \dontrun{
#'  get_repo_views(
#'      token.pathname = "Full path to token", 
#'      user = "bhelsel", 
#'      repo = "agcounts"
#'  )
#' }
#' @seealso 
#'  \code{\link[httr]{GET}}, 
#'  \code{\link[httr]{stop_for_status}}, 
#'  \code{\link[httr]{content}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[utils]{read.table}}
#' @rdname get_repo_clones
#' @export 
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom data.table fwrite
#' @importFrom utils read.csv

get_repo_clones <- function(token.pathname, user, repo, ...){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  url <- sprintf("https://api.github.com/repos/%s/%s/traffic/clones", user, repo)
  request <- httr::GET(url, token)
  httr::stop_for_status(request)
  data <- jsonlite::fromJSON(httr::content(request, as = "text"))$clones
  data$timestamp <- as.Date(data$timestamp)
  colnames(data) <- c("Date", "Clones", "Unique.Clones")
  
  if(is.null(dim(data))) {
    message(sprintf("No data found after the GitHub API request for the %s repository", repo))
  } else {
    data <- 
      seq(data$Date[1], Sys.Date(), 1) %>%
      as.character(.) %>%
      setdiff(as.character(data$Date)) %>%
      {data.frame(
        Date = as.Date(.), Clones = rep(0, length(.)), 
        Unique.Clones = rep(0, length(.))
      )} %>%
      rbind(data) %>%
      .[order(.$Date), ] %>%
      structure(., row.names = seq(nrow(.)))
    
    if(!repo %in% list.files(directory)){
      dir.create(paste0(directory, "/", repo))
    }
    clones_file_location <- paste0(directory, "/", repo, "/", "clones.csv")
    if(!file.exists(clones_file_location)){
      data.table::fwrite(
        x = data,
        file = clones_file_location,
        dateTimeAs = "write.csv",
        ...
      )
    } else{
      existing_data <- utils::read.csv(clones_file_location, colClasses = c(Date = "Date"))
      existing_data <- existing_data[1:(nrow(existing_data)-1), ]
      data <- data[!data$Date %in% existing_data$Date, ]
      data <- rbind(x = existing_data, y = data)
      data.table::fwrite(
        x = data,
        file = clones_file_location,
        dateTimeAs = "write.csv",
        ...
      )
    }
  }
}
