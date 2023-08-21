#' @title sendmail
#' @description Sends an email in R using the reticulate package as an R-Python interface for the Python email package
#' @param host The name of the remote host to which to connect
#' @param port The specific port of the remote host to which to connect
#' @param sender The email address of the senders. The sender's email can either be just an email or a combined vector of name and email.
#' @param recipients The email address of the recipients either as a single email or a combined vector of emails
#' @param subject The subject of the email
#' @param body The body of the email either in HTML or plain text
#' @param attachments Full path name to single file or a directory of files that should be included in the email, Default: NULL
#' @param html Set this parameter to TRUE if the body contains HTML, Default: FALSE
#' @return Sends an email via R
#' @details Sends an email in R using the reticulate package as an R-Python interface for the Python email package
#' @seealso 
#'  \code{\link[reticulate]{import}}, \code{\link[reticulate]{r-py-conversion}}
#' @rdname sendmail
#' @export 
#' @importFrom reticulate import `%as%` r_to_py

sendmail <- function(host, port, sender, recipients, subject, body, attachments = NULL, html = FALSE){
  
  smtplib <- reticulate::import("smtplib")
  email <- reticulate::import("email")
  io <- reticulate::import("io")
  `%as%` <- reticulate::`%as%`
  
  # Start email and add sender, recipients, and subject
  msg = email$mime$multipart$MIMEMultipart()
  
  if(length(sender) == 2) sender <- email$utils$formataddr(sender)
  
  if(length(recipients >= 2)) {
    msg["To"] <- paste(recipients, collapse = ", ")
  } else{
    msg["To"] = recipients
    }
    
  msg["From"] = sender
  msg["Subject"] = subject
  
  # HTML
  if(html){
    mytext = email$mime$text$MIMEText(body, "html")  
  } else{
    mytext = email$mime$text$MIMEText(body, "plain")  
  }
  
  email$message$Message$attach(msg, mytext)
  
  # Attachments
  if(!is.null(attachments)){
    
    if(length(attachments) == 1){
      if(dir.exists(attachments)){
        attachments <- list.files(attachments, full.names = TRUE)
      }
    }
      
    for(file in attachments){
      
      with(io$open(file, "rb") %as% attachment, {
        part = email$mime$multipart$MIMEBase("application", "octet-stream")
        email$message$Message$set_payload(part, io$BufferedReader$read(attachment))
      })
      
      email$encoders$encode_base64(part)
      email$message$Message$add_header(part, "Content-disposition", sprintf("attachment; filename= %s", basename(file)))
      email$message$Message$attach(msg, part)
    }
  }
  
  server =  smtplib$SMTP(host, as.integer(port))
  msg = email$message$Message$as_string(msg)
  smtplib$SMTP$sendmail(server, sender, reticulate::r_to_py(as.list(recipients)), msg)
}




