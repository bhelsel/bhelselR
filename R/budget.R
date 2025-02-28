#' @title calculate_salary
#' @description Input the base salary, fringe rate, and either the effort described
#'     in calendar months or percentage to calculate the salary, fringe, and
#'     total amount requested for key personnel.
#' @param salary Base salary of key personnel
#' @param fringe Fringe rate at the University. The fringe rate can be a decimal
#'     ranging from 0 to < 1 or a numeric variable ranging from 1 to 100.
#' @param calmo The effort described in calender year months, Default: NULL
#' @param percent The effort described in percent. The percent effort can be a decimal
#'     ranging from 0 to < 1 or a numeric variable ranging from 1 to 100, Default: NULL
#' @param print Prints the effort described in calendar months and percent and the
#'     salary, fringe, and total amounts requested, Default: FALSE
#' @return A list of the effort in calendar months and percent and the salary, fringe
#'     and total amount requested.
#' @details Input the base salary, fringe rate, and either the effort described
#'     in calendar months or percentage to calculate the salary, fringe, and
#'     total amount requested for key personnel.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  calculate_salary(salary = 75000, fringe = 34, percent = 15, print = TRUE)
#'  }
#' }
#' @rdname calculate_salary
#' @export 


calculate_salary <- function(salary, fringe, calmo = NULL, percent = NULL, print = FALSE){
  if(is.null(calmo) & is.null(percent)) stop("Need to provide calendar months or percent effort.")
  if(!is.null(percent)) if(percent > 1) percent <- percent / 100
  if(fringe > 1) fringe <- fringe / 100
  if(is.null(calmo)) calmo <- percent * 12
  if(is.null(percent)) percent <- calmo / 12
  salreq <- salary * percent
  fringereq <- salreq * fringe
  total <- format(round(salreq) + round(fringereq), big.mark = ",")
  salreq <- format(round(salreq), big.mark = ",")
  fringereq <- format(round(fringereq), big.mark = ",")
  if(print){
    message(
      paste(
        sprintf("Calender Months: %.2f", calmo),
        sprintf("Percent Effort: %.2f%%", percent * 100),
        sprintf("Salary Requested: %s", salreq),
        sprintf("Fringe: %s", fringereq),
        sprintf("Total Amount: %s", total),
        sep = "\n")
    )
  } else{
    return(list(
      `Calendar Months` = calmo,
      `Percent Effort` = percent * 100,
      `Salary Requested` = salreq,
      Fringe = fringereq,
      `Total Amount` = total
    ))
  }
}
