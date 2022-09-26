cutpoint.list <- list(
  freedson.child = list(
    "1" = list(
      "5" = c("light:101", "moderate:1291", "vigorous:3581"),
      "6" = c("light:101", "moderate:1400", "vigorous:3758"),
      "7" = c("light:101", "moderate:1515", "vigorous:3947"),
      "8" = c("light:101", "moderate:1638", "vigorous:4147"),
      "9" = c("light:101", "moderate:1770", "vigorous:4360"),
      "10" = c("light:101", "moderate:1910", "vigorous:4588"),
      "11" = c("light:101", "moderate:2059", "vigorous:4830"),
      "12" = c("light:101", "moderate:2220", "vigorous:5094"),
      "13" = c("light:101", "moderate:2393", "vigorous:5357"),
      "14" = c("light:101", "moderate:2580", "vigorous:5679"),
      "15" = c("light:101", "moderate:2781", "vigorous:6007"),
      "16" = c("light:101", "moderate:3000", "vigorous:6363"),
      "17" = c("light:101", "moderate:3239", "vigorous:6751"))),
  troiano.adult = list(
    "1" = c("light:101", "moderate:2020", "vigorous:5999")),
  fariasbarnett.adult = list(
    "1" = c("light:25", "mvpa:1013"),
    "3" = c("light:200", "mvpa:1924")),
  freedson.adult = list(
    "1" = c("light:100", "moderate:1952", "vigorous:5725", "very.vigorous:9499"), 
    "3" = c("light:200", "moderate:2690", "vigorous:6167", "very.vigorous:9642")),
  montoye.adult = list(
    "3" = c("light:2860", "mvpa:3941")))

# name = "freedson.adult"
# 
# 
# 
# function(name = NULL, axis = c(1, 3), type = NULL){
#   if(is.null(name)==FALSE){
#     if(1 %in% axis){
#       try(v.axis <- cutpoint.list[[name]][["1"]], stop("Cut points not found."))
#       intensity <- sapply(v.axis[1:length(v.axis)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[1])
#       values <- sapply(v.axis[1:length(v.axis)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[2])
#       v.axis <- as.matrix(cbind(intensity, values))
#       list("Vertical axis cut points", v.axis)
#     }
#     if(3 %in% axis){
#       try(vm <- cutpoint.list[[name]][["3"]], stop("Cut points not found."))
#       intensity <- sapply(vm[1:length(vm)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[1])
#       values <- sapply(vm[1:length(vm)], function(x) unlist(stringr::str_split(x, ":"), use.names=FALSE)[2])
#       vm <- as.matrix(cbind(intensity, values))
#       list("Vector magnitude cut points", vm)
#     }
#     
#   if(is.null(type)==FALSE){
#     
#   }
#   
# }




