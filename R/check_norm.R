check_norm <- function(variables){
  new.data <- data[, variables]
  hist <- new.data %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 15, fill="#0c4c8a", color="black", na.rm = TRUE) +
    theme_economist()
  
  qq <- new.data %>%
    gather %>%
    ggplot(aes(sample = value)) +
    facet_wrap(~ key, scales = "free") +
    geom_qq_line() +
    geom_qq() +
    theme_wsj()
  
  shp.wilk <- variables %>%
    sapply(function(x) shapiro.test(as.numeric(as_vector(new.data[, x])))) %>%
    t() %>%
    data.frame() %>%
    select(statistic, p.value) %>%
    mutate(statistic = round(as.numeric(gsub("[c(W =)]", "", x = statistic)), 3),
           p.value = round(as.numeric(p.value), 3)) %>%
    filter(p.value > 0.05) %>%
    cbind(`Variable Names` = rownames(.), .) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(title = "Normally Distributed Variables")
  
  return(list(shp.wilk, qq, hist))
}
