check_norm <- function(variables) {
  new.data <- data[, variables, drop = FALSE]
  hist <- new.data %>%
    dplyr::mutate_all(.funs = as.numeric) %>%
    tidyr::gather() %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggplot2::geom_histogram(
      bins = 15,
      fill = "#0c4c8a",
      color = "black",
      na.rm = TRUE
    ) +
    ggthemes::theme_economist()

  qq <- new.data %>%
    tidyr::gather() %>%
    ggplot2::ggplot(aes(sample = value)) +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggplot2::geom_qq_line() +
    ggplot2::geom_qq() +
    ggthemes::theme_wsj()

  shp.wilk <- variables %>%
    sapply(
      function(x)
        stats::shapiro.test(as.numeric(purrr::as_vector(new.data[, x])))
    ) %>%
    t() %>%
    data.frame() %>%
    dplyr::select(statistic, p.value) %>%
    dplyr::mutate(
      statistic = round(as.numeric(gsub("[c(W =)]", "", x = statistic)), 3),
      p.value = round(as.numeric(p.value), 3)
    ) %>%
    dplyr::filter(p.value > 0.05) %>%
    cbind(`Variable Names` = rownames(.), .) %>%
    gt::gt() %>%
    gtExtras::gt_theme_538() %>%
    gt::tab_header(title = "Normally Distributed Variables")

  return(list(shp.wilk, qq, hist))
}
