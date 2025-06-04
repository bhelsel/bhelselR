# Copyright © 2021 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title cor_matrix
#' @description A function to visualize correlations between variables.
#' @param dataset The data set where the variables can be located.
#' @param vars The names of the variables as they appear in the data set.
#' @param var.names The names of the variables as they should appear on the plot. If no variable names are entered, the plot will use the variable names from the data set, Default: NULL
#' @param type Type of correlation (i.e., spearman or pearson), Default: 'spearman'
#' @param p.value The p-value threshold to use for significance. A table of p-values under this threshold will be returned as the second element of this list, Default: 0.05
#' @param colors The low and high end colors on the plot to represent strong negative and positive correlations, respectively, Default: c("#B2BDED", "#C6E0B4")
#' @return Returns a list where the first element is the plot and the second element is a table of p-values that are under the p.value threshold
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[Hmisc]{rcorr}}
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}
#' @rdname cor_matrix
#' @export
#' @importFrom Hmisc rcorr
#' @importFrom reshape2 melt
#' @import ggplot2

cor_matrix <- function(
  dataset,
  vars,
  var.names = NULL,
  type = "spearman",
  p.value = 0.05,
  colors = c("#B2BDED", "#C6E0B4")
) {
  type <- match.arg(tolower(type), c("spearman", "pearson"))

  corres <-
    dataset %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    as.matrix() %>%
    Hmisc::rcorr(., type = type)

  cor.matrix.r <-
    corres %>%
    magrittr::extract2("r") %>%
    round(., 2)

  cor.matrix.p <-
    corres %>%
    magrittr::extract2("P") %>%
    round(., 2)

  cor.matrix.r[lower.tri(cor.matrix.r)] <- NA
  cor.matrix.p[lower.tri(cor.matrix.p)] <- NA

  if (length(var.names) != 0) {
    colnames(cor.matrix.r) <- c(var.names)
    rownames(cor.matrix.r) <- c(var.names)
    colnames(cor.matrix.p) <- c(var.names)
    rownames(cor.matrix.p) <- c(var.names)
  }

  plot <-
    cor.matrix.r %>%
    reshape2::melt(., na.rm = TRUE) %>%
    ggplot(., aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "black") +
    scale_fill_gradient2(
      low = colors[1],
      mid = "#ffffff",
      high = colors[2],
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = paste0(
        toupper(substr(type, 1, 1)),
        substr(type, 2, nchar(type)),
        "\nCorrelation"
      )
    ) +
    theme_minimal() +
    coord_fixed() +
    geom_text(
      aes(Var2, Var1, label = value, fontface = "bold"),
      color = "black",
      size = 4
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        size = 12,
        hjust = 1,
        face = "bold"
      ),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.5, 0.7),
      legend.direction = "horizontal"
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 7,
        barheight = 1,
        title.position = "top",
        title.hjust = 0.5
      )
    )

  cor.matrix.p %<>%
    reshape2::melt(., na.rm = TRUE) %>%
    dplyr::filter(value < p.value) %>%
    dplyr::mutate(value = ifelse(value < 0.001, "< 0.001", value))

  return(list(plot, cor.matrix.p))
}
