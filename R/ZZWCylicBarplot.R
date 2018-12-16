#' @title ZZWCylicBarplot
#' @author Zhiwei Zhou
#' @param raw_data data.frame. 2 columns, 1st column: name; 2nd column: frequency
#' @param min_radius min radius of cycle
#' @param max_radius max radius of cycle
#' @param is.output Default: FALSE
#' @importFrom magrittr '%>%'
#' @export
#' @examples
#' raw_data <- seq(10,1)
#' names(raw_data) <- letters[1:10]
#' raw_data <- data.frame(name=names(raw_data), frequency=as.numeric(raw_data),
#'                        stringsAsFactors = F)

ZZWCylicBarplot <- function(raw_data,
                            min_radius=NULL,
                            max_radius=NULL,
                            is_output=FALSE,
                            file_name='cylic_barplot.pdf'){

  label <- paste(raw_data$name, " (", raw_data$frequency, ")", sep = "")

  # label <- factor(x = raw_data$name,
  #                 levels = raw_data$name,
  #                 # levels = seq(length(superclass_label)),
  #                 labels = raw_data$name)

  label <- factor(x = label,
                  levels = label,
                  # levels = seq(length(superclass_label)),
                  labels = label)

  if (is.null(max_radius)) {
    max_radius <- 1.1*max(raw_data$frequency)
  }

  if (is.null(min_radius)) {
    min_radius <- 0-min(raw_data$frequency)
  }



  raw_data <- raw_data %>%
    dplyr::mutate(id = dplyr::row_number(),
                  label = label,
                  # label2=superclass_label_2,
                  angle = 90 - 360 * (id - 0.5) / dplyr::n())

  cylic_barplot <- raw_data  %>%
    ggplot2::ggplot(ggplot2::aes(factor(id), frequency, fill = label, label = label)) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::geom_text(hjust = 0,
                       angle = raw_data$angle,
                       alpha = 1,
                       size = 3) +
    # geom_text(hjust = 0, angle = raw_data$angle, alpha = 1) +
    ggplot2::coord_polar() + # polar coordinate
    # ggtitle('Mean dimond price') +
    ggplot2::ylim(min_radius, max_radius) + # min: min radius of cycle; max: max radius of cycle
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = 'bold'))
  # guides(fill = FALSE)
  # guides(fill = TRUE)

  if (is_output) {
    ggplot2::ggsave(plot = cylic_barplot,
                    filename = file_name,
                    width = 6,
                    height = 6)
  }

  cylic_barplot

}
