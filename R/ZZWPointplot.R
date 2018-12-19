#' @title ZZWPointplot
#' @description point plot
#' @author Zhiwei Zhou
#' \email{zhouzw@@sioc.ac.cn}
#' @param raw_data data.frame, it should contains 3 columns, 'x', 'y', 'colour'
#' @param val.x variable x
#' @param val.y variable y
#' @param val.colour variable colors
#' @param p.shape shape of point, please ref ggplot2
#' @param p.size size of point, please ref ggplot2
#' @param p.alpha alpha of point, please ref ggplot2
#' @param label.title title, please ref ggplot2
#' @param label.xlab xlab, please ref ggplot2
#' @param label.ylab ylab, please ref ggplot2
#' @param is.plot
#' @param file.name
#' @param width
#' @param height
#' @export
#' @examples
#' raw_data <- data.frame(x=seq(1:10), y=seq(10:1), colour=letters[1:10], stringAsFactors=F)
#' ZZWPointplot(raw_data, x, y, colour)

ZZWPointplot <- function(raw_data,
                         x,
                         y,
                         colour,
                         p.shape = 16,
                         p.size = 3,
                         p.alpha = 0.5,
                         label.title = "",
                         label.xlab = 'x',
                         label.ylab = 'y',
                         is.plot = FALSE,
                         file.name = "ZZWPointplot.pdf",
                         width = 7.5,
                         height = 6.5
                         ){

  result <- ggplot2::ggplot(raw_data, ggplot2::aes(x=x, y=y, colour=colour)) +
    ggplot2::geom_point(shape=p.shape, size=p.size, alpha=p.alpha) +
    # scale_color_continuous(low = "dodgerblue", high = "orange") +
    # scale_fill_continuous(low = "dodgerblue", high = "orange") +
    ggplot2::ggtitle(label.title) +
    ggplot2::xlab(label.xlab) +
    ggplot2::ylab(label.ylab) +
    # labs(colour="Log2\nAbundance") +
    # xlim(0, 13) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.4,
                                    face = "bold",
                                    size = 14),
          panel.grid.major = ggplot2::element_line(colour = "gray", linetype = "dashed"),
          panel.grid.minor = ggplot2::element_line(colour = "gray", linetype = "dashed"),
          axis.text.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_text(size = 10, angle = 90),
          axis.title.x = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 12))

  if (is.plot) {
    ggplot2::ggsave(file.name,
                    result,
                    width = width,
                    height = height)
  }

  return(result)

}
