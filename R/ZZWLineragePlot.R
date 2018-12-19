#' @title ZZWLineragePlot
#' @author Zhiwei Zhou
#' \email{zhouzw@@sioc.ac.cn}
#' @param raw.data it should contain 4 columns, "x", "ymin", "ymax", "label"
#' @param x
#' @param ymin
#' @param ymax
#' @param colour Default: 'dodgerblue'
#' @param label.y Default: 'y'
#' @param label.x Default: 'x'
#' @param is.output Default: FALSE
#' @export
#' @examples
#' mydata <- data.frame(
#'   Lebal  = c("linerange1","linerange2","linerange3","linerange4","linerange5"),
#'   x = c(3.5,7,12,16,20),
#'   ymin   = c(2.5,6.5,3,4.5,3.8),
#'   ymax   = c(7.5,9.5,9,13.5,4.2),
#'   label  = c("A","A","A","C","C")
#' )
#'
#' ZZWLineragePlot(mydata)

setGeneric(name = 'ZZWLineragePlot',
           def = function(
             raw.data,
             x,
             ymin,
             ymax,
             colour = 'dodgerblue',
             label.y = 'y',
             label.x = 'x',
             is.output = FALSE,
             file.name = 'LineragePlot.pdf',
             width = 8,
             height = 6
           ){
             linerage_plot <- ggplot2::ggplot(raw.data) +
               ggplot2::geom_linerange(ggplot2::aes(x = x,
                                                    ymin = ymin,
                                                    ymax = ymax),
                                       size = 1.5,
                                       colour = 'dodgerblue') +
               ggplot2::coord_flip() +
               ggplot2::ylab('m/z') +
               ggplot2::xlab('Superclass') +
               ggplot2::scale_x_continuous(breaks = raw.data$x,
                                           labels = raw.data$label) +
               # scale_y_continuous(breaks = seq(0, 3600, by = 600),
               #                    labels = seq(0, 3600, by = 600)) +
               ZZWTheme() +
               ggplot2::theme(axis.text.y = ggplot2::element_text(angle=0))
             # geom_hline(yintercept = 200, linetype="dashed") +
             # geom_hline(yintercept = 900, linetype="dashed")

             if (is.output) {
               ggplot2::ggsave(filename = file.name,
                               plot = linerage_plot,
                               width = width,
                               height = height)
             }

             return(linerage_plot)
           }
)

