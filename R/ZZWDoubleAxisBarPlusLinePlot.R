#' @title ZZWDoubleAxisBarPlusLinePlot
#' @author Zhiwei Zhou
#' @param raw_data data.frame. 1st column: numeric value of x axis; 2nd column: numeric value of prime y axis (left side); 3rd column: numeric value of second y axis (right side).
#' @param ylim_prim the range of prime y axis
#' @param ylim_sec the range of second y axis
#' @param xlab the title of x axis, Default: 'x1'
#' @param ylab_prim the title of prime y axis, Default: 'y1'
#' @param ylab_sec the title of prime y axis, Default: 'y2'
#' @param bar_fill the color of bar
#' @export
#' @examples
#' library(ggplot2)
#' raw_data <- tibble::tibble(
#'   Month = 1:12,
#'   Precip = c(49,36,47,41,53,65,81,89,90,84,73,55),
#'   Temp = c(-4,-4,0,5,11,15,16,15,11,6,1,-3)
#' )
#'
#' ZZWDoubleAxisBarPlusLinePlot(raw_data = raw_data,
#'                              ylim_prim = c(0, 180),
#'                              ylim_sec = c(-4, 18),
#'                              xlab = "Month",
#'                              ylab_prim = "Precipitation",
#'                              ylab_sec = "Temperature") +
#'   scale_x_continuous("Month", breaks = 1:12)


# library(ggplot2)
# raw_data <- tibble::tibble(
#   Month = 1:12,
#   Precip = c(49,36,47,41,53,65,81,89,90,84,73,55),
#   Temp = c(-4,-4,0,5,11,15,16,15,11,6,1,-3)
# )
#
# ZZWDoubleAxisBarPlusLinePlot(raw_data = raw_data,
#                              ylim_prim = c(0, 180),
#                              ylim_sec = c(-4, 18),
#                              xlab = "Month",
#                              ylab_prim = "Precipitation",
#                              ylab_sec = "Temperature") +
#   scale_x_continuous("Month", breaks = 1:12)

setGeneric(name = 'ZZWDoubleAxisBarPlusLinePlot',
           def = function(
             raw_data,
             ylim_prim,
             ylim_sec,
             xlab = 'x1',
             ylab_prim = 'y1',
             ylab_sec = 'y2',
             bar_fill = 'dodgerblue'
           ){
             if (missing(raw_data)) {stop('Please input the raw_data\n')}

             colnames(raw_data)[1:3] <- c('x', 'y1', 'y2')

             if (missing(ylim_prim)) {
               ylim_prim <- c(0.95*min(raw_data$y1), 1.05*max(raw_data$y1))
             }

             if (missing(ylim_sec)) {
               ylim_sec <- c(0.95*min(raw_data$y2), 1.05*max(raw_data$y2))
             }

             # estibalish linear transformation to re-scale second axis
             b <- diff(ylim_prim)/diff(ylim_sec)
             a <- b*(ylim_prim[1] - ylim_sec[1])

             result <- ggplot2::ggplot(raw_data) +
               ggplot2::geom_bar(ggplot2::aes(x = x, y = y1),
                                 stat = 'identity', fill = bar_fill) +
               ggplot2::geom_line(ggplot2::aes(x = x, y = a + y2*b)) +
               ggplot2::geom_point(ggplot2::aes(x = x, y = a + y2*b)) +
               ggplot2::scale_y_continuous(
                 ylab_prim,
                 sec.axis =  ggplot2::sec_axis(~ (. - a)/b,
                                               name = ylab_sec)
               ) +
               ggplot2::xlab(xlab) +
               ZZWtool::ZZWTheme()

             return(result)
           }
)

