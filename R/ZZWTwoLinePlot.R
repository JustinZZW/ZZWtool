#' @title ZZWTwoLinePlot
#' @author Zhiwei Zhou
#' @param data1 data set1; It requires to contain 2 columns, x1 and y1
#' @param data2 data set2; It requires to contain 2 columns, x2 and y2
#' @param x1
#' @param y1
#' @param x2
#' @param y2
#' @param sec_axis_scale Rescale sec_axis range, it need to fit with major axis. Default: c(0, 1800)
#' @param xlab Character.
#' @param ylab1 Character.
#' @param ylab2 Character.
#' @export
#' @example
#' data1 <- data.frame(x1=1:10, y1=seq(1, 100, 10), stringsAsFactors = F)
#' data2 <- data.frame(x2=1:10, y2=seq(2000, 200, -200), stringsAsFactors = F)
#' ZZWTwoLinePlot(data1 = data1, data2 = data2, sec_axis_scale = c(0, 100))


setGeneric(name = 'ZZWTwoLinePlot',
           def = function(
             data1,
             data2,
             x1,
             y1,
             x2,
             y2,
             sec_axis_scale = c(0, 1800),
             xlab = "x",
             ylab1 = 'y1',
             ylab2 = "y2"
           ){
             raw_sec_axis_range <- range(data2$y2)

             plot_result <- ggplot2::ggplot() +
               ggplot2::geom_line(ggplot2::aes(x = x1, y = y1),
                                  data = data1,
                                  colour='dodgerblue',
                                  size=1) +
               ggplot2::geom_point(ggplot2::aes(x = x1, y = y1),
                                   data = data1,
                                   shape=19,
                                   colour='dodgerblue',
                                   size=3) +
               ggplot2::geom_line(ggplot2::aes(x = x2,
                                               y = scales::rescale(y2, sec_axis_scale)),
                                  data = data2,
                                  colour='firebrick1',
                                  size=1) +
               ggplot2::geom_point(ggplot2::aes(x = x2,
                                                y = scales::rescale(y2, sec_axis_scale)),
                                   data = data2,
                                   shape=19,
                                   colour='firebrick1',
                                   size=3) +
               ggplot2::scale_y_continuous(
                 # breaks=scales::pretty_breaks(10),
                                           sec.axis = ggplot2::sec_axis(trans = ~scales::rescale(.,
                                                                                                 raw_sec_axis_range),

                                                                        # rescale the sec_axis to raw range

                                                                        # breaks = 10,
                                                                        # labels = seq(0, 2000, 200),
                                                                        name = ylab2)) +
               # scale_x_reverse(breaks=10:1,
               #                 labels=10:1) +
               ggplot2::xlab(xlab) +
               ggplot2::ylab(ylab1) +
               ZZWTheme()

             return(plot_result)

           }
)
