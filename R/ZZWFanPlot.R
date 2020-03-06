#' @title ZZWFanPlot
#' @author Zhiwei Zhou
#' @param raw_data a data.frame with different format.\n 1st column: character of type/group, it should be unique value; 2nd: numeric value
#' @export
#' @examples
#' type <- c('A','B','C','D','E','F','G')
#' nums <- c(10,23,8,33,12,40,60)
#' raw_data <- data.frame(type = type, nums = nums, stringsAsFactors = F)
#' ZZWFanPlot(raw_data = raw_data)


# type <- c('A','B','C','D','E','F','G')
# nums <- c(10,23,8,33,12,40,60)
# raw_data <- data.frame(type = type, nums = nums, stringsAsFactors = F)

setGeneric(name = 'ZZWFanPlot',
           def = function(
             raw_data
           ){
             temp_data <- raw_data
             colnames(temp_data) <- c('type', 'value')

             if (any(table(temp_data$type) > 1)) {
               stop('Please check the type! some types have replicates\n')
             }

             temp_data <- raw_data %>%
               dplyr::arrange(desc(nums)) %>%
               dplyr::mutate(x_min = seq(from = dplyr::n()+1, to = 2, by = -1),
                             x_max = seq(from = dplyr::n()+2, to = 3, by = -1),
                             y_min = 0,
                             y_max = nums/max(nums)*0.5)

             result_plot <- ggplot2::ggplot(temp_data) +
               ggplot2::geom_rect(ggplot2::aes(fill=type,
                                               ymax=y_max,
                                               ymin=y_min,
                                               xmax=x_max,
                                               xmin=x_min)) +
               ggplot2::xlim(c(0, max(temp_data$x_max))) +
               ggplot2::ylim(0, 1) +
               ggplot2::theme(aspect.ratio=1) +
               ggplot2::coord_polar(theta="y") +
               ggplot2::geom_text(ggplot2::aes(y = (temp_data$y_max + temp_data$y_min)/2,
                                               x = temp_data$x_min+0.5, label = temp_data$nums)) +
               ggplot2::theme_void()

             return(result_plot)

           }
)
