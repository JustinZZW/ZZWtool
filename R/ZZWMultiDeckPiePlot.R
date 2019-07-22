#' @title ZZWMultiDeckPiePlot
#' @author Zhiwei Zhou
#' @param raw_data a data.frame with different format.\n "single": 1st column: group1, 2nd column: value;  "double": 1st column: group1, 2nd column: group2, 3rd column value; "triple": 1st column: group1, 2nd column: group2, 3rd column: group4, 3rd column: value
#' @param mode "single", "double", "triple". Default: "single"
#' @export
#' @examples
#' type <- c('A','B','C','D','E','F','G')
#' type2 <- c(rep('AA',4), rep('BB', 3))
#' type3 <- c(rep('AAA', 7))
#' nums <- c(10,23,8,33,12,40,60)
#' # single mode
#' raw_data <- data.frame(type = type, nums = nums, stringsAsFactors = F)
#' # double mode
#' raw_data <- data.frame(type = type, type2=type2,  nums = nums, stringsAsFactors = F)
#' # triple mode
#' raw_data <- data.frame(type = type, type2=type2, type3=type3, nums = nums, stringsAsFactors = F)

setGeneric(name = 'ZZWMultiDeckPiePlot',
           def = function(
             raw_data,
             mode=c('single', 'double', 'triple')
           ){
             temp_data <- raw_data
             mode <- match.arg(mode)

             switch (mode,
               'single' = {
                 if (ncol(temp_data)!=2) {
                   stop('Please check the format. (1st column: group1, 2nd column: value)')
                 }

                 colnames(temp_data) <- c('group1', 'value')

                 temp_data$fraction <- temp_data$value/sum(temp_data$value)
                 temp_data$ymax <- cumsum(temp_data$fraction)
                 temp_data$ymin <- c(0, head(temp_data$ymax, n = -1))

                 label_value <- round(temp_data$fraction*100, 1)
                 label <- paste0(temp_data$value, " (", label_value, '%)')

                 pie_plot <- ggplot2::ggplot(temp_data) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group1,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=3,
                                 xmin=2)) +
                   ggplot2::xlim(c(0, 3)) +
                   ggplot2::theme(aspect.ratio=1) +
                   ggplot2::coord_polar(theta="y") +
                   ggplot2::theme_void() +
                   ggplot2::geom_text(ggplot2::aes(y = temp_data$fraction/2 + temp_data$ymin,
                                 x = 2.5, label = label))

                 return(pie_plot)
               },

               'double' = {
                 if (ncol(temp_data)!=3) {
                   stop('Please check the format. (1st column: group1, 2nd column: group2, 3rd column value)')
                 }

                 colnames(temp_data) <- c('group1', 'group2', 'value')

                 temp_data$fraction <- temp_data$value/sum(temp_data$value)
                 temp_data$ymax <- cumsum(temp_data$fraction)
                 temp_data$ymin <- c(0, head(temp_data$ymax, n = -1))

                 label_value <- round(temp_data$fraction*100, 1)
                 label <- paste0(temp_data$value, " (", label_value, '%)')

                 pie_plot <- ggplot2::ggplot(temp_data) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group2,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=4,
                                 xmin=3)) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group1,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=3,
                                 xmin=2)) +
                   ggplot2::xlim(c(0, 4)) +
                   ggplot2::theme(aspect.ratio=1) +
                   ggplot2::coord_polar(theta="y") +
                   ggplot2::geom_text(ggplot2::aes(y = temp_data$fraction/2 + temp_data$ymin,
                                 x = 2.5, label = label)) +
                   ggplot2::theme_void()

                 return(pie_plot)

               },


               'triple' = {
                 if (ncol(temp_data)!=4) {
                   stop('Please check the format. (1st column: group1, 2nd column: group2, 3rd column: group4, 3rd column: value)')
                 }

                 colnames(temp_data) <- c('group1', 'group2', 'group3', 'value')

                 temp_data$fraction <- temp_data$value/sum(temp_data$value)
                 temp_data$ymax <- cumsum(temp_data$fraction)
                 temp_data$ymin <- c(0, head(temp_data$ymax, n = -1))

                 label_value <- round(temp_data$fraction*100, 1)
                 label <- paste0(temp_data$value, " (", label_value, '%)')

                 pie_plot <- ggplot2::ggplot(temp_data) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group3,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=5,
                                 xmin=4)) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group2,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=4,
                                 xmin=3)) +
                   ggplot2::geom_rect(ggplot2::aes(fill=group1,
                                 ymax=ymax,
                                 ymin=ymin,
                                 xmax=3,
                                 xmin=2)) +
                   ggplot2::xlim(c(0, 5)) +
                   ggplot2::theme(aspect.ratio=1) +
                   ggplot2::coord_polar(theta="y") +
                   ggplot2::geom_text(ggplot2::aes(y = temp_data$fraction/2 + temp_data$ymin,
                                 x = 2.5, label = label)) +
                   ggplot2::theme_void()

                 return(pie_plot)

               }
             )


           }
)
#
# temp_data <- superclass_compare_table %>%
#   filter(superclass=='Benzenoids')
#
#
# label_value <- round(temp_data$value/sum(temp_data$value)*100, 1)
# label <- paste0(temp_data$value, " (", label_value, '%)')
#
# ggplot(temp_data) +
#   geom_rect(aes(fill=variable,
#                 ymax=ymax,
#                 ymin=ymin,
#                 xmax=3,
#                 xmin=2)) +
#   # scale_fill_manual(values = c(ZZWtool::ZZWcolors('seq_blue')[seq(1,5, by = 2)],
#   #                              ZZWtool::ZZWcolors('seq_red')[seq(1,7, by = 2)],
#   #                              'dodgerblue', 'orange')) +
#   xlim(c(0, 3)) +
#   theme(aspect.ratio=1) +
#   coord_polar(theta="y") +
#   theme_void()
