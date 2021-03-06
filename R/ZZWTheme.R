#' @title ZZWTheme
#' @author Zhiwei Zhou
#' \email{zhouzw@@sioc.ac.cn}
#' @description ggplot theme
#' @param type: 'common', 'classic'
#' @export
#' @examples
#' mydata <- data.frame(
#'  Lebal  = c("linerange1","linerange2","linerange3","linerange4","linerange5"),
#'  xstart = c(3.5,7,12,16,20),
#'  ymin   = c(2.5,6.5,3,4.5,3.8),
#'  ymax   = c(7.5,9.5,9,13.5,4.2),
#'  class  = c("A","A","A","C","C")
#' )
#' test <- ggplot(mydata) +
#'  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5)
#' test
#' mytheme <- ZZWTheme()
#' test + mytheme
#' mytheme2 <- ZZWTheme('classic')
#' test +
#'   mytheme2 +
#'   theme(axis.text.y = ggplot2::element_text(size = 10, angle = 0,
#'                                             colour = 'black'))



# mydata <- data.frame(
#   Lebal  = c("linerange1","linerange2","linerange3","linerange4","linerange5"),
#   xstart = c(3.5,7,12,16,20),
#   ymin   = c(2.5,6.5,3,4.5,3.8),   ymax   = c(7.5,9.5,9,13.5,4.2),
#   class  = c("A","A","A","C","C")
# )
# test <- ggplot(mydata) +
#   geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5)
# test
# mytheme <- ZZWTheme()
# test + mytheme
#
# mydata <- data.frame(
#   Lebal  = c("linerange1","linerange2","linerange3","linerange4","linerange5"),
#   xstart = c(3.5,7,12,16,20),
#   ymin   = c(2.5,6.5,3,4.5,3.8),   ymax   = c(7.5,9.5,9,13.5,4.2),
#   class  = c("A","A","A","C","C")
# )
# test <- ggplot(mydata) +
#   geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5)
# test
# mytheme1 <- ZZWTheme()
# test + mytheme1
# mytheme2 <- ZZWTheme('classic')
# test +
#   mytheme2 +
#   theme(axis.text.y = ggplot2::element_text(size = 10, angle = 0,
#                                             colour = 'black'))

setGeneric(name = 'ZZWTheme',
           def = function(
             type = c('common', 'classic')
           ){

             type <- match.arg(type)
             switch (type,
                     'common' = {
                       result <- ggplot2::theme_bw() +
                         ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.4,
                                                                           face = "bold",
                                                                           size = 14),
                                        panel.grid.major = ggplot2::element_blank(),
                                        panel.grid.minor = ggplot2::element_blank(),
                                        panel.background = ggplot2::element_blank(),
                                        panel.border = ggplot2::element_rect(fill = NA, colour = 'black'),
                                        legend.background = ggplot2::element_blank(),
                                        axis.text.x = ggplot2::element_text(size = 10, colour = 'black'),
                                        axis.text.y = ggplot2::element_text(size = 10, angle = 90,
                                                                            colour = 'black'),
                                        axis.title.x = ggplot2::element_text(size = 12, colour = 'black'),
                                        axis.title.y = ggplot2::element_text(size = 12, colour = 'black'),
                                        axis.ticks = ggplot2::element_line(colour = 'black'))
                     },
                     'classic' = {
                       result <- ggplot2::theme_classic() +
                         ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.4,
                                                                           face = "bold",
                                                                           size = 14),
                                        panel.grid.major = ggplot2::element_blank(),
                                        panel.grid.minor = ggplot2::element_blank(),
                                        panel.background = ggplot2::element_rect(fill = NA, colour = NA),
                                        # panel.border = ggplot2::element_rect(fill = NA, colour = 'black'),
                                        legend.background = ggplot2::element_blank(),
                                        axis.line = ggplot2::element_line(colour = 'black'),
                                        axis.text.x = ggplot2::element_text(size = 10, colour = 'black'),
                                        axis.text.y = ggplot2::element_text(size = 10, angle = 90,
                                                                            colour = 'black'),
                                        axis.title.x = ggplot2::element_text(size = 12, colour = 'black'),
                                        axis.title.y = ggplot2::element_text(size = 12, colour = 'black'),
                                        axis.ticks = ggplot2::element_line(colour = 'black'))
                     }
             )



             return(result)
           }
)

