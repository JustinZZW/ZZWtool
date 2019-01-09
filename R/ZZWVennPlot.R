#' @title ZZWVennPlot
#' @author Zhiwei Zhou
#' @param raw_data a list with group name. The group number needs less than 5.
#' @param height Default: 6
#' @param colour_type Please ref ZZWColors
#' @param weight Default: 6
#' @param filename Default: test
#' @export
#' @examples
#' test <- list(A=1:10,B=3:8,C=6:9)
#' ZZWVennPlot(raw_data=test)

setGeneric('ZZWVennPlot',
           def = function(raw_data,
                          height=6,
                          width=6,
                          colour_type='contrast1',
                          filename='ZZWVennPlot.pdf'){

             if (class(raw_data)!='list') {
               stop('The input data need to be a list with group name.')
             }

             if (length(raw_data) > 5) {
               stop('The maxmium groups of venn plot is 5, please narrow down the groups.')
             }

             fill_color <- ZZWcolors(colour_type)[seq(length(raw_data))]

             venn_result <- VennDiagram::venn.diagram(# list(A=1:10,B=3:8,C=6:9),
                                                      raw_data,
                                                      fill=fill_color,
                                                      alpha=rep(0.5, length(raw_data)),
                                                      cex=2,
                                                      cat.fontface=4,
                                                      lty = 1,
                                                      # fontfamily='3',
                                                      filename=NULL)
             # library(grDevices)
             grDevices::pdf(file=filename, width = width, height = height)
             grid::grid.draw(venn_result)
             dev.off()
           }
)
