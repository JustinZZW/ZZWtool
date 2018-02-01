#' @title ZZWpieplot
#' @author Zhiwei Zhou
#' \email{zhouzw@@sioc.ac.cn}
#' @description plot pie plot
#' @param raw.data data for plot
#' @param colors please refer \code{\link{ZZWcolors}}
#' @param file.name file name. Default: "Pie plot"
#' @param is.output Whether output? Default: TRUE
#' @export
#' @examples
#' raw.data <- c(1,4,5)
#' names(raw.data) <- c("group_1", "group_2", "group_3")
#' ZZWpieplot(raw.data=raw.data, file.name="Pie plot", is.output=F)


ZZWpieplot <- function(raw.data,
                       color="contrast1",
                       file.name="Pie plot",
                       is.output=TRUE){

  color <- ZZWtool::ZZWcolors(name = color)[1:length(raw.data)]

  if (is.output==TRUE) {
    pdf(file = file.name, width = 6, height = 6)
    pie(raw.data,
        edges = 200,
        angle = 45,
        init.angle = 90,
        border = "white",
        lwd=3,
        col = color,
        radius = 1)
    dev.off()
  } else {
    pie(raw.data,
        edges = 200,
        angle = 45,
        init.angle = 90,
        border = "white",
        lwd=3,
        col = color,
        radius = 1)
  }
}


