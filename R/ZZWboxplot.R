#' @title boxplot
#' @description plot boxplot and plot p
#' @author Zhiwei Zhou
#' \email{zhouzw@@sioca.ac.cn}
#' @param raw.data data.frame table
#' @param y the col name of y
#' @param x the col name of x
#' @param file.name pdf file name. Default: "boxplot.pdf"
#' @export
#' @examples
#' test.data <- data.frame(a=seq(1:16), g=rep(c(1,2,3,4), each=4))
#' ZZWboxplot(test.data, y="a", x="g", plot.point=T)

ZZWboxplot <- function(raw.data,
                       y,
                       x,
                       main="",
                       file.name=NULL,
                       plot.point=T,
                       is.output=T,
                       figure="pdf"){

  # pdf(file = paste(paste(rownames(lasso.data.identified)[i], "pdf", sep = "."), sep = "/") , width = 6, height = 6)

  y <- raw.data[,which(colnames(raw.data)==y)]
  x <- raw.data[,which(colnames(raw.data)==x)]

  if (is.null(file.name)) {
    file.name <- "boxplot.pdf"
  } else {
    file.name <- paste(file.name, "pdf", sep = ".")
  }

  if (is.output) {
    pdf(file = file.name, width = 6, height = 6)
  }

  boxplot(y ~ x,
          data = raw.data,
          main = main,
          cex.lab = 1.8,
          cex.axis = 1.5,
          # col = ZZWcolors()[seq(unique(x))],
          add = FALSE,
          outline = FALSE)

  if (plot.point==T) {
    beeswarm::beeswarm(y ~ x,
                       data = raw.data,
                       labels = F,
                       add = TRUE,
                       col = ZZWcolors()[seq(unique(x))],
                       pch = 19)
  }

  if (is.output) {
    dev.off()
  }

}

