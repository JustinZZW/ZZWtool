#' @title boxplot
#' @description plot boxplot and plot p
#' @author Justin Zhou
#' @param x,y numeric vector
#' @export

ZZWboxplot <- function(x, y, lable.group, plot.point=T, figure="jepg", main.name){

  # pdf(file = paste(paste(rownames(lasso.data.identified)[i], "pdf", sep = "."), sep = "/") , width = 6, height = 6)

  boxplot(as.numeric(x), as.numeric(y),
          cex.lab = 1.8, cex.axis = 1.5,
          col = c("lightseagreen", "salmon"),
          # col = mycol,
          # border = c("lightseagreen", "salmon"),
          main = main.name,
          cex.main = 1.8, names = lable.group, add = FALSE, outline = FALSE)

  if (plot.point==T) {
    beeswarm::beeswarm(x = list(as.numeric(x), as.numeric(y)),
                       labels = F,add = TRUE, col = c("seagreen1", "brown"),
                       pch = 19)
  }

}



