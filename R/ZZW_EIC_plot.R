#' @title ZZW_EIC_plot
#' @description EIC plot
#' @author Justin Zhou
#' @param raw.data The 2 column data.frame. The column_1 represents time, and column_2 represents counts.
#' @param pic.name The export picture name. The default is NULL.
#' @param rt.range The retention time range.
#' @param color The color of line. Default is "dodgerblue".
#' @export

ZZW_EIC_plot <- function(raw.data,
                         pic.name=NULL,
                         rt.range=NULL,
                         color="dodgerblue"){
  colnames(raw.data) <- c("Time", "Counts")

  if (is.null(rt.range)) {
    rt.range <- c(0, max(as.numeric(raw.data$Time)))
  }

  if (is.null(pic.name)) {
    pic.name <- c("EIC.pdf")
  } else {
    pic.name <- paste(pic.name, ".pdf")
  }

  max.count <- max(as.numeric(raw.data$Counts))

  pdf(file = pic.name, width = 6, height = 4)

  plot(as.numeric(raw.data$Time),
       as.numeric(raw.data$Counts)/max.count, type = "l", lwd=2, xlab = "Retention time (s)",
       ylab = "Counts",
       main = "Extract Ion Chromatography",
       col=color,
       bty="n",
       xlim = rt.range,
       axes = F)

  axis(side = 1, at=seq(round(rt.range[1], digits = 0), round(rt.range[2], digits = 0), length.out = 5), labels = seq(round(rt.range[1], digits = 0), round(rt.range[2], digits = 0), length.out = 5))

  axis(side = 2, at=c(0,0.5,1), labels = c(0, 0.5, 1))

  box()

  dev.off()

}
