#' @title ZZW_MSMS_plot
#' @description Plot MS/MS match plot
#' @author Zhiwei Zhou
#' @param exp.spec experimental spectrum: first column: mz; second column:intensity
#' @param std.spec standard spectrum: first column: mz; second column:intensity
#' @param is.output whether export picture. Default: False
#' @param file.name Picture name. Default: "MSMS"
#' @param dir.path output path. Default: current directory.
#' @param pg.format Output picture format, including "pdf", "png". Default: "pdf"
#' @examples
#' exp.spec <- data.frame(mz=c(78, 96, 107, 134, 158), int=c(1338,74,74,522,98))
#' std.spec <- data.frame(mz=c(78, 96, 107, 134, 158), int=c(3845,169,258,610,516))
#' ZZW_MSMS_plot(exp.spec, std.spec, is.output=FALSE)

ZZW_MSMS_plot <- function(exp.spec,
                          std.spec,
                          is.output=FALSE,
                          file.name=NULL,
                          dir.path=NULL,
                          pg.format="pdf"){

  temp.exp.spec <- as.data.frame(exp.spec)
  temp.std.spec <- as.data.frame(std.spec)
  colnames(temp.exp.spec) <- colnames(temp.std.spec) <- c("mz", "intensity")

  exp.max.int <- max(as.numeric(temp.exp.spec$intensity))
  std.max.int <- max(as.numeric(temp.std.spec$intensity))
  temp.exp.spec$intensity <- temp.exp.spec$intensity/exp.max.int
  temp.std.spec$intensity <- temp.std.spec$intensity/std.max.int

  mz <- c(temp.exp.spec$mz, temp.std.spec$mz)
  int <- c(temp.exp.spec$intensity, -temp.std.spec$intensity)

  color <- c(rep("dodgerblue", length(c(temp.exp.spec$mz))), rep("firebrick1", length(c(temp.std.spec$mz))))
  # color[infor.annotate=="precursor"] <- "gray"

  x.range <- c(0.95*min(as.numeric(mz)), 1.05*max(as.numeric(mz)))


  if (is.null(file.name)) file.name <- "MSMS"
  if (is.null(dir.path)) dir.path <- "."

  if (is.output) {
    switch(pg.format,
           "png"={
             file.name <- paste(file.name,".png", sep = "")
             file.name <- file.path(dir.path, file.name)
             png(filename = file.name, width = 800, height = 400)
           },
           "pdf"={
             file.name <- paste(file.name,".pdf", sep = "")
             file.name <- file.path(dir.path, file.name)
             pdf(file=file.name, width = 8, height = 4)
           })
  }

  par(mar=c(3, 3, 2, 2), oma=c(0.5, 0.5, 0.5, 0.25))

  plot(mz, int,
       type = "h",
       lwd = 1.5,
       xlim = x.range,
       ylim = c(-1.1, 1.1),
       col = color,
       xlab = "m/z",
       ylab = "Relative intensity",
       # main = information[1],
       mgp = c(1.8, 0.5, 0),
       cex.main = 1,
       cex.lab = 1,
       cex.axis = 0.8)

  abline(h=0, lwd=1)

  # if (!is.null(exp.spec.annotate)) {
  #   frag.annotate <- c(exp.spec.annotate, std.spec.annotate)
  #   points(x = mz[frag.annotate=="match"],
  #          y = int[frag.annotate=="match"],
  #          pch = 19,
  #          cex = 0.7,
  #          # col = "black",
  #          col = c(rep("dodgerblue", sum(exp.spec.annotate=="match")),
  #                  rep("firebrick1", sum(std.spec.annotate=="match"))))
  # }
  #
  # # frag annotation
  # text(x = (temp.std.spec$mz)+3.5,
  #      y = -(temp.std.spec$intensity)-0.03,
  #      labels = row.names(temp.std.spec),
  #      srt=0, cex = 0.6)

  # score

  # text(x = min(as.numeric(mz))*0.75,
  #      y = -1,
  #      cex = 0.6,
  #      pos = 4,
  #      labels = paste("Score:", information[2]))
  #
  # text(x = min(as.numeric(mz))*0.75,
  #      y = -1.1,
  #      cex = 0.6,
  #      pos = 4,
  #      labels = paste("Name:", information[1]))

  legend("topleft",
         legend = c("Matched peaks: experiment", "Matched peaks: library"),
         # pch = 19,
         fill = c("dodgerblue", "firebrick1"),
         bty="n", cex = 0.6)

  if (is.output){
    dev.off()
  }

  # legend("bottomleft",
  #        legend = c(paste("Score:", information[2]),
  #                   paste("Name:", information[1])),
  #        bty="n",
  #        cex = 0.6)

}
