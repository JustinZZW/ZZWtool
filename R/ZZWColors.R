#' @title ZZWcolors
#' @description Some colors for plots
#' @author Zhiwei Zhou
#' {zhouzw@@sioc.ac.cn}
#' @param name The name of color set. Contrast color: "contrast1", "contrast2", "contrast3", "contrast4", "google", "ggplot20". Sequential color: "seq_blue", "seq_red", "seq_green". Default: "contrast1"
#' @export


ZZWcolors <- function(name="contrast1"){
  switch(name,
         "contrast1"={
           color.result <- c("dodgerblue", "firebrick1", "orange", "springgreen2", "orchid", "gray")
         },

         "contrast2"={
           color.result <- c("#F7931D", "#F15A22", "#429EB5", "#006A92")
         },

         "contrast3"={
           color.result <- c("#F8766D", "#D89000", "#A3A500", "#39B600", "#00BF7D", "#00BFC4", "#00B0F6", "#9590FF", "#E76BF3", "#FF62BC")
         },
         "contrast4"={
           # color.result <- c("#503C46", "#502800", "#002D46", "#5F5A19", "#003C32", "#23465A", "#5A3C00", "#000000")
           color.result <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
         },
         "ggplot20"={
           # color.result <- c("#503C46", "#502800", "#002D46", "#5F5A19", "#003C32", "#23465A", "#5A3C00", "#000000")
           color.result <- c("#F8766D", "#EA8331", "#D89000", "#C09B00", "#A3A500", "#7CAE00", "#39B600", "#00BB4E", '#00BF7D', '#00C1A3', '#00BFC4', '#00BAE0', '#00B0F6', '#35A2FF', '#9590FF', '#C77CFF', '#E76BF3', '#FA62DB', '#FF62BC', '#FF6A98')
         },

         "google"={
           c("#4285F4", "#EA4335", "#FBBC05", "#34A853")
         },
         "seq_blue"={
           color.result <- c("#132B43", "#1A3855", "#214667", "#28547A", "#2F638E", "#3772A2", "#3E81B7", "#4691CC", "#4EA1E1", "#56B1F7")
         },
         "seq_red"={
           color.result <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A","#E31A1C", "#BD0026", "#800026")
         },
         "seq_green"={
           color.result <- c("#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850")
         }
         )

  return(color.result)
}
