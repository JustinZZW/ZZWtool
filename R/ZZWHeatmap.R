#' @title ZZWHeatmap
#' @author Zhiwei Zhou
#' @param raw_data
#' @param group_info

setGeneric(name = 'ZZWHeatmap',
           def = function(raw_data,
                          group_info,
                          scale = "row",
                          show_rownames = TRUE,
                          show_colnames = TRUE,
                          border_color = NA,
                          fontsize_row = 10,
                          cluster_rows = TRUE,
                          cluster_cols = TRUE,
                          clustering_method = "ward.D"){
             pheatmap::pheatmap(
               raw_data,
               color = colorRampPalette(c("navy", "white", "firebrick3"))(500),
               scale = "row",
               show_rownames = TRUE,
               show_colnames = TRUE,
               border_color = NA,
               annotation_col = group_info,
               # annotation_row = annotation_row,
               fontsize_row = 10,
               cluster_rows = TRUE,
               cluster_cols = TRUE,
               clustering_method = "ward.D"
             )
           })
