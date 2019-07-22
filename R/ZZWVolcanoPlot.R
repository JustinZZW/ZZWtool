#' @title ZZWVolcanoPlot
#' @author Zhiwei Zhou
#' @param raw_data 1st column: feautre name; 2nd column: p_values; 3rd column: fold_changes
#' @param p_cutoff the p cutoff of significant; All p-values would be -log10 transformated in the plot. Default: 0.05
#' @param fc_cutoff the fold change cutoff of significant: Default: 1.25
#' @param log_tran log_tranformation method for fold change, "log2" or "log10". Default: log2
#' @export
#' @examples
#' data('stat_result')
#' ZZWVolcanoPlot(raw_data = stat_result, p_cutoff = 0.05, fc_cutoff = 1.25, log_tran = 'log2')

setGeneric(name = 'ZZWVolcanoPlot',
           def = function(
             raw_data,
             p_cutoff = 0.05,
             fc_cutoff = 1.25,
             log_tran = c('log2', 'log10')
           ){
             log_tran <- match.arg(log_tran)
             colnames(raw_data) <- c('feature', 'p_values', 'fold_changes')

             switch (log_tran,
                     'log2' = {
                       log_p <- -log10(raw_data$p_values)
                       log_fc <- log2(raw_data$fold_changes)
                     },
                     'log10' = {
                       log_p <- -log10(raw_data$p_values)
                       log_fc <- log10(raw_data$fold_changes)
                     }
             )

             volcano_result <- raw_data %>%
               dplyr::mutate(log_p = log_p,
                             log_fc = log_fc) %>%
               dplyr::mutate(is_sig=ifelse(p_values <= p_cutoff &
                                             (fold_changes >= fc_cutoff | fold_changes <= 1/fc_cutoff),
                                           TRUE,
                                           FALSE))

             volcano_plot <- ggplot2::ggplot(data = volcano_result) +
               ggplot2::geom_point(ggplot2::aes(x = log_fc, y = log_p, colour = is_sig)) +
               ggplot2::geom_hline(yintercept = -log10(p_cutoff), linetype=2) +
               ggplot2::geom_vline(xintercept = ifelse(log_tran=='log2', log2(fc_cutoff), log10(fc_cutoff)),
                                   linetype = 2) +
               ggplot2::geom_vline(xintercept = ifelse(log_tran=='log2', -log2(fc_cutoff), -log10(fc_cutoff)),
                                   linetype = 2) +
               ggplot2::scale_color_manual(values = c('TRUE' = 'firebrick1', 'FALSE'='gray')) +
               ggplot2::xlab(paste0(log_tran, '(Fold-change)')) +
               ggplot2::ylab(paste0('-log10(P-value)')) +
               ZZWtool::ZZWTheme()

             return(volcano_plot)

           }
)
