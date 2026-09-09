library(ggplot2)
library(ggpubr)

# reusable function
box_jitter_plot <- function(data,
                            x_var,
                            y_var,
                            group1,
                            group2,
                            x_label,
                            y_label,
                            title_text,
                            jitter_color = "#CC79A7",
                            label_y = NULL) {
  
  # automatic label position if not provided
  if (is.null(label_y)) {
    label_y <- max(data[[y_var]], na.rm = TRUE) * 1.05
  }
  
  ggplot(data,
         aes_string(x = x_var,
                    y = y_var)) +
    
    geom_boxplot(
      width = 0.5,
      outlier.shape = NA
    ) +
    
    geom_jitter(
      width = 0.15,
      size = 2,
      alpha = 0.7,
      colour = jitter_color
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = scales::breaks_pretty(n = 10)
    )+
    stat_compare_means(
      comparisons = list(c(group1, group2)),
      method = "wilcox.test",
      label = "p.format",
      label.y = label_y,
      bracket.size = 1,
      tip.length = 0.02,
      size = 4
    ) +
    
    labs(
      x = x_label,
      y = y_label,
      title = title_text
    ) +
    
    theme_classic() +
    
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 1,
        size = 12,
        face = "bold"
      ),
      axis.text.y = element_text(
        size = 12,
        face = "bold"
      ),
      axis.title = element_text(
        size = 15,
        face = "bold"
      ),
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        lineheight = 1.2
      ),
      axis.line = element_line(linewidth = 1),
      legend.position = "none"
    )
}