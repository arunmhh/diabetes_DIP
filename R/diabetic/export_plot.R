# R/export_plots.R

save_plots <- function(
    plots,
    names,
    dataset_name = "plots",
    base_dir     = "output/figure/diabetes/plots_all_labvalues",
    subfolder    = NULL,
    width        = 8,
    height       = 6,
    dpi          = 600,
    formats      = c("svg", "png")
) {
  
  library(here)
  library(svglite)   # ensures high-quality SVG rendering
  
  # build full path from project root
  if (!is.null(subfolder)) {
    base_path <- here::here(base_dir, subfolder)
  } else {
    base_path <- here::here(base_dir)
  }
  
  for (fmt in formats) {
    
    dir_path <- file.path(base_path, paste0(dataset_name, "_plot_", fmt))
    
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    for (i in seq_along(plots)) {
      
      file_name <- paste0(names[i], ".", fmt)
      file_path <- file.path(dir_path, file_name)
      
      ggplot2::ggsave(
        filename = file_path,
        plot     = plots[[i]],
        width    = width,
        height   = height,
        dpi      = dpi,
        device   = fmt          # ← explicitly set per format
      )
    }
    
    message("✔ Saved ", length(plots), " ", toupper(fmt), " plots to: ", dir_path)
  }
}