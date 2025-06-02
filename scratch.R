########## SCRATCH CODE ##########
sample_df <- hi_jo_ld_qu_uo_ts_df_list[[1]]

sample_df_viz <- sample_df %>% 
  rename(geo_entity_text = region_text) %>% 
  mutate(date_period_text = "monthly",
         geo_entity_type_text = "nation",
         geo_entity_text = "US") %>% 
  pivot_longer(cols = starts_with("value"), names_to = "date_measure_text", values_to = "value") %>% 
  mutate(date_measure_text = if_else(date_measure_text == "value", "cur", "cur_trail_3"))


non_rec_avg <- hi_jo_ld_qu_uo_avg_list[[1]][1]
rec_avg <- hi_jo_ld_qu_uo_avg_list[[1]][2]

## I need to figure out how to customize line specifics when they are 
## mapped to two different variables

## scale_size_manual()/scale_size_identity()?

sample_df_viz 
  
make_ts_line_chart_revamp(
  viz_df = sample_df_viz,
  x_col = date,
  y_col = value,
  rec_avg_line = rec_avg,
  non_rec_avg_line = non_rec_avg,
  y_data_type = "number",
  viz_subtitle = "<b style=\"color: #a6cee3\">Latest</b> and <b style = \"color: #1f78b4\">3 month trailing average</b>",
  viz_caption = paste("Average lines for data since Dec. '00.",
                      base_viz_caption)
)


make_ts_line_chart_revamp <- function(viz_df, x_col, y_col, rec_avg_line = NULL, 
                               non_rec_avg_line = NULL, y_data_type,
                               viz_title = NULL, viz_subtitle, viz_caption) {
  # TEMPORARY: Creating ad-hoc columns for line size and color
  viz_df <- viz_df %>% 
    mutate(linewidth_sizes = if_else(date_measure_text == "cur", 0.8, 2.75),
           line_colors = if_else(date_measure_text == "cur", "#a6cee3", "#1f78b4"))
  
  # https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
  # Quoting X and Y variables:
  x_col_quo <- enquo(x_col)
  y_col_quo <- enquo(y_col)
  
  viz_title <- make_chart_title(viz_df, viz_title)
  
  # Getting data range to use for annotation calculations
  data_range <- get_data_range(pull(viz_df, !!y_col_quo))
  
  latest_date_dte <- max(viz_df$date, na.rm = T)
  earliest_date_dte <- min(viz_df$date, na.rm = T)
  # Getting dashed recession/non-recession average line offset
  num_data_dte_range_diff <- diff(as.numeric(range(viz_df$date, na.rm = T)))
  x_ann <- get_x_annotation_val(num_data_dte_range_diff, latest_date_dte)
  
  latest_date_str <- format(latest_date_dte, "%b. '%y")
  
  # Creating final viz caption
  viz_caption_full <- str_replace(viz_caption, "MMM. 'YY", latest_date_str)
  
  
  # Base plt
  plt <- ggplot(viz_df, mapping = aes(x = !!x_col_quo, y = !!y_col_quo)) +
    coord_cartesian(
      xlim = c(earliest_date_dte, latest_date_dte),
      clip = "off") +
    geom_line(mapping = aes(linewidth = linewidth_sizes, color = line_colors),
              lineend = "round",
              linejoin = "bevel") +
    scale_x_date(date_labels = "%b. '%y") +
    scale_size_identity() +
    scale_color_identity() +
    guides(
      color = F,
      linewidth = F
    ) +
    labs(
      title = viz_title,
      subtitle = viz_subtitle,
      caption = viz_caption_full
    ) +
    ts_line_theme()
  
  # # Adding in second smaller line if specified
  # if (second_y_col) {
  #   y_col_two_quo <- enquo(y_col_two)
  #   # Replacing the data range with the more volatile mom annualized column
  #   second_line_data_range <- get_data_range(pull(viz_df, !!y_col_two_quo))
  #   data_range <- range(data_range, second_line_data_range)
  #   plt <- plt + geom_line(mapping = aes(y = !!y_col_two_quo),
  #                          linewidth = 0.8,
  #                          color = "#a6cee3", 
  #                          lineend = "round",
  #                          linejoin = "bevel")
  #   
  # }
  
  if (!is.null(non_rec_avg_line)) {
    if (between(non_rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = non_rec_avg_line,
                              color = "black",
                              linewidth = 0.75,
                              linetype = "dashed"
      ) + annotate("text",
                   x = x_ann,
                   y = non_rec_avg_line,
                   hjust = 0.5,
                   label = "Non-recession\navg.",
                   color = "black",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (!is.null(rec_avg_line)) {
    if (between(rec_avg_line, data_range[1], data_range[2])) {
      plt <- plt + geom_hline(yintercept = rec_avg_line,
                              color = "red",
                              linewidth = 0.75,
                              linetype = "dashed"
      ) + annotate("text",
                   x = x_ann,
                   y = rec_avg_line,
                   hjust = 0.5,
                   label = "Recession\navg.",
                   color = "red",
                   size = 3.5,
                   fontface = "bold")
    }
  }
  
  if (y_data_type == "percentage") {
    plt <- plt + scale_y_continuous(labels = label_percent(scale = 100, 
                                                           suffix = "%", 
                                                           accuracy = 0.1))
  } else if (y_data_type == "dollar") {
    plt <- plt + scale_y_continuous(labels = label_currency(scale = 1, 
                                                            prefix = "$", 
                                                            scale_cut = cut_short_scale()))
  } else if (y_data_type == "number") {
    plt <- plt + scale_y_continuous(labels = label_number(scale = 1, 
                                                          scale_cut = cut_short_scale()))
  }
  
  return(plt)
  
}

