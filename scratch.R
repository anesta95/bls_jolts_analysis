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
plt <- ggplot(sample_df_viz, mapping = aes(x = date, y = value, color = date_measure_text)) +
  coord_cartesian(
    xlim = c(min(sample_df_viz$date), max(sample_df_viz$date)),
    clip = "off") +
  geom_line(aes(linewidth = date_measure_text),
            lineend = "round",
            linejoin = "bevel") +
  scale_x_date(date_labels = "%b. '%y") +
  scale_size_manual(c("cur" = 0.8, "cur_trail_3" = 4))

make_ts_line_chart(
  viz_df = x,
  x_col = date,
  y_col_one = value_trail_three,
  second_y_col = T,
  y_col_two = value,
  rec_avg_line = rec_avg,
  non_rec_avg_line = non_rec_avg,
  y_data_type = "number",
  viz_subtitle = "<b style=\"color: #a6cee3\">Latest</b> and <b style = \"color: #1f78b4\">3 month trailing average</b>",
  viz_caption = paste("Average lines for data since Dec. '00.",
                      base_viz_caption)
)




