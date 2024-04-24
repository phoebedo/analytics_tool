
data = airquality
variable = "Ozone"
category = "Month"
      
      
      
fig_hist <- plot_ly(alpha = 0.2)
for( cat in unique(data[[category]])){
  cat_data <- filter(data, data[[category]]==cat)
  fig_hist <- fig_hist %>% add_histogram(x = cat_data[[variable]], name= cat)
}


fig_hist <- fig_hist %>% layout(barmode = "stack",bargap=0.05)

fig_hist









