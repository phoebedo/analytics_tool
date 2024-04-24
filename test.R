
data = airquality
variable = "Ozone"
category = "Month"
unique(data[[category]])

      
      
      
fig_hist <- plot_ly(alpha = 0.5)
for( cat in unique(data[[category]])){
  fig_hist <- fig_hist %>% add_histogram(x = cat_data[[variable]])
}


fig_hist <- fig_hist %>% layout(barmode = "stack")

fig_hist




fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = ~rnorm(500))
fig <- fig %>% add_histogram(x = ~rnorm(500) + 1)
fig <- fig %>% layout(barmode = "stack")

fig