ply_roi_balloon<- function(data, x_var, y_var, size_var, text_var, color_var,
                           legend_pos,
                           colrs = list(), titles = list()){
  
  # source: https://plotly.com/r/bubble-charts/
  
  data |>
    plot_ly(x = ~.data[[x_var]], y = ~.data[[y_var]], size = ~.data[[size_var]], 
            text = ~.data[[text_var]],
            color = ~.data[[color_var]], colors = colrs$grp1) |>
    add_markers(marker = list(sizemode = "diameter")) |>
    add_text(textposition = "top right",
             textfont = list(
               size = 24,
               color = "black"),
             showlegend = FALSE) |>
    layout(title = list(text = titles$title,
                        font = list(color = "midnightblue")),
           xaxis = list(title = titles$x),
           yaxis = list(title = titles$y),
           legend = list(x = legend_pos[1], y = legend_pos[2],
                         bgcolor = "transparent",
                         bordercolor = "transparent"),
           plot_bgcolor = colrs$bg)
}
