ply_valid_3D <- function(data, x_var,y_var, z_var, color_var, 
                         colrs = list(), titles = list()){
  data |>
    plot_ly(x = ~.data[[x_var]], y = ~.data[[y_var]], z = ~.data[[z_var]],
            color = ~.data[[color_var]], 
            colors = colrs$prune) |> 
    add_markers(size = 2) |>
    layout(
      title = list(text = titles$title, 
                   font = list(color = "white")),
      scene = list(xaxis = list(title = titles$x,
                                type = 'log',
                                gridcolor = colrs$grid),
                   yaxis = list(title = titles$y,
                                type = 'log',
                                gridcolor = colrs$grid),
                   zaxis = list(title = titles$z,
                                type = 'log',
                                gridcolor = colrs$grid)),
      paper_bgcolor = "#2a2a2b")
}
