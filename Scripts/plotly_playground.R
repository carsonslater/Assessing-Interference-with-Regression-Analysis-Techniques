
# Plotly Playground -------------------------------------------------------

library("plotly")
# summary(mtcars)

fig <- plot_ly(aggregate_data, x = ~input_psd, y = ~transform5, z = ~scaled_psd,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers(alpha = 0.01)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Transmitted PSD'),
                                   yaxis = list(title = 'Transformed Distance'),
                                   zaxis = list(title = 'Recieved PSD')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Miles/(US) gallon',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig
