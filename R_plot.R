library(plotly)
library(dplyr)

dt <- read.csv("data_sample.csv", stringsAsFactors = F)
dt$hovertext <- paste0(
  "<b><i>Section: ","</i></b>",dt$section, "<br>",
  "<b><i>States: ","</i></b>",dt$States, "<br>",
  "<b><i>Rink: ","</i></b>", dt$Rink,"<br>",
  "<b><i>Skater: ","</i></b>", dt$Name,"<br>",
  "<b><i>Average Total Score: ","</i></b>",dt$mean_total_score, "<br>")
# order data
dt$Rink <- factor(dt$Rink, levels = c(dt$Rink))


# Define vertical lines

color_fill_line = colorRampPalette(c("#367589","#baf4ff"))(nrow(dt))
color_fill_line = colorRampPalette(c("#0f5970","#42c9e3"))(nrow(dt))

line <- list(
  type = "line",
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in 1: nrow(dt)) {
  line[["x0"]] <- dt[["Rink"]][i]
  line[["x1"]] <- dt[["Rink"]][i]
  line[c("y0", "y1")] <- c(0, dt[["mean_total_score"]][i])
  line[["line"]] = list(color = color_fill_line[i], width = 3,dash = 'dot')
  lines <- c(lines, list(line))
}

p <- plotly::plot_ly(dt,
                     x = ~ Rink,
                     y = ~ mean_total_score,
                     hoverinfo = "text",
                     hovertext = ~ hovertext,
                     type ='scatter',
                     mode ="markers",
                     marker = list(color = color_fill_line, size = 25))
p <- p %>% add_trace(y = ~0.5,mode ="markers",marker = list(color = color_fill_line, size = 10))
plot <- p %>% layout(
  shapes = lines,
  font = list(color = 'gray',family = 'sans serif',size = 12),
  hoverlabel = list(font=list(size=13)),
  showlegend =  FALSE,
  title = list(text ="Top 25 of the US Figure Skate Clubs",
               font = list(
                 family = 'sans serif',
                 size = 22,
                 color = "#0b6d70")),
  margin =list(l=5,r=5,b=10,t=100),
  xaxis = list(
    tickangle = -45,
    tickfont = list(
      family = 'sans serif',
      size = 17,
      color = "#0b6d70"
    ),
    titlefont = list(
      family = 'sans serif',
      size = 20,
      color = "#0b6d70"
    ),
    title = "US Figure Skate Club",
    zeroline = FALSE,
    tickmode = "array",
    color = "#0b6d70"
  ),
  yaxis = list(
    tickfont = list(
      family = 'sans serif',
      size = 17,
      color = "#0b6d70"),
    titlefont = list(
      family = 'sans serif',
      size = 20,
      color = "#0b6d70"),
    title = "Total Scores",
    zeroline = FALSE,
    color = "#0b6d70"),
  legend = "none")

plot
