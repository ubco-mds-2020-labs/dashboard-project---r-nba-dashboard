library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

metrics <- read.csv('../data/r_dashboard_csv/metrics.csv')
chart_1 <- read.csv('../data/r_dashboard_csv/df_chart_1.csv')
chart_2 <- read.csv('../data/r_dashboard_csv/df_chart_2.csv')
chart_3 <- read.csv('../data/r_dashboard_csv/df_chart_3.csv')
chart_4 <- read.csv('../data/r_dashboard_csv/df_chart_4.csv')
chart_5 <- read.csv('../data/r_dashboard_csv/df_chart_5.csv')
chart_11 <- read.csv('../data/r_dashboard_csv/df_chart_11.csv')
chart_12 <- read.csv('../data/r_dashboard_csv/df_chart_12.csv')
chart_13 <- read.csv('../data/r_dashboard_csv/df_chart_13.csv')


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      dccDropdown(
        id='col-select',
        options = msleep %>%
          colnames %>%
          purrr::map(function(col) list(label = col, value = col)), 
        value='bodywt')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    p <- ggplot(subset(chart_2,Player == 'Kobe Bryant' & Stage == 'Regular_Season')) + 
      aes(x = Season, y = Assists_per_game, colour = 'blue') +
      stat_summary(fun = mean, geom = 'line', size =2) +
      ylab("Assists") +
      ggtitle('Average Assists by Season') +
      scale_x_continuous(breaks = chart_2$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(face = 'bold', vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    ggplotly(p)
  }
)

app$run_server(debug = T)