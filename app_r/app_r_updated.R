library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(stringr)


# metrics data
metrics <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/metrics.csv')[,-1]

# tab 1 data
chart_1 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_1.csv')[,-1]
chart_2 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_2.csv')[,-1]
chart_3 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_3.csv')[,-1]
chart_4 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_4.csv')[,-1]
chart_5 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_5.csv')[,-1]

# tab 2 data
chart_11 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_11.csv')[,-1]
chart_12 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_12.csv')[,-1]
chart_13 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_13.csv')[,-1]

# tab 3 data
df_chart_21 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart1.csv')[,-1]
df_chart_22 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart2.csv')[,-1]
df_chart_23 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart3.csv')[,-1]

df_tab_3_dropdown <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab_3_dropdown.csv')[,-1]

#------------------------------------------------------------#
# Setup app and layout/frontend

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
server <- app$server


# cards

# cards tab 1
first_card = dbcCard(
  dbcCardBody(children=
    list(
           htmlP("Career FG%", className="card-title"),
           htmlH5(id="card-01")
    )
  ),
  color='info'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

second_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career FT%", className="card-title"),
       htmlH5(id="card-02")
     )
  ),
  color='secondary'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

third_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career 3-pt %", className="card-title"),
       htmlH5(id="card-03")
     )
  ),
  color='info'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

fourth_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Avg Minutes per game", className="card-title"),
       htmlH5(id="card-04")
     )
  ),
  color='secondary'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)


cards <- dbcRow(
  list(
    dbcCol(first_card, width=3), 
    dbcCol(second_card, width=3),
    dbcCol(third_card, width=3), 
    dbcCol(fourth_card, width=3)
  )
)

# cards tab 2
fifth_card = dbcCard(
  dbcCardBody(children=
    list(
           htmlP("Career FG%", className="card-title"),
           htmlH5(id="card-05")
    )
  ),
  color='info'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

sixth_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career FT%", className="card-title"),
       htmlH5(id="card-06")
     )
  ),
  color='secondary'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

seventh_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career 3-pt %", className="card-title"),
       htmlH5(id="card-07")
     )
  ),
  color='info'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)

eighth_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Avg Minutes per game", className="card-title"),
       htmlH5(id="card-08")
     )
  ),
  color='secondary'
  , style=list('text-align' = 'center')
  , inverse = TRUE
)


cards_tab2 <- dbcRow(
 list(
   dbcCol(fifth_card, width=3), 
   dbcCol(sixth_card, width=3),
   dbcCol(seventh_card, width=3), 
   dbcCol(eighth_card, width=3)
 )
)

# data for dropdowns
longform_stat_list <- unique(df_tab_3_dropdown$stat_name)

option_statistic <- lapply(longform_stat_list,
                           function(available_indicator) {

                             list(label = available_indicator,
                                  value = available_indicator)
                           })

Players <- unique(chart_1$Player)

option_player <- lapply(Players,
                           function(available_indicator) {

                             list(label = available_indicator,
                                  value = available_indicator)
                           })

Stages <- unique(chart_2$Stage)

option_stage <- lapply(Stages,
                           function(available_indicator) {

                             list(label = available_indicator,
                                  value = available_indicator)
                           })

# dropdowns
first_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='player-widget',
      options = option_player,
      value='Kobe Bryant'  # REQUIRED to show the plot on the first page load
      )
  )#,
  #style={"width": "100%"}
)

second_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='stage-widget',
      #style={'width': '250px'},
      options=option_stage,
      value='Regular_Season'  # REQUIRED to show the plot on the first page load
      )
  )#,
  #style={"width": "100%"}
)

third_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='player-widget-2',
      #style={'width': '250px'},
      value='Kobe Bryant',  # REQUIRED to show the plot on the first page load
      options = option_player
     )
  ),
 # style={"width": "100%"}
)

fourth_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='stage-widget-2',
      #style={'width': '250px'},
      value='Regular_Season',  # REQUIRED to show the plot on the first page load
      options=option_stage
      )
  ),
  #style={"width": "100%"}
)

fifth_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='statistic-1',
      #style={'width': '250px'},
      value='Points per Game',  # REQUIRED to show the plot on the first page load
      options=option_statistic
      )
  ),
  #style={"width": "100%"}
)

dropdowns3 = dbcRow(
  list(
    dbcCol(fifth_dropdown, width=5), 
    dbcCol(htmlH5(''), width=7)
  )
)

dropdowns2 = dbcRow(
  list(
    dbcCol(third_dropdown, width=3), 
    dbcCol(fourth_dropdown, width=3),
    dbcCol(htmlH5(''), width=6)
  )
)

dropdowns = dbcRow(
  list(
    dbcCol(first_dropdown, width=3), 
    dbcCol(second_dropdown, width=3),
    dbcCol(htmlH5(''), width=6)
  )
)

# content for tab 1
tab1_content <- htmlDiv(
  list(
    dropdowns,
    htmlBr(),
    cards,
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-1',
 #           style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '345px', 'height': '300px'}
 ), width = 4),
        dbcCol(
          dccGraph(
            id='chart-2',
#            style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '345px', 'height': '300px'}
), width = 4),
        dbcCol(
          dccGraph(
            id='chart-3',
 #           style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '345px', 'height': '300px'}
 ), width = 4)
        
      )
    ),
    htmlBr(),
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-4',
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}
), width = 6),
        dbcCol(
          dccGraph(
            id='chart-5',
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}
), width = 6)
      )
    )
  )
)

# content for tab 2
tab2_content = htmlDiv(
  list(
    dropdowns2,
    htmlBr(),
    cards_tab2,
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-11',
            style=list('width' = '530px', 'height' = '350px')
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
            ),
            width = 6
        ),
        dbcCol(
          dccGraph(
            id='chart-12',
            style=list('width' = '530px', 'height' = '350px')
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
            ),
            width = 6
          )
      )
    ),
    htmlBr(),
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-13',
            style=list('width' = '530px', 'height' = '350px')
           # style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
          ),
          width = 6
        ),
        dbcCol(
          dccGraph(
            id='chart-14',
            style=list('width' = '530px', 'height' = '350px')
           # style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
           ),
           width = 6
        )
      )
    )
  )
)

# content for tab 3
tab3_content = htmlDiv(
  list(
    dropdowns3,
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-21',
            style=list('width' = '530px', 'height' = '350px')
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
            ),
            width = 6
        ),
        dbcCol(
          dccGraph(
            id='chart-22',
            style=list('width' = '530px', 'height' = '350px')
            #style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
            ),
            width = 6
          )
      )
    ),
    htmlBr(),
    htmlBr(),
    dbcRow(
      list(
        dbcCol(
          dccGraph(
            id='chart-23',
            style=list('width' = '1090px', 'height' = '350px')
           # style={'border-width': '1', 'border-color': '#DCDCDC', 'width': '530px', 'height': '300px'}), width = 6
          ),
          width = 6
        )
      )
    )
  )
)

# all tabs content
tabs = htmlDiv(
  list(
    htmlH2("NBA Analytics Dashboard"),
    dbcTabs(
      list(
        dbcTab(children=list(
          htmlBr(),
          tab1_content
        ),
        label="Player stats"
       # style={"padding": "10px"},
       # label_style={"color": "#4682B4", "font-weight": "bold", "font-size": "larger", "background-color": "#f4f6f6"},
       # active_label_style={"color": "#DC143C", "font-weight": "bold", "font-size": "larger", "background-color": "#FFEFD5"}
        ),
          dbcTab(children=list(
          htmlBr(),
          tab2_content
        ),
        label="Advanced Analytics"
       # style={"padding": "10px"},
       # label_style={"color": "#4682B4", "font-weight": "bold", "font-size": "larger", "background-color": "#f4f6f6"},
       # active_label_style={"color": "#DC143C", "font-weight": "bold", "font-size": "larger", "background-color": "#FFEFD5"}
        ),
        dbcTab(children=list(
          htmlBr(),
          tab3_content
        ),
        label="NBA Trends"
       # style={"padding": "10px"},
       # label_style={"color": "#4682B4", "font-weight": "bold", "font-size": "larger", "background-color": "#f4f6f6"},
       # active_label_style={"color": "#DC143C", "font-weight": "bold", "font-size": "larger", "background-color": "#FFEFD5"}
        )
      )
    )
  )
)

# layout
app$layout(
  dbcContainer(
    list(
      htmlBr(),
      tabs
    )
  )
)


# Set up callbacks/backend

# metrics
app$callback(
  output("card-01", "children"), 
  list(input("player-widget", "value"),
    input('stage-widget', 'value')),
  function(player, stage){
    tmp1 <- filter(metrics, Player == player & Stage == stage)
    career_FG <- tmp1$career_FG_.
    paste0(career_FG, ' %')
  }
)

app$callback(
  output("card-02", "children"), 
  list(input("player-widget", "value"),
    input('stage-widget', 'value')),
  function(player, stage){
    tmp2 <- filter(metrics, Player == player & Stage == stage)
    career_FT <- tmp2$career_FT_.
    paste0(career_FT, ' %')
  }
)

app$callback(
  output("card-03", "children"), 
  list(input("player-widget", "value"),
  input('stage-widget', 'value')),
  function(player, stage){
    tmp3 <- filter(metrics, Player == player & Stage == stage)
    career_3PT <- tmp3$career_3PT_.
    paste0(career_3PT, ' %')
  }
)

app$callback(
  output("card-04", "children"), 
  list(input("player-widget", "value"),
       input('stage-widget', 'value')),
  function(player, stage){
    tmp4 <- filter(metrics, Player == player & Stage == stage)
    avg_minutes <- tmp4$Minutes_per_game
    paste0(avg_minutes, ' minutes')
  }
)

app$callback(
  output("card-05", "children"), 
  list(input("player-widget-2", "value"),
    input('stage-widget-2', 'value')),
  function(player, stage){
    tmp1 <- filter(metrics, Player == player & Stage == stage)
    career_FG <- tmp1$career_FG_.
    paste0(career_FG, ' %')
  }
)

app$callback(
  output("card-06", "children"), 
  list(input("player-widget-2", "value"),
    input('stage-widget-2', 'value')),
  function(player, stage){
    tmp2 <- filter(metrics, Player == player & Stage == stage)
    career_FT <- tmp2$career_FT_.
    paste0(career_FT, ' %')
  }
)

app$callback(
  output("card-07", "children"), 
  list(input("player-widget-2", "value"),
  input('stage-widget-2', 'value')),
  function(player, stage){
    tmp3 <- filter(metrics, Player == player & Stage == stage)
    career_3PT <- tmp3$career_3PT_.
    paste0(career_3PT, ' %')
  }
)

app$callback(
  output("card-08", "children"), 
  list(input("player-widget-2", "value"),
       input('stage-widget-2', 'value')),
  function(player, stage){
    tmp4 <- filter(metrics, Player == player & Stage == stage)
    avg_minutes <- tmp4$Minutes_per_game
    paste0(avg_minutes, ' minutes')
  }
)
 

app$callback(
  output('chart-1', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')),
  function(input1, input2) {
    p <- ggplot(subset(chart_1,Player == input1 & Stage == input2)) + 
      aes(x = Season, y = Points_per_game,  fill = Points_type) +
      geom_bar(position="stack", stat="identity") +
      ylab("Points") +
      ggtitle('Average Points by Season') +
      scale_fill_manual("Points_type", values = c('2 Point' = 'steelblue3', '3 Point' = 'darkorange', 'Free throws' = 'coral2')) +
      scale_x_continuous(breaks = chart_1$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    return(ggplotly(p) %>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)

app$callback(
  output('chart-2', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(xcol, ycol){
    chart <- ggplot(subset(chart_2,Player == xcol & Stage == ycol)) + 
      aes(x = Season, y = Assists_per_game) +
      stat_summary(fun = mean, geom = 'line', size=1, color='steelblue3') +
      ylab("Assists") +
      ggtitle('Average Assists by Season') +
      scale_x_continuous(breaks = chart_2$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="none",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    return(ggplotly(chart))
  }
)

app$callback(
  output('chart-3', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(xcol, ycol){
    chart <- ggplot(subset(chart_3, Player == xcol & Stage == ycol)) +
      aes(x = Season, y = Rebounds_per_game,  fill = Rebound_type) +
      geom_bar(position="stack", stat="identity") +
      ylab("Rebounds") +
      ggtitle('Average Rebounds by Season') +
      scale_fill_manual("Rebound_type", values = c('Defensive Rebounds' = 'steelblue3', 'Offensive Rebounds' = 'darkorange')) + 
      scale_x_continuous(breaks = chart_3$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    return(ggplotly(chart) %>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)

app$callback(
  output('chart-4', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(xcol, ycol){
    chart <- ggplot(subset(chart_4,Player == xcol & Stage == ycol)) + 
      aes(x = Season, y = per_game, color = Blocks.Steals) +
      stat_summary(fun = mean, geom = 'line', size=1) +
      ylab("Count") +
      ggtitle('Average Blocks & Steals by Season') +
      scale_color_manual("Blocks.Steals", values = c('Blocks' = 'steelblue3', 'Steals' = 'darkorange')) +
      scale_x_continuous(breaks = chart_4$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    return(ggplotly(chart) %>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)

app$callback(
  output('chart-5', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(xcol, ycol){
    chart <- ggplot(subset(chart_5,Player == xcol & Stage == ycol)) + 
      aes(x = Season, y = per_game, color = Turnovers.Fouls) +
      stat_summary(fun = mean, geom = 'line', size=1) +
      ylab("Count") +
      ggtitle('Average Turnovers & Fouls by Season') +
      scale_color_manual("Turnovers.Fouls", values = c('Fouls' = 'steelblue3', 'Turnovers' = 'darkorange')) +
      # maximum value for turnovers or fouls in our dataset is 6.25
      ylim(0, 6.25) +
      scale_x_continuous(breaks = chart_5$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      )
    return(ggplotly(chart) %>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)

app$callback(
  output('chart-11', 'figure'),
  list(input('player-widget-2', 'value'),
       input('stage-widget-2', 'value')
  ),
  function(xcol, ycol){
  chart <- ggplot(subset(chart_11,Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = per_game, color = X2PT_3PT_eFG) +
    stat_summary(fun = mean, geom = 'line', size=1.0) +
    xlab("") +
    ylab("Shooting Percentage") +
    ggtitle('Average Shooting Percentages by Season') +
    scale_color_manual("X2PT_3PT_eFG", values = c('2PT_%' = 'steelblue3', '3PT_%' = 'darkorange', 'eFG_%' = 'darkred')) +
    scale_x_continuous(breaks = chart_11$Season) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position="bottom",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
    )
  return(ggplotly(chart) %>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)

app$callback(
  output('chart-12', 'figure'),
  list(input('player-widget-2', 'value'),
       input('stage-widget-2', 'value')
  ),
  function(xcol, ycol){
  chart <- ggplot(subset(chart_12, Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = True.shooting.percentage) +
    geom_bar(stat="identity", fill="steelblue3") +
    ylab("True Shooting Percentage") +
    ggtitle('True Shooting Percentage by Season') +
    scale_x_continuous(breaks = chart_12$Season) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position="none",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
    )
  return(ggplotly(chart))
  }
)

app$callback(
  output('chart-13', 'figure'),
  list(input('player-widget-2', 'value'),
       input('stage-widget-2', 'value')
  ),
  function(xcol, ycol){
  chart <- ggplot(subset(chart_13, Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = Game.Score) +
    geom_bar(stat="identity", fill="steelblue3") +
    ylab("Game Score") +
    ggtitle('Player Productivity by Season') +
    scale_x_continuous(breaks = chart_13$Season) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position="none",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
    )
  return(ggplotly(chart))
  }
)

app$callback(
  output('chart-14', 'figure'),
  list(input('player-widget-2', 'value')),
  function(xcol){
  sub_chart = subset(chart_13, Player == xcol)
  min_min <- trunc(min(sub_chart$Minutes.Played),0)
  max_min <- trunc(max(sub_chart$Minutes.Played) + 1,0)
  min_range <- seq(min_min, max_min, 1)
  chart <- ggplot(sub_chart) + 
    aes(x = Minutes.Played, y = Game.Score) +
    geom_point(aes(colour = Stage), size=1.8) +
    ylab("Game Score") +
    xlab("Minutes Played") +
    ggtitle('Player Productivity by Minutes Played') +
    scale_color_manual("Stage", values = c('Playoffs' = 'steelblue3', 'Regular_Season' = 'darkorange')) +
    scale_x_continuous(breaks = min_range) +
    theme(
      axis.text.x = element_text(angle = 0),
      legend.position="bottom",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_line(colour = 'lightgrey'),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), axis.title.y=element_text(vjust=0.1)
    )
  return(ggplotly(chart)%>% layout(legend = list(orientation = 'h', y= -0.2)))
  }
)


# Simple statistic line graph by Season (chart 21)
app$callback(
  output('chart-21', 'figure'),
  list(input('statistic-1', 'value')),
  function(statistic){
  leave_list = c('Average Player Minutes Played per Game', 'Average Player Weight (lbs)', 'Average Player Height (cm)', 'Average Player Body Mass Index', 'Ratio of Field Goals That Are 3-pointers')
  stat_lab <- str_replace(subset(df_tab_3_dropdown, stat_name == statistic)[,2], "/", ".")
  stat_lab <- str_replace(stat_lab, "3", "X3")
  if (statistic %in% leave_list != TRUE){
    statistic <- paste(statistic, "per Team")
  }
  chart <- ggplot(df_chart_21) + 
    aes(x = Season, y = .data[[stat_lab]]) +
    geom_line(aes(group=1), size = 1, color='darkred') +
    ggtitle(statistic) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.4),
      legend.position="none",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), 
      axis.title.y = element_blank()
    )
  return(ggplotly(chart))
}
)

# Statistic vs. age chart (chart 22)
app$callback(
  output('chart-22', 'figure'),
  list(input('statistic-1', 'value')),
  function(statistic){
  stat_lab <- str_replace(subset(df_tab_3_dropdown, stat_name == statistic)[,2], "/", ".")
  stat_lab <- str_replace(stat_lab, "3", "X3")
  chart <- ggplot(df_chart_22) + 
    aes(x = Age, y = .data[[stat_lab]]) +
    geom_line(size = 1, color='grey') +
    ggtitle(statistic) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.4),
      legend.position="none",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), 
      axis.title.y = element_blank()
    )
  return(ggplotly(chart))
}
)

# Statistic Bar chart for playoffs and regular season (chart 23)
app$callback(
  output('chart-23', 'figure'),
  list(input('statistic-1', 'value')),
  function(statistic){
  leave_list = c('Average Player Minutes Played per Game', 'Average Player Weight (lbs)', 'Average Player Height (cm)', 'Average Player Body Mass Index', 'Ratio of Field Goals That Are 3-pointers')
  stat_lab <- str_replace(subset(df_tab_3_dropdown, stat_name == statistic)[,2], "/", ".")
  stat_lab <- str_replace(stat_lab, "3", "X3")
  if (statistic %in% leave_list != TRUE){
    statistic <- paste(statistic, "per Team")
  }
  chart <- ggplot(df_chart_23) + 
    aes(x = Season, y = .data[[stat_lab]], fill = Type) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    ggtitle(statistic) +
    scale_fill_manual("Type", values = c('Playoffs' = 'steelblue2', 'Regular Season' = 'darkorange')) + 
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.4),
      legend.position="bottom",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.y=element_line(colour = 'lightgrey'),
      panel.grid.minor.y=element_blank(),
      panel.background=element_rect(fill="white"),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_text(vjust=-0.5), 
      axis.title.y = element_blank()
    )
  return(ggplotly(chart) %>% layout(legend = list(orientation = 'h', y= 1.15)))
}
)

app$run_server(debug = T)
