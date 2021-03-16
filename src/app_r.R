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


#------------------------------------------------------------#
# Setup app and layout/frontend

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
server <- app$server


# cards

first_card = dbcCard(
  dbcCardBody(children=
    list(
           htmlP("Career FG%", className="card-title"),
           htmlH5(id="card-01")
    )
  ),
  color='info'#, inverse=True#, style={'text-align': 'center'}
)

second_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career FT%", className="card-title"),
       htmlH5(id="card-02")
     )
  ),
  color='secondary'#, inverse=True#, style={'text-align': 'center'}
)

third_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Career 3-pt %", className="card-title"),
       htmlH5(id="card-03")
     )
  ),
  color='info'#, inverse=True#, style={'text-align': 'center'}
)

fourth_card = dbcCard(
  dbcCardBody(children=
     list(
       htmlP("Avg Minutes per game", className="card-title"),
       htmlH5(id="card-04")
     )
  ),
  color='secondary'#, inverse=True#, style={'text-align': 'center'}
)


cards <- dbcRow(
  list(
    dbcCol(first_card, width=3), 
    dbcCol(second_card, width=3),
    dbcCol(third_card, width=3), 
    dbcCol(fourth_card, width=3)
  )
)


#cards_tab2 <- dbcRow(
#  list(
 #   dbcCol(fifth_card, width=3), 
 #   dbcCol(sixth_card, width=3),
 #   dbcCol(seventh_card, width=3), 
 #   dbcCol(eighth_card, width=3)
 # )
#)







Players <- unique(chart_1$Player)

option_player <- lapply(Players,
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

Stages <- unique(chart_2$Stage)

option_stage <- lapply(Stages,
                           function(available_indicator) {

                             list(label = available_indicator,
                                  value = available_indicator)
                           })

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

dropdowns = dbcRow(
  list(
    dbcCol(first_dropdown, width=3), 
    dbcCol(second_dropdown, width=3),
    dbcCol(htmlH5(''), width=6)
  )
)


third_dropdown = htmlDiv(
  list(
    dccDropdown(
      id='player-widget-2',
      #style={'width': '250px'},
      value='Kobe Bryant',  # REQUIRED to show the plot on the first page load
      options=Players
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
      options=Stage
      )
  ),
  #style={"width": "100%"}
)




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

tabs = htmlDiv(
  list(
    htmlH2("NBA Analytics Dashboard"),
    dbcTabs(
      list(
        dbcTab(children=list(
          htmlBr(),
          tab1_content
        ),
        label="Player stats",
       # style={"padding": "10px"},
       # label_style={"color": "#4682B4", "font-weight": "bold", "font-size": "larger", "background-color": "#f4f6f6"},
       # active_label_style={"color": "#DC143C", "font-weight": "bold", "font-size": "larger", "background-color": "#FFEFD5"}
        )
      )
    )
  )
)

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
  output('chart-1', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')),
  function(input1, input2) {
    p <- ggplot(subset(chart_1,Player == input1 & Stage == input2)) + 
      aes(x = Season, y = Points_per_game,  fill = Points_type) +
      stat_summary(fun = mean, position = 'stack', geom = 'bar') +
      ylim(0, 40) + 
      ylab("Points") +
      ggtitle('Average Game Points by Season') +
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
    ggplotly(p)
  }
)

app$callback(
  output('chart-2', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(input1, input2) {
    p <- ggplot(subset(chart_2,Player == input1 & Stage == input2)) + 
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

app$callback(
  output('chart-3', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(input1, input2) {
    p <- ggplot(subset(chart_3,Player == input1 & Stage == input2)) + 
      aes(x = Season, y = Rebounds_per_game,  fill = Rebound_type) +
      stat_summary(fun = mean, position = 'stack', geom = 'bar') +
      ylab("Rebounds") +
      ggtitle('Average Rebounds by Season') +
      #scale_fill_manual("Points_type", values = c('Defensive Rebounds' = 'steelblue2', 'Offensive Rebounds' = 'darkorange')) + 
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
    ggplotly(p)
  }
)

app$callback(
  output('chart-4', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(input1, input2) {
    p <- ggplot(subset(chart_4,Player == input1 & Stage == input2)) + 
      aes(x = Season, y = per_game, colour = Blocks.Steals) +
      stat_summary(fun = mean, geom = 'line', size =2) +
      ylab("Count") +
      ggtitle('Average Blocks & Steals by Season') +
      scale_x_continuous(breaks = chart_2$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.key = element_rect(fill='white'),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(face = 'bold', vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      ) +
      guides(size = 'none', colour = 'legend')
    ggplotly(p)
  }
)


app$callback(
  output('chart-5', 'figure'),
  list(input('player-widget', 'value'),
       input('stage-widget', 'value')
  ),
  function(input1, input2) {
    p <- ggplot(subset(chart_5,Player == input1 & Stage == input2)) + 
      aes(x = Season, y = per_game, colour = Turnovers.Fouls) +
      stat_summary(fun = mean, geom = 'line', size =2) +
      ylab("Count") +
      ggtitle('Average Turnovers and Fouls by Season') +
      scale_x_continuous(breaks = chart_2$Season) +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.key = element_rect(fill='white'),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(colour = 'lightgrey'),
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.line = element_line(colour = 'black'),
        axis.title.x = element_text(face = 'bold', vjust=-0.5), axis.title.y=element_text(vjust=0.1)
      ) +
      guides(size = 'none', colour = 'legend')
    ggplotly(p)
  }
)

app$run_server(debug = T)

