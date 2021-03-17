# Import libraries

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

# Read in the neccesary dataframes

# tiles metrics dataframe

df_metrics <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/metrics.csv')[,-1]

# tab 1 dataframes

df_chart_1 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_1.csv')[,-1]

df_chart_2 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_2.csv')[,-1]

df_chart_3 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_3.csv')[,-1]

df_chart_4 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_4.csv')[,-1]

df_chart_5 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_5.csv')[,-1]

# tab 2 dataframes

df_chart_11 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_11.csv')[,-1]

df_chart_12 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_12.csv')[,-1]

df_chart_13 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/df_chart_13.csv')[,-1]

# tab 3 dataframes

df_chart_21 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart1.csv')[,-1]

df_chart_22 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart2.csv')[,-1]

df_chart_23 <- read.csv('https://raw.githubusercontent.com/ubco-mds-2020-labs/dashboard-project---r-nba-dashboard/master/data/r_dashboard_csv/tab3_chart3.csv')[,-1]






# Functions for tiles

# Career Field Goal Percentage

metric_FG <- function(player, stage){
  career_FG = subset(df_metrics, Player == player & Stage == stage)$career_FG_.[1]
  return (paste(as.character(career_FG),'%'))
}

# Career Free Throw Percentage

metric_FT <- function(player, stage){
  career_FT = subset(df_metrics, Player == player & Stage == stage)$career_FT_.[1]
  return (paste(as.character(career_FT),'%'))
}

# Career 3-Point percentage

metric_3PT <- function(player, stage){
  career_3PT = subset(df_metrics, Player == player & Stage == stage)$career_3PT_.[1]
  return (paste(as.character(career_3PT),'%'))
}

# Career Average Minutes Per game

metric_minutes <- function(player, stage){
  avg_minutes = subset(df_metrics, Player == player & Stage == stage)$Minutes_per_game[1]
  return (paste(as.character(avg_minutes),'minutes'))
}




# Functions for charts for tab 1

# Average points by season (chart 1). Does not omit missed seasons (leaves a blank space)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_1, Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = Points_per_game,  fill = Points_type) +
    geom_bar(position="stack", stat="identity") +
    ylab("Points") +
    ggtitle('Average Points by Season') +
    scale_fill_manual("Points_type", values = c('2 Point' = 'steelblue2', '3 Point' = 'darkorange', 'Free throws' = 'coral2')) +
    scale_x_continuous(breaks = df_chart_1$Season) +
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


# Average Assists by Season (chart 2)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_2,Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = Assists_per_game) +
    geom_line(size=1, color='steelblue2') +
    ylab("Assists") +
    ggtitle('Average Assists by Season') +
    scale_x_continuous(breaks = df_chart_2$Season) +
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


# Average Rebounds by Season (chart 3)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_3, Player == xcol & Stage == ycol)) +
    aes(x = Season, y = Rebounds_per_game,  fill = Rebound_type) +
    geom_bar(position="stack", stat="identity") +
    ylab("Rebounds") +
    ggtitle('Average Rebounds by Season') +
    scale_fill_manual("Rebound_type", values = c('Defensive Rebounds' = 'steelblue2', 'Offensive Rebounds' = 'darkorange')) + 
    scale_x_continuous(breaks = df_chart_3$Season) +
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


# Average Blocks & Steals by Season (chart 4)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_4,Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = per_game, color = Blocks.Steals) +
    geom_line(size=1) +
    ylab("Count") +
    ggtitle('Average Blocks & Steals by Season') +
    scale_color_manual("Blocks.Steals", values = c('Blocks' = 'steelblue2', 'Steals' = 'darkorange')) +
    scale_x_continuous(breaks = df_chart_4$Season) +
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

# Average Turnovers & Fouls by Season (chart 5)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_5,Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = per_game, color = Turnovers.Fouls) +
    geom_line(size=1) +
    ylab("Count") +
    ggtitle('Average Turnovers & Fouls by Season') +
    scale_color_manual("Turnovers.Fouls", values = c('Fouls' = 'steelblue2', 'Turnovers' = 'darkorange')) +
    # maximum value for turnovers or fouls in our dataset is 6.25
    ylim(0, 6.25) +
    scale_x_continuous(breaks = df_chart_5$Season) +
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





# Functions for charts for tab 2

# Average Shooting Percentages by Season chart (chart 11)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_11,Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = per_game, color = X2PT_3PT_eFG) +
    geom_line(size=1) +
    ylab("Shooting Percentage") +
    ggtitle('Average Shooting Percentages by Season') +
    scale_color_manual("X2PT_3PT_eFG", values = c('2PT_%' = 'steelblue2', '3PT_%' = 'darkorange', 'eFG_%' = 'darkred')) +
    scale_x_continuous(breaks = df_chart_11$Season) +
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


# True Shooting Percentage by Season (chart 12) NOTE y scale is fixed at 0. I couldn't get it to be 'free'

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_12, Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = True.shooting.percentage) +
    geom_bar(stat="identity", fill="steelblue2") +
    ylab("True Shooting Percentage") +
    ggtitle('True Shooting Percentage by Season') +
    scale_x_continuous(breaks = df_chart_12$Season) +
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

# Player Productivity by Season (chart 13)

plot_R <- function(xcol, ycol){
  chart <- ggplot(subset(df_chart_13, Player == xcol & Stage == ycol)) + 
    aes(x = Season, y = Game.Score) +
    geom_bar(stat="identity", fill="steelblue2") +
    ylab("Game Score") +
    ggtitle('Player Productivity by Season') +
    scale_x_continuous(breaks = df_chart_13$Season) +
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

# Player Productivity by Minutes Played (chart 14)

plot_R <- function(xcol){
  sub_chart = subset(df_chart_13, Player == xcol)
  min_min <- trunc(min(sub_chart$Minutes.Played),0)
  max_min <- trunc(max(sub_chart$Minutes.Played) + 1,0)
  min_range <- seq(min_min, max_min, 1)
  chart <- ggplot(sub_chart) + 
    aes(x = Minutes.Played, y = Game.Score) +
    geom_point(aes(colour = Stage), size=1.8) +
    ylab("Game Score") +
    xlab("Minutes Played") +
    ggtitle('Player Productivity by Minutes Played') +
    scale_color_manual("Stage", values = c('Playoffs' = 'steelblue2', 'Regular_Season' = 'darkorange')) +
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





# Functions for charts for tab 3

# Simple statistic line graph by Season (chart 21)

simple_stat <- function(statistic){
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


# Statistic vs. age chart (chart 22)

simple_stat_age <- function(statistic){
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



