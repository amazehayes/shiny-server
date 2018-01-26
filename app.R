library(shiny)
library(shinydashboard)
library(DT)

defenses <- read.csv("defenses.csv")
defenses_avg <- read.csv("defenses_avg.csv")
defenses_qb <- defenses[1:576,2:15]
defenses_rb <- defenses[1:576,c(2,16:28)]
defenses_wr <- defenses[1:576,c(2,29:38)]
defenses_te <- defenses[1:576,c(2,39:48)]
defenses_avg_qb <- defenses_avg[,1:13]
defenses_avg_rb <- defenses_avg[,c(1,14:25)]
defenses_avg_wr <- defenses_avg[,c(1,26:34)]
defenses_avg_te <- defenses_avg[,c(1,35:43)]

defenses_qb <- setNames(defenses_qb,c("Week","Player","PassAtt","Comp","Comp%","PassYards",
                                      "PassTDs","INTs","RushAtts","RushYards","RushTDs","FPts(4pt)",
                                      "FPts(6pt)","PosRank"))
defenses_rb <- setNames(defenses_rb,c("Week","Player","RushAtts","RushYards","YPC","RushTDs",
                                      "Targets","Receptions","RecYards","RecTDs","FPts(PPR)",
                                      "FPts(1/2PPR)","FPts(Std)","PosRank"))
defenses_wr <- setNames(defenses_wr,c("Week","Player","Targets","Receptions","Rec%","RecYards",
                                      "RecTDs","FPts(PPR)","FPts(1/2PPR)","FPts(Std)","PosRank"))
defenses_te <- setNames(defenses_te,c("Week","Player","Targets","Receptions","Rec%","RecYards",
                                      "RecTDs","FPts(PPR)","FPts(1/2PPR)","FPts(Std)","PosRank"))

defenses_avg_qb <- setNames(defenses_avg_qb,c("Team","PassAtt","Comp","Comp%","PassYards",
                                              "PassTDs","INTs","RushAtts","RushYards","RushTDs",
                                              "FPts(4pt)","FPts(6pt)","PosRank"))
defenses_avg_rb <- setNames(defenses_avg_rb,c("Team","RushAtts","RushYards","YPC","RushTDs",
                                              "Targets","Receptions","RecYards","RecTDs",
                                              "FPts(PPR)","FPts(1/2PPR)","FPts(Std)","PosRank"))
defenses_avg_wr <- setNames(defenses_avg_wr,c("Team","Targets","Receptions","Rec%","RecYards",
                                              "RecTDs","FPts(PPR)","FPts(1/2PPR)","FPts(Std)",
                                              "PosRank"))
defenses_avg_te <- setNames(defenses_avg_te,c("Team","Targets","Receptions","Rec%","RecYards",
                                              "RecTDs","FPts(PPR)","FPts(1/2PPR)","FPts(Std)",
                                              "PosRank"))


consistency <- read.csv("consistencydata.csv")
consistency <- setNames(consistency, c("year","player","team","pos","posrank","games","total","avg",
                                                 "std.dev","floor","ceiling","CV%","COR","#top12",
                                                 "#13-24","#25-36","#rest"))

weekly_data <- read.csv("Weekly Data.csv")

total_weekly_data <- read.csv("total_Weekly_Data.csv")
total_weekly_points <- total_weekly_data[,1:25]
total_weekly_rank <- total_weekly_data[,c(1:6,32:52)]
total_weekly_points <- setNames(total_weekly_points, c("year","posrank","player","pos","team","G","1","2","3","4",
                                                       "5","6","7","8","9","10","11","12","13",
                                                       "14","15","16","17","total","avg"))
total_weekly_rank <- setNames(total_weekly_rank, c("year","posrank","player","pos","team","G","1","2","3","4",
                                                   "5","6","7","8","9","10","11","12","13",
                                                   "14","15","16","17","#1-12","#13-24",
                                                   "#25-36","#rest"))

yearly <- read.csv("Yearly Data.csv")

yearly_rank <- yearly[,1:17]
yearly_rank_current <- yearly_rank[1:971,]
yearly_rank_past <- yearly_rank[972:1457,]

yearly_points <- yearly[,c(1:3,19:26)]
yearly_points_current <- yearly_points[1:971,]
yearly_points_past <- yearly_points[972:1457,]

yearly_rank <- setNames(yearly_rank, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                       "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points <- setNames(yearly_points, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                           "2011","2010"))
yearly_rank_past <- setNames(yearly_rank_past, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                 "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points_past <- setNames(yearly_points_past, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                     "2011","2010"))
yearly_rank_current <- setNames(yearly_rank_current, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                       "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points_current <- setNames(yearly_points_current, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                                           "2011","2010"))
test <- t(yearly[,4:11])
colnames(test) <- yearly$player
Year <- c("2017","2016","2015","2014","2013","2012","2011","2010")
y <- cbind(Year,test)


qbdata <- read.csv("QBdata.csv")
qbdata <- transform(qbdata,
                    Comp. = as.numeric(sub("%","",Comp.)),
                    TD. = as.numeric(sub("%","",TD.)),
                    INT. = as.numeric(sub("%","",INT.)),
                    Comp..1 = as.numeric(sub("%","",Comp..1)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Pass.TDs = as.numeric(sub("%","",X.Pass.TDs)),
                    INT..1 = as.numeric(sub("%","",INT..1)),
                    X.INTs = as.numeric(sub("%","",X.INTs)),
                    Comp..2 = as.numeric(sub("%","",Comp..2)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Pass.TDs.1 = as.numeric(sub("%","",X.Pass.TDs.1)),
                    INT..2 = as.numeric(sub("%","",INT..2)),
                    X.INT = as.numeric(sub("%","",X.INT)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)),
                    FP.from.Rushing = as.numeric(sub("%","",FP.from.Rushing)))
qbdata <- setNames(qbdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","PassAtt","PassComp","Comp%","PassYards","PassTDs","INTs",
                            "Att/G","Comp/G","YPA","YPG","TD/G","INT/G","TD/INT","TD%","INT%",
                            "RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs",
                            "RZ.PassAtt<20","RZ.PassComp<20","RZ.Comp%<20","RZ.TDs<20","RZ.INTs<20",
                            "RZ.TDPer<20","%PassTDs<20","RZ.INT%<20","%INTs<20","RZ.TD/INT<20",
                            "RZ.PassAtt<10","RZ.PassComp<10","RZ.Comp%<10","RZ.TDs<10","RZ.INTs<10",
                            "RZ.TD%<10","%PassTDs<10","RZ.INT%<10","%INTs<10","RZ.TD/INT<10",
                            "FPts(4pt/TD)","FPts(6pt/TD)","PPG(4pt/TD)","PPG(6pt/TD)",
                            "PPAtt(4pt/TD)","PPAtt(6pt/TD)","PosRank(4pt/TD)","PosRank(6pt/TD)",
                            "FPfromYards","FPfromTDs","FPfromRush","YardMonster","TDdepend","MobileQB"))

rbdata <- read.csv("RBdata.csv")
rbdata <- transform(rbdata,
                    TD. = as.numeric(sub("%","",TD.)),
                    Rec. = as.numeric(sub("%","",Rec.)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    Total.TD. = as.numeric(sub("%","",Total.TD.)),
                    X.Rush.Att = as.numeric(sub("%","",X.Rush.Att)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Rush.TD = as.numeric(sub("%","",X.Rush.TD)),
                    X.Rush.Att.1 = as.numeric(sub("%","",X.Rush.Att.1)),
                    TD..3 = as.numeric(sub("%","",TD..3)),
                    X.Rush.TD.1 = as.numeric(sub("%","",X.Rush.TD.1)),
                    X.Rush.Att.2 = as.numeric(sub("%","",X.Rush.Att.2)),
                    TD..4 = as.numeric(sub("%","",TD..4)),
                    X.Rush.TD.2 = as.numeric(sub("%","",X.Rush.TD.2)),
                    Rec..1 = as.numeric(sub("%","",Rec..1)),
                    X.Target = as.numeric(sub("%","",X.Target)),
                    TD..5 = as.numeric(sub("%","",TD..5)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Rec..2 = as.numeric(sub("%","",Rec..2)),
                    X.Target.1 = as.numeric(sub("%","",X.Target.1)),
                    TD..6 = as.numeric(sub("%","",TD..6)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    X.Touches = as.numeric(sub("%","",X.Touches)),
                    TD..7 = as.numeric(sub("%","",TD..7)),
                    X.Total.TD = as.numeric(sub("%","",X.Total.TD)),
                    X.Touches.1 = as.numeric(sub("%","",X.Touches.1)),
                    X.Total.TD.1 = as.numeric(sub("%","",X.Total.TD.1)),
                    TD..8 = as.numeric(sub("%","",TD..8)),
                    FP.from.Rec = as.numeric(sub("%","",FP.from.Rec)),
                    FP.from.RuYards = as.numeric(sub("%","",FP.from.RuYards)),
                    FP.from.Total.TD = as.numeric(sub("%","",FP.from.Total.TD)))
rbdata <- setNames(rbdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","RushAtt","RushYards","YPC","RushTDs",
                            "RuAtt/G","RuYPG","RushTD%","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","Receptions/G","YPR","RecYPG","RecTD%","Touches",
                            "TotalYards","TotalTDs","YPT","TotalYPG","TotalTD%",
                            "RZ.RushAtt<20","RZ.RushYards<20","RZ.RushTDs<20","RZ.%RushAtt<20",
                            "RZ.RushTD%<20","RZ.%RushTD<20","RZ.RushAtt<10","RZ.RushYards<10",
                            "RZ.RushTDs<10","RZ.%RushAtt<10","RZ.RushTD%<10","RZ.%RushTD<10",
                            "RZ.RushAtt<5","RZ.RushYards<5","RZ.RushTDs<5","RZ.%RushAtt<5",
                            "RZ.RushTD%<5","RZ.%RushTD<5","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecYards<20","RZ.RecTDs<20","RZ.%Targets<20",
                            "RZ.RecTD%<20","RZ.%RecTD<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecYards<10","RZ.RecTDs<10","RZ.%Targets<10",
                            "RZ.RecTD%<10","RZ.%RecTD<10","RZ.Touches<20","RZ.TotalYards<20",
                            "RZ.TotalTDs<20","RZ.%Touches<20","RZ.TotalTD%<20","RZ.%TotalTD<20",
                            "RZ.Touches<10","RZ.TotalYards<10","RZ.TotalTDs<10","RZ.%Touches<10",
                            "RZ.TotalTD%<10","RZ.%TotalTD<10","FPts(PPR)","FPts(1/2PPR)","FPts(STD)",
                            "PPG(PPR)","PPG(1/2PPR)","PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)",
                            "PosRank(STD)","PPTouch(PPR)","PPTouch(1/2PPR)","PPTouch(STD)",
                            "FPfromRec","FPfromRuYards","FPfromTotalTDs","PPRMachine","YardMonster",
                            "TDdepend"))

wrdata <- read.csv("WRdata.csv")
wrdata <- transform(wrdata, 
                    Reception. = as.numeric(sub("%","",Reception.)),
                    Market.Share = as.numeric(sub("%","",Market.Share)),
                    Rec..TD. = as.numeric(sub("%","",Rec..TD.)),
                    Rush.TD. = as.numeric(sub("%","",Rush.TD.)),
                    Total.TD. = as.numeric(sub("%","",Total.TD.)),
                    Reception..1 = as.numeric(sub("%","",Reception..1)),
                    X.Targets = as.numeric(sub("%","",X.Targets)),
                    TD. = as.numeric(sub("%","",TD.)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Team.Target. = as.numeric(sub("%","",Team.Target.)),
                    Reception..2 = as.numeric(sub("%","",Reception..2)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    X.Targets.1 = as.numeric(sub("%","",X.Targets.1)),
                    FP.from.Rec. = as.numeric(sub("%","",FP.from.Rec.)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)),
                    Team.Target..1 = as.numeric(sub("%","",Team.Target..1)))
wrdata <- setNames(wrdata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                            "RecYPG","RecTD%","RushAtt","RushYards","RushTDs","RuAtt/G","RuYPG",
                            "RushTD%","TotalTDs","TotalTD%","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecTDs<20","RZ.%Targets<20","RZ.RecTD%<20",
                            "RZ.%RecTD<20","RZ.TeamTarget%<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecTDs<10","RZ.%Targets<10","RZ.RecTD%<10",
                            "RZ.%RecTD<10","RZ.TeamTarget%<10","FPts(PPR)","FPts(1/2PPR)","FPts(STD)",
                            "PPG(PPR)","PPG(1/2PPR)","PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)",
                            "PosRank(STD)","PPTarget(PPR)","PPTarget(1/2PPR)","PPTarget(STD)",
                            "FPfromRec","FPfromRecYards","FPfromTDs","PPRMachine","YardMonster",
                            "TDdepend"))

tedata <- read.csv("TEdata2.csv",fileEncoding="latin1")
tedata <- transform(tedata, 
                    Reception. = as.numeric(sub("%","",Reception.)),
                    Market.Share = as.numeric(sub("%","",Market.Share)),
                    TD. = as.numeric(sub("%","",TD.)),
                    Reception..1 = as.numeric(sub("%","",Reception..1)),
                    X.Targets = as.numeric(sub("%","",X.Targets)),
                    TD..1 = as.numeric(sub("%","",TD..1)),
                    X.Rec.TD = as.numeric(sub("%","",X.Rec.TD)),
                    Team.Target. = as.numeric(sub("%","",Team.Target.)),
                    Reception..2 = as.numeric(sub("%","",Reception..2)),
                    X.Targets.1 = as.numeric(sub("%","",X.Targets.1)),
                    TD..2 = as.numeric(sub("%","",TD..2)),
                    X.Rec.TD.1 = as.numeric(sub("%","",X.Rec.TD.1)),
                    Team.Target..1 = as.numeric(sub("%","",Team.Target.)),
                    FP.from.Rec. = as.numeric(sub("%","",FP.from.Rec.)),
                    FP.from.Yards = as.numeric(sub("%","",FP.from.Yards)),
                    FP.from.TDs = as.numeric(sub("%","",FP.from.TDs)))
tedata <- setNames(tedata,c("Year","Player","Age","Season","Round","Overall",
                            "Team","HeadCoach","OffCoordinator","DefCoordinator","SOS",
                            "Oline","Games","Targets","Receptions","Reception%","RecYards",
                            "RecTDs","Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                            "RecYPG","RecTD%","RZ.Targets<20","RZ.Receptions<20",
                            "RZ.Rec%<20","RZ.RecTDs<20","RZ.%Targets<20","RZ.RecTD%<20",
                            "RZ.%RecTD<20","RZ.TeamTarget%<20","RZ.Targets<10","RZ.Receptions<10",
                            "RZ.Rec%<10","RZ.RecTDs<10","RZ.%Targets<10","RZ.RecTD%<10",
                            "RZ.%RecTD<10","RZ.TeamTarget%<10","FPts(PPR)","FPts(1/2PPR)","FPts(STD)",
                            "PPG(PPR)","PPG(1/2PPR)","PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)",
                            "PosRank(STD)","PPTarget(PPR)","PPTarget(1/2PPR)","PPTarget(STD)",
                            "FPfromRec","FPfromRecYards","FPfromTDs","PPRMachine","YardMonster",
                            "TDdepend"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "FF Statistics"),
  
  dashboardSidebar(
    sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("dashboard")),
    menuItem("Start/Sit Tool", tabName = "tool", icon = icon("wrench")),
    menuItem("Consistency Data", tabName = "consistency", icon = icon("table")),
    menuItem("Weekly Data", tabName = "weekly", icon = icon("table")),
    menuItem("Yearly Data", tabName = "yearly", icon = icon("table"),
             menuSubItem("Data", tabName = "yearlydata"),
             menuSubItem("Chart",tabName = "yearlychart")),
    menuItem("Defenses", tabName = "defense", icon = icon("table"),
             menuSubItem("Team Data", tabName = "teamdefense"),
             menuSubItem("Averages", tabName = "avgdefense")),
    menuItem("Database", tabName = "database", icon = icon("database"),
             menuSubItem("Quarterback", tabName = "data_qb"),
             menuSubItem("Running Back", tabName = "data_rb"),
             menuSubItem("Wide Receiver", tabName = "data_wr"),
             menuSubItem("Tight End", tabName = "data_te")),
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "welcome",
            h1("Welcome!"),
            "Welcome to FF Statistics! This website aims to be your one-stop-shop for fantasy football data and statistics! Each tab contains different, sortable data for your fantasy football data needs. If you use the data, all we ask is you mention where you got it, from us!",
            br(),
            br(),
            strong("Start/Sit Tool"),
              ("– Compares two players based on percentage each player hits X amount of points, with graphs!"),
            br(),
            br(),
            strong("Consistency Data"),
              ("– Datatable of each player’s consistency stats based on average and standard deviation, and more!"),
            br(),
            br(),
            strong("Weekly Data"),
              ("– Datatable of every player’s weekly production (points and rank) since 2010."),
            br(),
            br(),
            strong("Yearly Data"),
              ("- Two tabs:"),
            br(),
              ("1) Datatable of every player’s yearly production (points and rank) since 2010."),
            br(),
              ("2) Chart and datatable of each player's yeary production (rank) since 2010."),
            br(),
            br(),
            strong("Defense Data"),
              ("- Two seperate datatables:"),
            br(),
              ("1) Team Data - a datatable that breaks down statlines for the top fantasy scorer each defense allowed to each position (QB, RB, WR, TE) by week"),
            br(),
              ("2) Averages - a datatable that shows the overall averages each defense allowed to each positions top fantasy scorer"),
            br(),
            br(),
            strong("Database"),
              ("– A massive, user-controlled fantasy football database in which you control the stats you want to see from the players you want to see. "),
            br(),
            br(),
            "We are always looking to improve the site! If you notice any bugs or errors, or want to see other stats and data, message Addison Hayes (@amazehayes_roto) on Twitter or email ajh5737@gmail.com with suggestions, comments, or questions!",
            br(),
            br(),
            "Enjoy!"
            
            ),
    
    tabItem(tabName = "tool",
            fluidRow(
              column(6, selectInput("format", "Choose Scoring Format:",
                                    c("PPR (4pt/TD)", "1/2PPR","Standard","PPR (6pt/TD)"),
                                    selected = "PPR (4pt/TD)"))),
            fluidRow(
              column(6, selectInput("con_playerA", "Choose Player A:",
                                           unique(as.character(weekly_data$Player)),
                                           selected = "Aaron Rodgers")),
              column(6, selectInput("con_playerB", "Choose Player B:",
                                               unique(as.character(weekly_data$Player)),
                                               selected = "Drew Brees"))),
            fluidRow(column(6, numericInput("con_numberA", "Select Points Needed (Greater Than):",
                                            value = 20, min = 0, max = 50, step = 0.1)),
                     column(6, numericInput("con_numberB", "Select Points Needed (Greater Than):",
                                            value = 20, min = 0 , max = 50, step = 0.1))),
            fluidRow(column(6, verbatimTextOutput("probA")),
                     column(6, verbatimTextOutput("probB"))),
            fluidRow(column(6,plotOutput("con_graphA")))),
      
    tabItem(tabName = "consistency",
            fluidRow(
              column(4,selectInput("pos","Position:",c("All",unique(
                as.character(consistency$pos))))),
              column(4,selectInput("con_year","Select Year:", c("All",unique(
                as.character(consistency$year)))))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("consistency"))),
    
    tabItem(
      tabName = "weekly",
            fluidRow(
              column(4, selectInput("weekly_year","Choose Year:",c("All",unique(
                as.character(total_weekly_data$year))))),
              column(4, selectInput("tog_weekly","Choose Type:",
                                    c("Points","Rank"))),
              column(4, selectInput("pos_weekly", "Position:",c("All",unique(
                as.character(total_weekly_data$Position)))))),
              fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("weekly"))),
    
    tabItem(tabName = "yearlydata",
            fluidRow(
              column(4, selectInput("tog_yearly","Choose Type:",
                                    c("Points","Rank"),selected = "Points")),
              column(4, selectInput("group_yearly", "Choose Player Group:",
                                    c("All", "Current", "Past"))),
              column(4, selectInput("pos_yearly", "Position:",
                                    c("All", unique(as.character(yearly$pos)))))),
              fluidRow(DT::dataTableOutput("yearly"))),
    
    tabItem(tabName = "yearlychart",
            fluidRow(
              column(4, selectInput("player_yearly","Choose Player:",
                                    c(unique(as.character(yearly$player)))))),
            fluidRow(
              column(8, plotOutput("yearly_graph")),
              column(3, DT::dataTableOutput("yearly_posrank")))),
    
    tabItem(tabName = "teamdefense",
            fluidRow(
              column(4, selectInput("def_team","Choose Team:",
                                    c(unique(as.character(defenses$Team))))),
              column(4, selectInput("def_pos", "Choose Position:",
                                    c("QB","RB","WR","TE")))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("team_defense"))),
    
    tabItem(tabName = "avgdefense",
            fluidRow(
              column(4, selectInput("def_avg","Choose Position:",
                                    c("QB","RB","WR","TE")))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("avg_defense"))),
    
    tabItem(tabName = "data_qb", 
            fluidPage(
            fluidRow(
              column(4, selectInput("qb_vars","Select Column(s):", choices = list(
                Player = c("Year","Player","Age","Season","Games"),
                Draft = c("Round","Overall"),
                Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                Passing = c("PassAtt","PassComp","Comp%","PassYards","PassTDs","INTs",
                            "Att/G","Comp/G","YPA","YPG","TD/G","INT/G","TD/INT","TD%","INT%"),
                Rushing = c("RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs"),
                RedZone20 = c("RZ.PassAtt<20","RZ.PassComp<20","RZ.Comp%<20","RZ.TDs<20","RZ.INTs<20",
                              "RZ.TDPer<20","%PassTDs<20","RZ.INT%<20","%INTs<20","RZ.TD/INT<20"),
                RedZone10 = c("RZ.PassAtt<10","RZ.PassComp<10","RZ.Comp%<10","RZ.TDs<10","RZ.INTs<10",
                              "RZ.TD%<10","%PassTDs<10","RZ.INT%<10","%INTs<10","RZ.TD/INT<10"),
                Fantasy = c("FPts(4pt/TD)","FPts(6pt/TD)","PPG(4pt/TD)","PPG(6pt/TD)",
                            "PPAtt(4pt/TD)","PPAtt(6pt/TD)","PosRank(4pt/TD)","PosRank(6pt/TD)",
                            "FPfromYards","FPfromTDs","FPfromRush","YardMonster","TDdepend","MobileQB")
              ),multiple = TRUE,selected = c("Year","Player","Team","Games","PassAtt","PassComp",
                                             "Comp%","PassYards","PassTDs","INTs")))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("qbdata")))),
    
    tabItem(tabName = "data_rb",
            fluidPage(
            fluidRow(
              column(4, selectInput("rb_vars","Select Column(s):", choices = list(
                Player = c("Year","Player","Age","Season","Games"),
                Draft = c("Round","Overall"),
                Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                Rushing = c("RushAtt","RushYards","YPC","RushTDs","RuAtt/G","RuYPG","RuTD%"),
                Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs","Targets/G",
                              "Receptions/G","YPR","ReYPG","RecTD%"),
                Total = c("Touches","TotalYards","TotalTDs","YPT","TotalYPG","TotalTD%"),
                RedZone20 = c("RZ.RushAtt<20","RZ.RushYards<20","RZ.RushTDs<20","RZ.%RushAtt<20",
                              "RZ.RushTD%<20","RZ.%RushTD<20","RZ.Targets<20","RZ.Receptions<20",
                              "RZ.Rec%<20","RZ.RecYards<20","RZ.RecTDs<20","RZ.%Targets<20",
                              "RZ.RecTD%<20","RZ.%RecTD<20","RZ.Touches<20","RZ.TotalYards<20",
                              "RZ.TotalTDs<20","RZ.%Touches<20","RZ.TotalTD%<20","RZ.%TotalTD<20"),
                RedZone10 = c("RZ.RushAtt<10","RZ.RushYards<10","RZ.RushTDs<10","RZ.%RushAtt<10",
                              "RZ.RushTD%<10","RZ.%RushTD<10","RZ.Targets<10","RZ.Receptions<10",
                              "RZ.Rec%<10","RZ.RecYards<10","RZ.RecTDs<10","RZ.%Targets<10",
                              "RZ.RecTD%<10","RZ.%RecTD<10","RZ.Touches<10","RZ.TotalYards<10",
                              "RZ.TotalTDs<10","RZ.%Touches<10","RZ.TotalTD%<10","RZ.%TotalTD<10"),
                RedZone5 = c("RZ.RushAtt<5","RZ.RushYards<5","RZ.RushTDs<5","RZ.%RushAtt<5",
                             "RZ.RushTD%<5","RZ.%RushTD<5"),
                Fantasy = c("FPts(PPR)","FPts(1/2PPR)","FPts(STD)","PPG(PPR)","PPG(1/2PPR)",
                            "PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)","PosRank(STD)",
                            "PPTouch(PPR)","PPTouch(1/2PPR)","PPTouch(STD)","FPfromRec",
                            "FPfromRuYards","FPfromTotalTDs","PPRMachine","YardMonster","TDdepend")
              ),multiple = TRUE,selected = c("Year","Player","Team","Games","RushAtt","RushYards",
                                             "YPC","RushTDs")))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("rbdata")))),

    tabItem(tabName = "data_wr",
            fluidPage(
              fluidRow(
                column(4, selectInput("wr_vars","Select Column(s):", choices = list(
                  Player = c("Year","Player","Age","Season","Games"),
                  Draft = c("Round","Overall"),
                  Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                  Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs",
                                "Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                                "RecYPG","RecTD%"),
                  Rushing = c("RushAtt","RushYards","RushTDs","RuAtt/G","RuYPG","RushTD%"),
                  Total = c("TotalTDs","TotalTD%"),
                  RedZone20 = c("RZ.Targets<20","RZ.Receptions<20","RZ.Rec%<20","RZ.RecTDs<20",
                                "RZ.%Targets<20","RZ.RecTD%<20","RZ.%RecTD<20","RZ.TeamTarget%<20"),
                  RedZone10 = c("RZ.Targets<10","RZ.Receptions<10","RZ.Rec%<10","RZ.RecTDs<10",
                                "RZ.%Targets<10","RZ.RecTD%<10","RZ.%RecTD<10","RZ.TeamTarget%<10"),
                  Fantasy = c("FPts(PPR)","FPts(1/2PPR)","FPts(STD)","PPG(PPR)","PPG(1/2PPR)",
                              "PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)","PosRank(STD)",
                              "PPTarget(PPR)","PPTarget(1/2PPR)","PPTarget(STD)","FPfromRec",
                              "FPfromRecYards","FPfromTDs","PPRMachine","YardMonster","TDdepend")
                ),multiple = TRUE,selected = c("Year","Player","Team","Games","Targets","Receptions",
                                               "Reception%","RecYards","RecTDs")))),
              fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("wrdata")))),
    
    tabItem(tabName = "data_te",
            fluidPage(
              fluidRow(
                column(4, selectInput("te_vars","Select Column(s):", choices = list(
                  Player = c("Year","Player","Age","Season","Games"),
                  Draft = c("Round","Overall"),
                  Team = c("Team","HeadCoach","OffCoordinator","DefCoordinator","SOS","Oline"),
                  Receiving = c("Targets","Receptions","Reception%","RecYards","RecTDs",
                                "Targets/G","MarketShare","Receptions/G","YPTarget","YPR",
                                "RecYPG","RecTD%"),
                  RedZone20 = c("RZ.Targets<20","RZ.Receptions<20","RZ.Rec%<20","RZ.RecTDs<20",
                                "RZ.%Targets<20","RZ.RecTD%<20","RZ.%RecTD<20","RZ.TeamTarget%<20"),
                  RedZone10 = c("RZ.Targets<10","RZ.Receptions<10","RZ.Rec%<10","RZ.RecTDs<10",
                                "RZ.%Targets<10","RZ.RecTD%<10","RZ.%RecTD<10","RZ.TeamTarget%<10"),
                  Fantasy = c("FPts(PPR)","FPts(1/2PPR)","FPts(STD)","PPG(PPR)","PPG(1/2PPR)",
                              "PPG(STD)","PosRank(PPR)","PosRank(1/2PPR)","PosRank(STD)",
                              "PPTarget(PPR)","PPTarget(1/2PPR)","PPTarget(STD)","FPfromRec",
                              "FPfromRecYards","FPfromTDs","PPRMachine","YardMonster","TDdepend")
                ),multiple = TRUE,selected = c("Year","Player","Team","Games","Targets","Receptions",
                                               "Reception%","RecYards","RecTDs")))),
              fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("tedata"))))
    
    
    )
)
)

# Define server logic
server <- function(input, output) {
  
  #Print Welcome Tab
  output$welcometext <- ({
    renderText("")
  })
  
  #Print Consistency Datatable
  output$consistency <- DT::renderDataTable({
    DT::datatable({
      
      if(input$pos == "All" & input$con_year == "All") {
        consistency
      }
      
      if(input$pos == "All" & input$con_year != "All") {
        consistency
        consistency <- consistency[consistency$year == input$con_year,]
      }
      
      if(input$pos != "All" & input$con_year == "All") {
        consistency <- consistency[consistency$pos == input$pos,]
      }
      
      if(input$pos != "All" & input$con_year != "All") {
        consistency
        consistency <- consistency[consistency$year == input$con_year,]
        consistency <- consistency[consistency$pos == input$pos,]
      }
      
    })
    
    consistency
    
  }, rownames = FALSE, filter = "top", extensions = 'Buttons',options = list(lengthMenu = c(12,24,36,50),dom = 'Bfrtip', buttons = c('excel',
                                                                                              'csv','copy')))
  
  #Print Start/Sit Tool
  output$probA <- renderText({
    
    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsA <- input$con_numberA
      count <- 0
      
      p1 <- x[(input$con_playerA),]
      
      for (i in 1:length(p1)) {
        if(p1[i] >= pointsA & !is.na(p1[i])) {
          count = sum(p1>=pointsA, na.rm = TRUE)
        }
      }
      a <- count/length(na.omit(p1))
    }
    
    if(input$format == "PPR (6pt/TD)") {
      x <- as.matrix(weekly_data[1051:1573,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsA <- input$con_numberA
      count <- 0
      
      p1 <- x[(input$con_playerA),]
      
      for (i in 1:length(p1)) {
        if(p1[i] >= pointsA & !is.na(p1[i])) {
          count = sum(p1>=pointsA, na.rm = TRUE)
        }
      }
      a <- count/length(na.omit(p1))
    }
    
    if(input$format == "1/2PPR") {
      x <- as.matrix(weekly_data[1:523,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsA <- input$con_numberA
      count <- 0
      
      p1 <- x[(input$con_playerA),]
      
      for (i in 1:length(p1)) {
        if(p1[i] >= pointsA & !is.na(p1[i])) {
          count = sum(p1>=pointsA, na.rm = TRUE)
        }
      }
      a <- count/length(na.omit(p1))
    }
    
    if(input$format == "Standard") {
      x <- as.matrix(weekly_data[526:1048,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsA <- input$con_numberA
      count <- 0
      
      p1 <- x[(input$con_playerA),]
      
      for (i in 1:length(p1)) {
        if(p1[i] >= pointsA & !is.na(p1[i])) {
          count = sum(p1>=pointsA, na.rm = TRUE)
        }
      }
      a <- count/length(na.omit(p1))
    }
    
    paste(signif(a, digits = 4)*100,"% - (RED)")
    
  })
  
  output$probB <- renderText({
    
    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsB <- input$con_numberB
      count <- 0
      
      p2 <- x[(input$con_playerB),]
      
      for (i in 1:length(p2)) {
        if(p2[i] >= pointsB & !is.na(p2[i])) {
          count = sum(p2>=pointsB, na.rm = TRUE)
        }
      }
      b <- count/length(na.omit(p2))
    }
    
    if(input$format == "PPR (6pt/TD)") {
      x <- as.matrix(weekly_data[1051:1573,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsB <- input$con_numberB
      count <- 0
      
      p2 <- x[(input$con_playerB),]
      
      for (i in 1:length(p2)) {
        if(p2[i] >= pointsB & !is.na(p2[i])) {
          count = sum(p2>=pointsB, na.rm = TRUE)
        }
      }
      b <- count/length(na.omit(p2))
    }
    
    if(input$format == "1/2PPR") {
      x <- as.matrix(weekly_data[1:523,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsB <- input$con_numberB
      count <- 0
      
      p2 <- x[(input$con_playerB),]
      
      for (i in 1:length(p2)) {
        if(p2[i] >= pointsB & !is.na(p2[i])) {
          count = sum(p2>=pointsB, na.rm = TRUE)
        }
      }
      b <- count/length(na.omit(p2))
    }
    
    if(input$format == "Standard") {
      x <- as.matrix(weekly_data[526:1048,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsB <- input$con_numberB
      count <- 0
      
      p2 <- x[(input$con_playerB),]
      
      for (i in 1:length(p2)) {
        if(p2[i] >= pointsB & !is.na(p2[i])) {
          count = sum(p2>=pointsB, na.rm = TRUE)
        }
      }
      b <- count/length(na.omit(p2))
    }
    
    paste(signif(b, digits = 4)*100,"% - (LIGHT GREEN)")
    
  })
  
  output$con_graphA <- renderPlot({

    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
    }
    
    if(input$format == "PPR (6pt/TD)") {
      x <- as.matrix(weekly_data[1051:1573,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
    }
    
    if(input$format == "1/2PPR") {
      x <- as.matrix(weekly_data[1:523,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
    }
    
    if(input$format == "Standard") {
      x <- as.matrix(weekly_data[526:1048,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
    }
    
    hist(p1, col = "red", main = paste("Histogram of", input$con_playerA, "&", input$con_playerB), xlab = "Fantasy Points", ylim = c(0,15))
    hist(p2, col = rgb(0,1,0,0.5), add = TRUE)
  })
  
  #Weekly Tab
  output$weekly <- DT::renderDataTable({
    DT::datatable({
      
      if(input$weekly_year == "All" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- total_weekly_points
      }
      
      if(input$weekly_year != "All" & input$tog_weekly == "Points" & input$pos_weekly == "All") {
        weekly <- total_weekly_points
        weekly <- total_weekly_points[total_weekly_points$year == input$weekly_year,]
      }
      
      if(input$weekly_year != "All" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- total_weekly_points
        weekly <- total_weekly_points[total_weekly_points$year == input$weekly_year & total_weekly_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "All" & input$tog_weekly == "Points" & input$pos_weekly != "All") {
        weekly <- total_weekly_points
        weekly <- total_weekly_points[total_weekly_points$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "All" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- total_weekly_rank
      }
      
      if(input$weekly_year != "All" & input$tog_weekly == "Rank" & input$pos_weekly == "All") {
        weekly <- total_weekly_rank
        weekly <- total_weekly_rank[total_weekly_rank$year == input$weekly_year,]
      }
      
      if(input$weekly_year != "All" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- total_weekly_rank
        weekly <- total_weekly_rank[total_weekly_rank$year == input$weekly_year & total_weekly_rank$pos == input$pos_weekly,]
      }
      
      if(input$weekly_year == "All" & input$tog_weekly == "Rank" & input$pos_weekly != "All") {
        weekly <- total_weekly_rank
        weekly <- total_weekly_rank[total_weekly_rank$pos == input$pos_weekly,]
      }
      
    })
    
    weekly
  }, rownames = FALSE, filter = "top", extensions = 'Buttons' ,options = list(lengthMenu = c(12,24,36,50),
                                                              dom = 'Bfrtip', buttons = 'excel',
                                                              'csv','copy'))
  
  #Yearly Data
  output$yearly <- DT::renderDataTable({
    DT::datatable({
      
      if (input$tog_yearly == "Points" & input$group_yearly == "All" & input$pos_yearly == "All") {
        yearly <- yearly_points
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "All" & input$pos_yearly != "All") {
        yearly <- yearly_points
        yearly <- yearly_points[yearly_points$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "Current" & input$pos_yearly == "All") {
        yearly <- yearly_points_current
      }
      
      if(input$tog_yearly == "Points" & input$group_yearly == "Current" & input$pos_yearly != "All") {
        yearly <- yearly_points_current
        yearly <- yearly_points_current[yearly_points_current$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Points" & input$group_yearly == "Past" & input$pos_yearly == "All") {
        yearly <- yearly_points_past
      }
      
      if(input$tog_yearly == "Points" & input$group_yearly == "Past" & input$pos_yearly != "All") {
        yearly <- yearly_points_past
        yearly <- yearly_points_past[yearly_points_past$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "All" & input$pos_yearly == "All") {
        yearly <- yearly_rank
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "All" & input$pos_yearly != "All") {
        yearly <- yearly_rank
        yearly <- yearly_rank[yearly_rank$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "Current" & input$pos_yearly == "All") {
        yearly <- yearly_rank_current
      }
      
      if(input$tog_yearly == "Rank" & input$group_yearly == "Current" & input$pos_yearly != "All") {
        yearly <- yearly_rank_current
        yearly <- yearly_rank_current[yearly_rank_current$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$group_yearly == "Past" & input$pos_yearly == "All") {
        yearly <- yearly_rank_past
      }
      
      if(input$tog_yearly == "Rank" & input$group_yearly == "Past" & input$pos_yearly != "All") {
        yearly <- yearly_rank_past
        yearly <- yearly_rank_past[yearly_points_rank$pos == input$pos_yearly,]
      }
    })
    yearly <- yearly[order(yearly$pos),]
  },  rownames = FALSE, filter = "top", extensions = 'Buttons' ,options = list(lengthMenu = c(12,24,36,50),
                                                               dom = 'Bfrtip', buttons = c('excel',
                                                               'csv','copy')))
  
  #Yearly Graph
  output$yearly_graph <- renderPlot({
    
    x <- as.matrix(yearly[,4:11])
    rownames(x) <- yearly$player
    p1 <- x[(input$player_yearly),]
    p2 <- names(p1)
    yrange <- range(1,p1, na.rm = TRUE)
    xrange <- range(p2, na.rm = FALSE)
    yrange2 <- ceiling(yrange[2]/5)*yrange[2]
    
    plot(na.omit(p1),type = "l",axes = FALSE, ylim = rev(range(yrange)),xlab = "Year",
         ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearly))
    axis(1, at=1:8, lab=c("'17","'16","'15","'14","'13","'12","'11","'10"))
    axis(2, at=c(1,5*1:yrange[2]))
  })
  
  output$yearly_posrank <- DT::renderDataTable({
    DT::datatable({
      y <- y[,c("Year",input$player_yearly)]
    },rownames = FALSE, options = list(dom = 't'))
  })
  
  #Defenses Tab
  output$team_defense <- DT::renderDataTable({
    DT::datatable({
      
      if(input$def_pos == "QB") {
        team_defense <- defenses_qb[defenses$Team == input$def_team,]
      }
      
      if(input$def_pos == "RB") {
        team_defense <- defenses_rb[defenses$Team == input$def_team,]
      }
      
      if(input$def_pos == "WR") {
        team_defense <- defenses_wr[defenses$Team == input$def_team,]
      }
      
      if(input$def_pos == "TE") {
        team_defense <- defenses_te[defenses$Team == input$def_team,]
      }
      
    team_defense  
    
    },rownames = FALSE, filter = "top", extensions = 'Buttons', options = list(dom = 'Bfrtip', 
                                                               buttons = c('excel','csv','copy'),
                                                               pageLength = 20))
  })
  
  output$avg_defense <- DT::renderDataTable({
    DT::datatable({
      
      if(input$def_avg == "QB") {
        avg_defense <- defenses_avg_qb
      }
      
      if(input$def_avg == "RB") {
        avg_defense <- defenses_avg_rb
      }
      
      if(input$def_avg == "WR") {
        avg_defense <- defenses_avg_wr
      }
      
      if(input$def_avg == "TE") {
        avg_defense <- defenses_avg_te
      }
      
    avg_defense 
      
    },rownames = FALSE, filter = "top", extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                               buttons = c('excel','csv','copy'),
                                                               pageLength = 35))
  })
  
  
  #QB Database
  output$qbdata <- DT::renderDataTable({
    DT::datatable(
    
      if(!is.null(input$qb_vars)) {
        qbdata[,input$qb_vars, drop = FALSE]
      }
      , rownames = FALSE, filter = "top" , extensions = 'Buttons', 
      options = list(lengthMenu = c(10,25,50,100), dom = 'Bfrtip', buttons = c('excel',
                                                                             'csv','copy'))

    )
  })
  
  #RB Database
  output$rbdata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$rb_vars)) {
        rbdata[,input$rb_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top", extensions = 'Buttons',
      options = list(lengthMenu = c(10,25,50,100), dom = 'Bfrtip', buttons = c('excel',
                                                                             'csv','copy'))
      
    )
  })
    
  #WR Database
  output$wrdata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$wr_vars)) {
        wrdata[,input$wr_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top", extensions = 'Buttons',
      options = list(lengthMenu = c(10,25,50,100), dom = 'Bfrtip', buttons = c('excel',
                                                                               'csv','copy'))
      
    ) 
    
  })
  
  #TE Database
  output$tedata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$te_vars)) {
        tedata[,input$te_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top", extensions = 'Buttons',
      options = list(lengthMenu = c(10,25,50,100), dom = 'Bfrtip', buttons = c('excel',
                                                                               'csv','copy'))
      
    ) 
    
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

