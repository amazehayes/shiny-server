library(shiny)
library(shinydashboard)
library(DT)

zscores <- read.csv("zscores.csv")
colnames(zscores) <- c("Year","Player","Age","Season","FanPts","ZScore","",
                       "Year","Player","Age","Season","FanPts","ZScore","",
                       "Year","Player","Age","Season","FanPts","ZScore","",
                       "Year","Player","Age","Season","FanPts","ZScore","",
                       "Age","Average","Season","Average","",
                       "Age","Average","Season","Average","",
                       "Age","Average","Season","Average","",
                       "Age","Average","Season","Average")
zscoreqb <- zscores[1:1166,1:6]
zscorerb <- zscores[1:2609,8:13]
zscorewr <- zscores[1:3044,15:20]
zscorete <- zscores[1:1778,22:27]
zscoreqbage <- zscores[1:24,29:30]
zscorerbage <- zscores[1:19,34:35]
zscorewrage <- zscores[1:22,39:40]
zscoreteage <- zscores[1:17,44:45]
zscoreqbseason <- zscores[1:21,31:32]
zscorerbseason <- zscores[1:16,36:37]
zscorewrseason <- zscores[1:20,41:42]
zscoreteseason <- zscores[1:17,46:47]

finish <- read.csv("total_finishes.csv")
colnames(finish) <- c("Player","1-12","13-24","25-36","Rest","%1-12","%13-24","%25-36","%Rest")

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
conyear <- read.csv("consistency.csv")
conyear <- setNames(conyear, c("player","2017","2016","2015","2014","2013","2012",
                                       "2011","2010","2017","2016","2015","2014","2013","2012",
                                       "2011","2010"))

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
twp_2017 <- total_weekly_points[2:565,]
twp_2016 <- total_weekly_points[566:1131,]
twp_2015 <- total_weekly_points[113:1707,]
twp_2014 <- total_weekly_points[1708:2250,]
twp_2013 <- total_weekly_points[2251:2767,]
twp_2012 <- total_weekly_points[2768:3291,]
twp_2011 <- total_weekly_points[3292:3888,]
twp_2010 <- total_weekly_points[3889:4490,]


offyear <- read.csv("offyearly2.csv")
offyear <- setNames(offyear, c("Year","Player","Pos","Team","Games","PassAtt","PassComp","Comp%","PassYards",
                               "PassTDs","INTs","RushAtt","RushYards","YPC","RushTDs","Targets",
                               "Receptions","Reception%","RecYards","RecTDs","ReturnYards","FP"))
position <- offyear$Pos
offyearqb <- subset(offyear[,c(1:2,4:13,15,21:22)],position == "QB")
offyearrb <- subset(offyear[,c(1:2,4:5,12:22)],position == "RB")
offyearwr <- subset(offyear[,c(1:2,4:5,16:22)],position == "WR")
offyearte <- subset(offyear[,c(1:2,4:5,16:22)],position == "TE")
offyearflex <- subset(offyear[,c(1:5,12:22)],position != "QB")

offyearqb2 <- subset(offyear[,c(1:2,4:13,15)],position == "QB")
offyearrb2 <- subset(offyear[,c(1:2,4:5,12:21)],position == "RB")
offyearwr2 <- subset(offyear[,c(1:2,4:5,16:21)],position == "WR")
offyearte2 <- subset(offyear[,c(1:2,4:5,16:21)],position == "TE")
offyearflex2 <- subset(offyear[,c(1:5,12:21)],position != "QB")

idpyear <- read.csv("idpyearly.csv")
idpyear <- setNames(idpyear, c("Year","Player","Pos","Team","Games","Tackles","Assists",
                               "Sacks","PassDef","INTs","FumbleForced","FumbleRec","Safeties",
                               "TDs","ReturnYards","FP","FP/G","PosRank"))

positionidp <- idpyear$Pos
idpyear2 <- idpyear[,1:15]
idpyeardl <- subset(idpyear[,1:16],positionidp == "DL")
idpyearlb <- subset(idpyear[,1:16],positionidp == "LB")
idpyeardb <- subset(idpyear[,1:16],positionidp == "DB")

idpyeardl2 <- subset(idpyear[,1:15],positionidp == "DL")
idpyearlb2 <- subset(idpyear[,1:15],positionidp == "LB")
idpyeardb2 <- subset(idpyear[,1:15],positionidp == "DB")

yearly <- read.csv("Yearly Data.csv")

yearly_rank <- yearly[,1:17]
yearly_points <- yearly[,c(1:3,19:26)]


yearly_rank <- setNames(yearly_rank, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                       "2011","2010","#1","#top5","#1-12","#13-24","#25-36","#rest"))
yearly_points <- setNames(yearly_points, c("player","team","pos","2017","2016","2015","2014","2013","2012",
                                           "2011","2010"))

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
                            "RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs","ReturnYards",
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
                            "TotalYards","TotalTDs","YPT","TotalYPG","TotalTD%","ReturnYards",
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
                            "RushTD%","TotalTDs","TotalTD%","ReturnYards","RZ.Targets<20","RZ.Receptions<20",
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
                            "RecYPG","RecTD%","ReturnYards","RZ.Targets<20","RZ.Receptions<20",
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

  dashboardHeader(title = "FF Statistics",
                  tags$li(a(img(src = 'logo.png',height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  
  dashboardSidebar(
    sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("dashboard")),
    menuItem("Start/Sit Tool", tabName = "tool", icon = icon("wrench")),
    menuItem("Consistency", tabName = "consistency", icon = icon("table"),
             menuSubItem("Consistency Data", tabName = "condata"),
             menuSubItem("Consistency Charts", tabName = "conchart")),
    menuItem("Weekly Data", tabName = "weekly", icon = icon("table"),
             menuSubItem("Weekly Datatable","weeklydata"),
             menuSubItem("Weekly Tool", "weeklytool"),
             menuSubItem("Weekly Chart","weeklychart")),
    menuItem("Yearly Data", tabName = "yearly", icon = icon("table"),
             menuSubItem("Yearly Datatable", tabName = "yearlydata"),
             menuSubItem("Yearly Chart",tabName = "yearlychart")),
    menuItem("Defenses", tabName = "defense", icon = icon("table"),
             menuSubItem("Team Data", tabName = "teamdefense"),
             menuSubItem("Averages", tabName = "avgdefense")),
    menuItem("Database", tabName = "database", icon = icon("database"),
             menuSubItem("Quarterback", tabName = "data_qb"),
             menuSubItem("Running Back", tabName = "data_rb"),
             menuSubItem("Wide Receiver", tabName = "data_wr"),
             menuSubItem("Tight End", tabName = "data_te")),
    menuItem("Custom Fantasy Charts", tabName = "custom", icon = icon("wrench"),
             menuSubItem("Offense", tabName = "customoff"),
             menuSubItem("Defense", tabName = "customdef")),
    menuItem("Z-Score Analysis", tabName = "zscore", icon = icon("bar-chart"),
             menuSubItem("Datatable",tabName = "zdata"),
             menuSubItem("Graphs",tabName = "zgraph")),
    menuItem("Download Data", tabName = "download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(includeScript("analystics.js")),
    tags$head(tags$style(HTML('
                              .sidebar {
                              color: #FFF;
                              position: fixed;
                              width: 220px;
                              white-space: nowrap;
                              overflow: visible;
                              }
                              
                              .main-header {
                              position: fixed;
                              width:100%;
                              }
                              .content {
                              padding-top: 60px;
                              }'))),
    tabItems(
    tabItem(tabName = "welcome",
            h1("Welcome!"),
            "Welcome to FF Statistics! This website aims to be your one-stop-shop for fantasy football data and statistics! Each tab contains different, sortable data for your fantasy football data needs. 
            If you use the data, all we ask is you mention where you got it, from us! Unless otherwise noted, all formats are PPR and 4 point per passing TD.",
            br(),
            br(),
            strong("Start/Sit Tool"),
              ("- Compares two players based on percentage each player hits X amount of points, with graphs!"),
            br(),
            br(),
            strong("Consistency Data"),
              ("- Two tabs:"),
            br(),
              ("1) Datatable of each player's consistency stats based on average and standard deviation, and more!"),
            br(),
              ("2) Chart of each  player's average points/game since 2010 with error bars to show their standard deviation."),
            br(),
            br(),
            strong("Weekly Data"),
              ("- Three tabs:"),
            br(),
              ("1) Datatable of every player's weekly production (points and rank) since 2010."),
            br(),
              ("2) Datatable of every player's weekly points over a customizable weekly time frame."),
            br(),
              ("3) Bar graph of each player's career weekly finishes (rank) since 2010."),
            br(),
            br(),
            strong("Yearly Data"),
              ("- Two tabs:"),
            br(),
              ("1) Datatable of every player's yearly production (points and rank) since 2010."),
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
              ("- A massive, user-controlled fantasy football database in which you control the stats you want to see from the players you want to see."),
            br(),
              ("*Note: A '0' in the Round and Overall columns means Undrafted Free Agent*"),
            br(),
            br(),
            strong("Custom Fantasy Charts"),
              ("- A 100% customizable datatable for any fantasy scoring format for both offense and defense!"),
            br(),
            br(),
            strong("Z-Score Analysis"),
              ("- Partnering with Tyler Ghee (@TylerGheeNFL), this tab uses z-scores to analyze player scoring and positional career arcs. 
               Z-scores act similar to a percentile (usually between -3 and 3). The higher the z-score,
               the better the player scored compared to the average player at his position. For specific questions, message Tyler or Addison on Twitter!"),
            br(),
            br(),
            strong("Download"),
              ("- This tab gives users the ability to download any dataset from FF Statistics into Excel for personal use! Just select the data you want and hit Download!"),
            br(),
            br(),
            ("We are always looking to improve the site! If you notice any bugs or errors, or want to see other stats and data, message Addison Hayes (@amazehayes_roto) on Twitter or email ajh5737@gmail.com with suggestions, comments, or questions!"),
            br(),
            br(),
            ("FF Statistics logo created by Jake Anderson (@nfldrafttalker)."),
            br(),
            br(),
            ("Enjoy!"),
            div(img(src = 'logo.png', height=200,width=200), style="text-align: right;")
            
            ),
    
    tabItem(tabName = "tool",
            fluidRow(
              column(6, selectInput("format", "Choose Scoring Format:",
                                    c("PPR (4pt/TD)", "1/2PPR","Standard","PPR (6pt/TD)"),
                                    selected = "PPR (4pt/TD)")),
              column(6, numericInput("con_number", "Select Points Needed (Greater Than):",
                                     value = 20, min = 0, max = 50, step = 0.1)) ),
            fluidRow(
              column(6, selectInput("con_playerA", "Choose Player A:",
                                           unique(as.character(weekly_data$Player)),
                                           selected = "Aaron Rodgers")),
              column(6, selectInput("con_playerB", "Choose Player B:",
                                               unique(as.character(weekly_data$Player)),
                                               selected = "Drew Brees"))),
            fluidRow(column(6, verbatimTextOutput("probA")),
                     column(6, verbatimTextOutput("probB"))),
            fluidRow(column(6,plotOutput("con_graphA")),
                     column(4,DT::dataTableOutput("con_tableB")))),
      
    tabItem(tabName = "condata",
            fluidRow(
              column(4,selectInput("pos","Position:",c("All",unique(
                as.character(consistency$pos))))),
              column(4,selectInput("con_year","Select Year:", c("All",unique(
                as.character(consistency$year)))))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("condatatable")),
            br(),
            br(),
            h2("Glossary"),
            strong("Average"),
            ("- the average fantasy points per game scored by the player"),
            br(),
            br(),
            strong("Standard Deviation (Std Dev)"),
            ("- measurement of how spread apart the data is around the average (higher the std dev, the more spread apart the data is around the average and vice versa)"),
            br(),
            br(),
            strong("Floor and Ceiling:"),
            br(),
            ("Floor - found by subtracting the std dev from the average"),
            br(),
            ("Ceiling - found by adding the std dev to the average"),
            br(),
            ("Note: this range of values predicts where a player will score on a weekly basis in ~70% of his games"),
            br(),
            br(),
            strong("CV%"),
            ("- used in finance, CV is found by dividing the std dev and the average. 
             The lower the number, the 'better' the return-on-investment is for a certain stock.
             For fantasy, this number can explain an amount of risk associated with starting a player weekly. 
             The lower the number the 'better'."),
            br(),
            br(),
            strong("COR"),
            ("- statistic created by Addison Hayes, COR is a single 'power ranking' number that incorporates CV% and ceiling for each player to rank consistency.
             COR weighs both ceiling and CV that rewards players for breakout performances, but also performing on a consistent basis every week.
             A high COR means that player is consistently performing for fantasy and mostly at a high level.
             A low COR means that player is struggling to find consistency or relevant fantasy production.
             By nature, quarterbacks will have higher CORs than most RBs, WRs, and TEs, while tight ends usually have lower CORs than the other three positions.")
            ),
    
    tabItem(tabName = "conchart",
            fluidRow(
              column(4, selectInput("conchart_playerA","Choose Player:",
                                    c("None",unique(as.character(conyear$player))),
                                    selected = "Russell Wilson")),
              column(4, selectInput("conchart_playerB","Choose Player:",
                                    c("None",unique(as.character(conyear$player))),
                                    selected = "Cam Newton")),
              column(4, selectInput("conchart_playerC","Choose Player:",
                                    c("None",unique(as.character(conyear$player))),
                                    selected = "None"))),
            fluidRow(column(10, plotOutput("con_chart"))),
            fluidRow(column(10, style = "overflow-x: scroll", DT::dataTableOutput("con_dt")))
            ),
    
    tabItem(tabName = "weeklydata",
            fluidRow(
              column(4, selectInput("weekly_year","Choose Year:",c("All",unique(
                as.character(total_weekly_data$year))))),
              column(4, selectInput("tog_weekly","Choose Type:",
                                    c("Points","Rank"))),
              column(4, selectInput("pos_weekly", "Position:",c("All",unique(
                as.character(total_weekly_data$Position)))))),
              fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("weekly"))),
    
    tabItem(tabName = "weeklytool",
            fluidPage(
            fluidRow(
              column(3, selectInput("wt_year","Choose Year:",
                                    c("2017","2016","2015","2014","2013","2012","2011","2010"))),
              column(3, selectInput("wt_weekA", "Choose Week:",
                                    c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))),
              column(3, selectInput("wt_weekB", "Choose Week:",
                                    c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"),
                                    selected = "5"))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("weekly_tool")))),
    
    tabItem(tabName = "weeklychart",
            fluidRow(
              column(3, selectInput("player_weeklyA","Choose Player:",
                                    c("None",unique(as.character(finish$Player))),
                                    selected = "Russell Wilson")),
              column(3, selectInput("player_weeklyB","Choose Player:",
                                    c("None",unique(as.character(finish$Player))),
                                    selected = "Cam Newton")),
              column(3, selectInput("player_weeklyC","Choose Player:",
                                    c("None",unique(as.character(finish$Player))),
                                    selected = "None")),
              column(3, selectInput("tog_wf","Choose Type:",c("Total","Percent")))),
            fluidRow(
              column(8, plotOutput("weekly_graph")),
              column(4, DT::dataTableOutput("weekly_table")))),
    
    tabItem(tabName = "yearlydata",
            fluidRow(
              column(4, selectInput("tog_yearly","Choose Type:",
                                    c("Points","Rank"),selected = "Points")),
              column(4, selectInput("pos_yearly", "Position:",
                                    c("All", unique(as.character(yearly$pos)))))),
              fluidRow(style = "overflow-x: scroll",DT::dataTableOutput("yearly"))),
    
    tabItem(tabName = "yearlychart",
            fluidRow(
              column(4, selectInput("player_yearlyA","Choose Player:",
                                    c("None",unique(as.character(yearly$player))),
                                    selected = "Russell Wilson")),
              column(4, selectInput("player_yearlyB","Choose Player:",
                                    c("None",unique(as.character(yearly$player))),
                                    selected = "Cam Newton")),
              column(4, selectInput("player_yearlyC","Choose Player:",
                                    c("None",unique(as.character(yearly$player)))))),
            fluidRow(
              column(8, plotOutput("yearly_graph")),
              column(4, style = "overflow-x: scroll", DT::dataTableOutput("yearly_posrank")))),
    
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
                Rushing = c("RushAtt","RushYards","RushTDs","RAtt/G","RYards/G","TotalTDs","ReturnYards"),
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
                Total = c("Touches","TotalYards","TotalTDs","YPT","TotalYPG","TotalTD%","ReturnYards"),
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
                  Total = c("TotalTDs","TotalTD%","ReturnYards"),
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
                                "RecYPG","RecTD%","ReturnYards"),
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
              fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("tedata")))),
    
    tabItem(tabName = "customoff",
            fluidRow(
              column(4, selectInput("offyear_pos","Select Position:",
                                    c(unique(as.character(offyear$Pos)),"Flex")))),
            fluidRow(
             column(2, numericInput("off_numberA", "Points Per Pass Attempt:",
                                     value = 0, min = 0, max = 1, step = 0.01)),
             column(2, numericInput("off_numberB", "Points Per Pass Compl:",
                                    value = 0, min = 0, max = 1, step = 0.01)),
             column(2, numericInput("off_numberC", "Points Per Pass Yard:",
                                    value = 0.04, min = 0, max = 1, step = 0.01)),
             column(2, numericInput("off_numberD", "Points Per Pass TD:",
                                    value = 4, min = 0, max = 10, step = 1)),
             column(2, numericInput("off_numberE", "Points Per Interception:",
                                    value = -2, min = -5, max = 5, step = 1))),
            fluidRow(
             column(2, numericInput("off_numberF", "Points Per Carry:",
                                     value = 0, min = 0, max = 1, step = 0.01)),
             column(2, numericInput("off_numberG", "Points Per Reception:",
                                    value = 1, min = 0, max = 5, step = 0.1)),
             column(2, numericInput("off_numberJ", "TE PPR Premium:",
                                    value = 1, min = 0, max = 5, step = 0.1)),
             column(2, numericInput("off_numberH", "Points Per Rush/Rec Yard:",
                                    value = 0.1, min = 0, max = 5, step = 0.1)),
             column(2, numericInput("off_numberI", "Points Per Rush/Rec TD:",
                                    value = 6, min = 0, max = 10, step = 1)),
             column(2, numericInput("off_numberK", "Points Per Return Yard:",
                                    value = 0, min = 0, max = 1, step = 0.01))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("offyearly"))),
    
    tabItem(tabName = "customdef",
            fluidRow(
              column(4, selectInput("defyear_pos","Select Position:",
                                    c("All",unique(as.character(idpyear$Pos)))))),
            fluidRow(
              column(2, numericInput("def_numberA", "Points Per Tackle:",
                                     value = 1, min = 0, max = 10, step = 0.1)),
              column(2, numericInput("def_numberB", "Points Per Assist:",
                                     value = 0.5, min = 0, max = 10, step = 0.1)),
              column(2, numericInput("def_numberC", "Points Per Sack:",
                                     value = 4, min = 0, max = 20, step = 0.1)),
              column(2, numericInput("def_numberD", "Points Per Pass Defensed:",
                                     value = 1, min = 0, max = 10, step = 0.1)),
              column(2, numericInput("def_numberE", "Points Per Interception:",
                                     value = 5, min = 0, max = 20, step = 0.1))),
            fluidRow(
              column(2, numericInput("def_numberF", "Points Per Forced Fumble:",
                                     value = 3, min = 0, max = 20, step = 0.1)),
              column(2, numericInput("def_numberG", "Points Per Fumble Rec.:",
                                     value = 2, min = 0, max = 20, step = 0.1)),
              column(2, numericInput("def_numberH", "Points Per Safety:",
                                     value = 2, min = 0, max = 10, step = 0.1)),
              column(2, numericInput("def_numberI", "Points Per TD:",
                                     value = 6, min = 0, max = 20, step = 0.1)),
              column(2, numericInput("def_numberJ", "Points Per Return Yard:",
                                     value = 0, min = 0, max = 1, step = 0.01))),
            fluidRow(style = "overflow-x: scroll", DT::dataTableOutput("defyearly"))),
    
    tabItem(tabName = "zdata",
            fluidRow(
              column(4, selectInput("z_pos","Choose Position:",c("QB","RB","WR","TE")))),
            fluidRow(column(12,style = "overflow-x: scroll", DT::dataTableOutput("zscoretable")))),
    
    tabItem(tabName = "zgraph",
            fluidRow(
              column(4, selectInput("z_pos2","Choose Position:",c("QB","RB","WR","TE"))),
              column(4, selectInput("z_tog","Choose Type:",c("Age","Season")))),
            fluidRow(
              column(8, plotOutput("z_graph")),
              column(4, DT::dataTableOutput("z_graphtable"))
            )),
    
    tabItem(tabName = "download",
            "Welcome to the Download tab! This is where you can download any dataset found on 
            FF Statistics into an Excel file for your own use! Just click on the dataset you need
            in the drop down box below and click Download!",
            br(),
            br(),
            fluidRow(
              column(4,  selectInput("dataset", "Choose a dataset:",
                                     choices = c("Consistency", "Weekly", "Yearly","Defenses",
                                                 "Defenses (Avg)","QB Database","RB Database",
                                                 "WR Database","TE Database","IDP Database","ZScore")))),
            fluidRow(column(4, downloadButton("downloadData", "Download"))))
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
  output$condatatable <- DT::renderDataTable({
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
    
  }, rownames = FALSE, filter = "top",options = list(lengthMenu = c(12,24,36,50)))
  
  output$con_chart <- renderPlot({
    
    if(input$conchart_playerA == "None" & input$conchart_playerB == "None" & input$conchart_playerC == "None"){
      paste("Please select a player...")
    }
    
    if(input$conchart_playerA != "None" & input$conchart_playerB == "None" & input$conchart_playerC == "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerA,]
      s1 <- consd[input$conchart_playerA,]
      bar <- barplot(p1, ylim = c(0,35), col = "red", main = paste("Average Points per Game for",input$conchart_playerA),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,p1-s1,bar,p1+s1, lwd = 1)
      legend("topright",c(input$conchart_playerA),col = c("red"), lwd = 8)
    }
    
    if(input$conchart_playerA == "None" & input$conchart_playerB != "None" & input$conchart_playerC == "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerB,]
      s1 <- consd[input$conchart_playerB,]
      bar <- barplot(p1, ylim = c(0,35), col = "blue", main = paste("Average Points per Game for",input$conchart_playerB),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,p1-s1,bar,p1+s1, lwd = 1)
      legend("topright",c(input$conchart_playerB),col = c("blue"), lwd = 8)
    }
    
    if(input$conchart_playerA == "None" & input$conchart_playerB == "None" & input$conchart_playerC != "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerC,]
      s1 <- consd[input$conchart_playerC,]
      bar <- barplot(p1, ylim = c(0,35), col = rgb(0,1,0,0.5), main = paste("Average Points per Game for",input$conchart_playerC),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,p1-s1,bar,p1+s1, lwd = 1)
      legend("topright",c(input$conchart_playerC),col = c(rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$conchart_playerA != "None" & input$conchart_playerB != "None" & input$conchart_playerC == "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerA,]
      s1 <- consd[input$conchart_playerA,]
      p2 <- conavg[input$conchart_playerB,]
      s2 <- consd[input$conchart_playerB,]
      test <- rbind(p1,p2)
      test2 <- rbind(s1,s2)
      bar <- barplot(test, ylim = c(0,35), col = c("red","blue"), beside = TRUE, main = paste("Average Points per Game for",input$conchart_playerA, "&",input$conchart_playerB),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,test-test2,bar,test+test2, lwd = 1)
      legend("topright",c(input$conchart_playerA, input$conchart_playerB),col = c("red","blue"), lwd = 8)
    }
    
    if(input$conchart_playerA == "None" & input$conchart_playerB != "None" & input$conchart_playerC != "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerB,]
      s1 <- consd[input$conchart_playerB,]
      p2 <- conavg[input$conchart_playerC,]
      s2 <- consd[input$conchart_playerC,]
      test <- rbind(p1,p2)
      test2 <- rbind(s1,s2)
      bar <- barplot(test, ylim = c(0,35), col = c("blue",rgb(0,1,0,0.5)), beside = TRUE, main = paste("Average Points per Game for",input$conchart_playerB, "&",input$conchart_playerC),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,test-test2,bar,test+test2, lwd = 1)
      legend("topright",c(input$conchart_playerB, input$conchart_playerC),col = c("blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$conchart_playerA != "None" & input$conchart_playerB == "None" & input$conchart_playerC != "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerA,]
      s1 <- consd[input$conchart_playerA,]
      p2 <- conavg[input$conchart_playerC,]
      s2 <- consd[input$conchart_playerC,]
      test <- rbind(p1,p2)
      test2 <- rbind(s1,s2)
      bar <- barplot(test, ylim = c(0,35), col = c("red",rgb(0,1,0,0.5)), beside = TRUE, main = paste("Average Points per Game for",input$conchart_playerA, "&",input$conchart_playerC),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,test-test2,bar,test+test2, lwd = 1)
      legend("topright",c(input$conchart_playerA, input$conchart_playerC),col = c("red",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$conchart_playerA != "None" & input$conchart_playerB != "None" & input$conchart_playerC != "None"){
      conavg <- as.matrix(conyear[,2:9])
      rownames(conavg) <- conyear$player
      consd <- as.matrix(conyear[,10:17])
      rownames(consd) <- conyear$player
      p1 <- conavg[input$conchart_playerA,]
      s1 <- consd[input$conchart_playerA,]
      p2 <- conavg[input$conchart_playerB,]
      s2 <- consd[input$conchart_playerB,]
      p3 <- conavg[input$conchart_playerC,]
      s3 <- consd[input$conchart_playerC,]
      test <- rbind(p1,p2,p3)
      test2 <- rbind(s1,s2,s3)
      bar <- barplot(test, ylim = c(0,35), col = c("red","blue",rgb(0,1,0,0.5)), beside = TRUE, main = paste("Average Points per Game for",input$conchart_playerA, "&",input$conchart_playerB,"&",input$conchart_playerC),
                     xlab = "Year", ylab = "Points per Game")
      segments(bar,test-test2,bar,test+test2, lwd = 1)
      legend("topright",c(input$conchart_playerA,input$conchart_playerB,input$conchart_playerC),col = c("red","blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    bar
    
  })
  
  output$con_dt <- renderDataTable({
    DT::datatable({
      
      if(input$conchart_playerA == "None" & input$conchart_playerB == "None" & input$conchart_playerC == "None"){
        paste("Please select a player...")
      }
      
      if(input$conchart_playerA != "None" & input$conchart_playerB == "None" & input$conchart_playerC == "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerA,]
        s1 <- consd[input$conchart_playerA,]
        con <- t(rbind(p1,s1))
        colnames(con) <- c(paste(input$conchart_playerA,"Avg"),paste(input$conchart_playerA,"StdDev"))
      }
      
      if(input$conchart_playerA == "None" & input$conchart_playerB != "None" & input$conchart_playerC == "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerB,]
        s1 <- consd[input$conchart_playerB,]
        con <- t(rbind(p1,s1))
        colnames(con) <- c(paste(input$conchart_playerB,"Avg"),paste(input$conchart_playerB,"StdDev"))
      }
      
      if(input$conchart_playerA == "None" & input$conchart_playerB == "None" & input$conchart_playerC != "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerC,]
        s1 <- consd[input$conchart_playerC,]
        con <- t(rbind(p1,s1))
        colnames(con) <- c(paste(input$conchart_playerC,"Avg"),paste(input$conchart_playerC,"StdDev"))
      }
      
      if(input$conchart_playerA != "None" & input$conchart_playerB != "None" & input$conchart_playerC == "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerA,]
        s1 <- consd[input$conchart_playerA,]
        p2 <- conavg[input$conchart_playerB,]
        s2 <- consd[input$conchart_playerB,]
        test <- rbind(p1,p2)
        test2 <- rbind(s1,s2)
        con <- t(rbind(p1,s1,p2,s2))
        colnames(con) <- c(paste(input$conchart_playerA,"Avg"),paste(input$conchart_playerA,"StdDev"),
                           paste(input$conchart_playerB,"Avg"),paste(input$conchart_playerB,"StdDev"))
      }
      
      if(input$conchart_playerA == "None" & input$conchart_playerB != "None" & input$conchart_playerC != "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerB,]
        s1 <- consd[input$conchart_playerB,]
        p2 <- conavg[input$conchart_playerC,]
        s2 <- consd[input$conchart_playerC,]
        test <- rbind(p1,p2)
        test2 <- rbind(s1,s2)
        con <- t(rbind(p1,s1,p2,s2))
        colnames(con) <- c(paste(input$conchart_playerB,"Avg"),paste(input$conchart_playerB,"StdDev"),
                           paste(input$conchart_playerC,"Avg"),paste(input$conchart_playerC,"StdDev"))
      }
      
      if(input$conchart_playerA != "None" & input$conchart_playerB == "None" & input$conchart_playerC != "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerA,]
        s1 <- consd[input$conchart_playerA,]
        p2 <- conavg[input$conchart_playerC,]
        s2 <- consd[input$conchart_playerC,]
        test <- rbind(p1,p2)
        test2 <- rbind(s1,s2)
        con <- t(rbind(p1,s1,p2,s2))
        colnames(con) <- c(paste(input$conchart_playerA,"Avg"),paste(input$conchart_playerA,"StdDev"),
                           paste(input$conchart_playerC,"Avg"),paste(input$conchart_playerC,"StdDev"))
      }
      
      if(input$conchart_playerA != "None" & input$conchart_playerB != "None" & input$conchart_playerC != "None"){
        conavg <- as.matrix(conyear[,2:9])
        rownames(conavg) <- conyear$player
        consd <- as.matrix(conyear[,10:17])
        rownames(consd) <- conyear$player
        p1 <- conavg[input$conchart_playerA,]
        s1 <- consd[input$conchart_playerA,]
        p2 <- conavg[input$conchart_playerB,]
        s2 <- consd[input$conchart_playerB,]
        p3 <- conavg[input$conchart_playerC,]
        s3 <- consd[input$conchart_playerC,]
        test <- rbind(p1,p2,p3)
        test2 <- rbind(s1,s2,s3)
        con <- t(rbind(p1,s1,p2,s2,p3,s3))
        colnames(con) <- c(paste(input$conchart_playerA,"Avg"),paste(input$conchart_playerA,"StdDev"),
                           paste(input$conchart_playerB,"Avg"),paste(input$conchart_playerB,"StdDev"),
                           paste(input$conchart_playerC,"Avg"),paste(input$conchart_playerC,"StdDev") )
      }
      
      con
      
    },options = list(dom = 't'))
  })
  
  #Print Start/Sit Tool
  output$probA <- renderText({
    
    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsA <- input$con_number
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
      pointsA <- input$con_number
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
      pointsA <- input$con_number
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
      pointsA <- input$con_number
      count <- 0
      
      p1 <- x[(input$con_playerA),]
      
      for (i in 1:length(p1)) {
        if(p1[i] >= pointsA & !is.na(p1[i])) {
          count = sum(p1>=pointsA, na.rm = TRUE)
        }
      }
      a <- count/length(na.omit(p1))
    }
    
    paste("Probability",input$con_playerA,">=",input$con_number,": ",signif(a, digits = 4)*100,"%")
    
  })
  
  output$probB <- renderText({
    
    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      pointsB <- input$con_number
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
      pointsB <- input$con_number
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
      pointsB <- input$con_number
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
      pointsB <- input$con_number
      count <- 0
      
      p2 <- x[(input$con_playerB),]
      
      for (i in 1:length(p2)) {
        if(p2[i] >= pointsB & !is.na(p2[i])) {
          count = sum(p2>=pointsB, na.rm = TRUE)
        }
      }
      b <- count/length(na.omit(p2))
    }
    
    paste("Probability",input$con_playerB,">=",input$con_number,": ",signif(b, digits = 4)*100,"%")
    
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
    
    hist(p1, col = "red", main = paste("Histogram of", input$con_playerA, "&", input$con_playerB), xlab = "Fantasy Points", ylim = c(0,15),xlim = c(-5,50))
    hist(p2, col = rgb(0,1,0,0.5), add = TRUE)
    legend("topright",c(input$con_playerA,input$con_playerB),col = c("red",rgb(0,1,0,0.5)), lwd = 8)
  })
  
  output$con_tableB <- renderDataTable({
    DT::datatable({
    
    if(input$format == "PPR (4pt/TD)") {
      x <- as.matrix(weekly_data[1576:2098,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
      z <- t(x)
      rownames(z) <- c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17",
                       "Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17")
      last8A <- z[c(17,16,15,14,13,12,11,10),input$con_playerA]
      last8B <- z[c(17,16,15,14,13,12,11,10),input$con_playerB]
      last8C <- cbind(last8A,last8B)
      colnames(last8C) <- c(input$con_playerA,input$con_playerB)
    }
    
    if(input$format == "PPR (6pt/TD)") {
      x <- as.matrix(weekly_data[1051:1573,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
      z <- t(x)
      rownames(z) <- c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17",
                       "Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17")
      last8A <- z[c(17,16,15,14,13,12,11,10),input$con_playerA]
      last8B <- z[c(17,16,15,14,13,12,11,10),input$con_playerB]
      last8C <- cbind(last8A,last8B)
      colnames(last8C) <- c(input$con_playerA,input$con_playerB)
    }
    
    if(input$format == "1/2PPR") {
      x <- as.matrix(weekly_data[1:523,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
      z <- t(x)
      rownames(z) <- c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17",
                       "Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17")
      last8A <- z[c(17,16,15,14,13,12,11,10),input$con_playerA]
      last8B <- z[c(17,16,15,14,13,12,11,10),input$con_playerB]
      last8C <- cbind(last8A,last8B)
      colnames(last8C) <- c(input$con_playerA,input$con_playerB)
    }
    
    if(input$format == "Standard") {
      x <- as.matrix(weekly_data[526:1048,2:35])
      y <- weekly_data[1576:2098,1]
      rownames(x) <- y
      p1 <- x[(input$con_playerA),]
      p1 <- as.numeric(p1)
      p2 <- x[(input$con_playerB),]
      p2 <- as.numeric(p2)
      z <- t(x)
      rownames(z) <- c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17",
                       "Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7","Week 8","Week 9","Week 10",
                       "Week 11","Week 12","Week 13","Week 14","Week 15","Week 16","Week 17")
      last8A <- z[c(17,16,15,14,13,12,11,10),input$con_playerA]
      last8B <- z[c(17,16,15,14,13,12,11,10),input$con_playerB]
      last8C <- cbind(last8A,last8B)
      colnames(last8C) <- c(input$con_playerA,input$con_playerB)
    }
    
    last8C
      
    }, options = list(dom = 't'))
    
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
  }, rownames = FALSE, filter = "top",options = list(lengthMenu = c(12,24,36,50)))
  
  output$weekly_graph <- renderPlot({
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      paste("Please select a player...")
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyA), col = "red",
              xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyA),col = c("red"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyB), col = "blue",
              xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyB),col = c("blue"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyC]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyC), col = rgb(0,1,0,0.5),
              xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyC),col = c(rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyB), beside = TRUE, col = c("red","blue"), xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyA,input$player_weeklyB),col = c("red","blue"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyB, "&", input$player_weeklyC), beside = TRUE, col = c("blue",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyB,input$player_weeklyC),col = c("blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyC), beside = TRUE, col = c("red",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyA,input$player_weeklyC),col = c("red",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      p3 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2,p3)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyB, "&", input$player_weeklyC), beside = TRUE, col = c("red","blue",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Frequency", ylim = c(0,100))
      legend("topright",c(input$player_weeklyA,input$player_weeklyB,input$player_weeklyC),col = c("red","blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      paste("Please select a player...")
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyA), col = "red",
              xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyA),col = c("red"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyB), col = "blue",
              xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyB),col = c("blue"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyC]
      both <- p1
      barplot(both, main = paste("Weekly Finishes for", input$player_weeklyC), col = rgb(0,1,0,0.5),
              xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyC),col = c(rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyB), beside = TRUE, col = c("red","blue"), xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyA,input$player_weeklyB),col = c("red","blue"), lwd = 8)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyB, "&", input$player_weeklyC), beside = TRUE, col = c("blue",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyB,input$player_weeklyC),col = c("blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyC), beside = TRUE, col = c("red",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyA,input$player_weeklyC),col = c("red",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      p3 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2,p3)
      barplot(both, main = paste("Comparison of", input$player_weeklyA, "&", input$player_weeklyB, "&", input$player_weeklyC), beside = TRUE, col = c("red","blue",rgb(0,1,0,0.5)), xlab = "Weekly Finish", ylab = "Percentage", ylim = c(0,70))
      legend("topright",c(input$player_weeklyA,input$player_weeklyB,input$player_weeklyC),col = c("red","blue",rgb(0,1,0,0.5)), lwd = 8)
    }
  })
  
  output$weekly_table <- renderDataTable({
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      both <- paste("Please select a player...")
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      both <- p1
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      both <- p1
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyC]
      both <- p1
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyB)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyB,input$player_weeklyC)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyC)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Total"){
      x <- as.matrix(finish[,2:5])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      p3 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2,p3)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyB,input$player_weeklyC)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      both <- paste("Please select a player...")
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      both <- p1
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      both <- p1
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyC]
      both <- p1
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC == "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyB)
    }
    
    if(input$player_weeklyA == "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyB]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyB,input$player_weeklyC)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB == "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyC)
    }
    
    if(input$player_weeklyA != "None" & input$player_weeklyB != "None" &input$player_weeklyC != "None" & input$tog_wf == "Percent"){
      x <- as.matrix(finish[,6:9])
      rownames(x) <- finish$Player
      y <- t(x)
      p1 <- y[,input$player_weeklyA]
      p2 <- y[,input$player_weeklyB]
      p3 <- y[,input$player_weeklyC]
      both <- rbind(p1,p2,p3)
      rownames(both) <- c(input$player_weeklyA,input$player_weeklyB,input$player_weeklyC)
    }
    
    DT::datatable({
      t(both)
    },options = list(dom = 't'))
  })
  
  #Weekly Tool
  output$weekly_tool <- DT::renderDataTable({
    
    if(input$wt_year == "2017"){
      try <- twp_2017[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2017$player
      Position <- twp_2017$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2016"){
      try <- twp_2016[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2016$player
      Position <- twp_2016$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2015"){
      try <- twp_2015[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2015$player
      Position <- twp_2015$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2014"){
      try <- twp_2014[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2014$player
      Position <- twp_2014$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2013"){
      try <- twp_2013[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2013$player
      Position <- twp_2013$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2012"){
      try <- twp_2012[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2012$player
      Position <- twp_2012$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2011"){
      try <- twp_2011[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2011$player
      Position <- twp_2011$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    if(input$wt_year == "2010"){
      try <- twp_2010[,7:23]
      TotalFPts <- rowSums(try[,c(input$wt_weekA:input$wt_weekB)], na.rm = TRUE)
      Player <- twp_2010$player
      Position <- twp_2010$pos
      try2 <- cbind(Player,Position,try[,c(input$wt_weekA:input$wt_weekB)],TotalFPts)
    }
    
    DT::datatable({
    try2 <- try2[order(-try2$TotalFPts),]
    },rownames = FALSE, filter = "top")
  })
  
  #Yearly Data
  output$yearly <- DT::renderDataTable({
    DT::datatable({
      
      if (input$tog_yearly == "Points" & input$pos_yearly == "All") {
        yearly <- yearly_points
      }
      
      if (input$tog_yearly == "Points" & input$pos_yearly != "All") {
        yearly <- yearly_points
        yearly <- yearly_points[yearly_points$pos == input$pos_yearly,]
      }
      
      if (input$tog_yearly == "Rank" & input$pos_yearly == "All") {
        yearly <- yearly_rank
      }
      
      if (input$tog_yearly == "Rank" & input$pos_yearly != "All") {
        yearly <- yearly_rank
        yearly <- yearly_rank[yearly_rank$pos == input$pos_yearly,]
      }
    })
    yearly <- yearly[order(yearly$pos),]
  },rownames = FALSE, filter = "top",options = list(lengthMenu = c(12,24,36,50)))
  
  #Yearly Graph
  output$yearly_graph <- renderPlot({
    
    if(input$player_yearlyA == "None" & input$player_yearlyB == "None" & input$player_yearlyC == "None"){
     paste("Please select a player...")
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB == "None" & input$player_yearlyC == "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyA),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      
      plot(p1,type = "l",axes = FALSE,xlim = c(1,8),ylim = rev(c(1,p1max)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyA), col = "red")
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:p1max))
      legend("bottomleft",c(input$player_yearlyA),col = c("red"), lwd = 8)
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB != "None" & input$player_yearlyC == "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyB),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      
      plot(p1,type = "l",axes = FALSE,xlim = c(1,8),ylim = rev(c(1,p1max)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyB), col = "blue")
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:p1max))
      legend("bottomleft",c(input$player_yearlyB),col = c("blue"), lwd = 8)
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB == "None" & input$player_yearlyC != "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyC),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      
      plot(p1,type = "l",axes = FALSE,xlim = c(1,8),ylim = rev(c(1,p1max)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyC), col = rgb(0,1,0,0.5))
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:p1max))
      legend("bottomleft",c(input$player_yearlyC),col = c(rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB != "None" & input$player_yearlyC == "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyA),]
      p2 <- y[(input$player_yearlyB),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      p2max <- mround(max(p2, na.rm = TRUE),5)
      bothmax <- max(rbind(p1max,p2max))
      
      plot(p1,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyA, "&", input$player_yearlyB), col = c("red","blue"))
      par(new = TRUE)
      plot(p2,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "",
           ylab = "", col = "blue")
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:bothmax))
      legend("bottomleft",c(input$player_yearlyA,input$player_yearlyB),col = c("red","blue"), lwd = 8)
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB != "None" & input$player_yearlyC != "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyB),]
      p2 <- y[(input$player_yearlyC),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      p2max <- mround(max(p2, na.rm = TRUE),5)
      bothmax <- max(rbind(p1max,p2max))
      
      plot(p1,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyB, "&", input$player_yearlyC), col = c("blue",rgb(0,1,0,0.5)))
      par(new = TRUE)
      plot(p2,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "",
           ylab = "", col = rgb(0,1,0,0.5))
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:bothmax))
      legend("bottomleft",c(input$player_yearlyB,input$player_yearlyC),col = c("blue",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB == "None" & input$player_yearlyC != "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyA),]
      p2 <- y[(input$player_yearlyC),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      p2max <- mround(max(p2, na.rm = TRUE),5)
      bothmax <- max(rbind(p1max,p2max))
      
      plot(p1,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyA, "&", input$player_yearlyC), col = c("red",rgb(0,1,0,0.5)))
      par(new = TRUE)
      plot(p2,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,bothmax)),xlab = "",
           ylab = "", col = rgb(0,1,0,0.5))
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:bothmax))
      legend("bottomleft",c(input$player_yearlyA,input$player_yearlyC),col = c("red",rgb(0,1,0,0.5)), lwd = 8)
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB != "None" & input$player_yearlyC != "None"){
      mround <- function(x,base){ 
        base*ceiling(x/base) 
      } 
      
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      y <- x[,ncol(x):1]
      p1 <- y[(input$player_yearlyA),]
      p2 <- y[(input$player_yearlyB),]
      p3 <- y[(input$player_yearlyC),]
      p1max <- mround(max(p1, na.rm = TRUE),5)
      p2max <- mround(max(p2, na.rm = TRUE),5)
      p3max <- mround(max(p3, na.rm = TRUE),5)
      allmax <- max(rbind(p1max,p2max,p3max))
      
      plot(p1,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,allmax)),xlab = "Year",
           ylab = "Position Finish",main = paste("Yearly Finishes for", input$player_yearlyA, "&", input$player_yearlyB, "&", input$player_yearlyC), col = c("red","blue",rgb(0,1,0,0.5)))
      par(new = TRUE)
      plot(p2,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,allmax)),xlab = "",
           ylab = "", col = "blue")
      par(new=TRUE)
      plot(p3,type = "l",axes = FALSE, xlim = c(1,8), ylim = rev(c(1,allmax)),xlab = "",
           ylab = "", col = rgb(0,1,0,0.5))
      axis(1, at=1:8, lab=c("'10","'11","'12","'13","'14","'15","'16","'17"))
      axis(2, at=c(1,1:allmax))
      legend("bottomleft",c(input$player_yearlyA,input$player_yearlyB,input$player_yearlyC),col = c("red","blue",rgb(0,1,0,0.5)), lwd = 8)
    }
  })
  
  output$yearly_posrank <- DT::renderDataTable({
    
    if(input$player_yearlyA == "None" & input$player_yearlyB == "None" & input$player_yearlyC == "None"){
      y <- paste("Please select a player...")
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB == "None" & input$player_yearlyC == "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- t(test[,input$player_yearlyA])
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB != "None" & input$player_yearlyC == "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- t(test[,input$player_yearlyB])
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB == "None" & input$player_yearlyC != "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- t(test[,input$player_yearlyC])
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB != "None" & input$player_yearlyC == "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- test[,c(input$player_yearlyA,input$player_yearlyB)]
    }
    
    if(input$player_yearlyA == "None" & input$player_yearlyB != "None" & input$player_yearlyC != "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- test[,c(input$player_yearlyB,input$player_yearlyC)]
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB == "None" & input$player_yearlyC != "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- test[,c(input$player_yearlyA,input$player_yearlyC)]
    }
    
    if(input$player_yearlyA != "None" & input$player_yearlyB != "None" & input$player_yearlyC != "None"){
      x <- as.matrix(yearly[,4:11])
      rownames(x) <- yearly$player
      colnames(x) <- c("2017","2016","2015","2014","2013","2012","2011","2010")
      test <- t(x)
      y <- test[,c(input$player_yearlyA,input$player_yearlyB,input$player_yearlyC)]
    }

    DT::datatable({
      y
    }, options = list(dom = 't'))
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
    
    },rownames = FALSE, filter = "top", options = list(pageLength = 20))
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
      
    },rownames = FALSE, filter = "top", options = list(pageLength = 35))
  })
  
  
  #QB Database
  output$qbdata <- DT::renderDataTable({
    DT::datatable(
    
      if(!is.null(input$qb_vars)) {
        qbdata[,input$qb_vars, drop = FALSE]
      }
      , rownames = FALSE, filter = "top", 
      options = list(lengthMenu = c(10,25,50,100))

    )
  })
  
  #RB Database
  output$rbdata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$rb_vars)) {
        rbdata[,input$rb_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top",
      options = list(lengthMenu = c(10,25,50,100))
      
    )
  })
    
  #WR Database
  output$wrdata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$wr_vars)) {
        wrdata[,input$wr_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top",
      options = list(lengthMenu = c(10,25,50,100))
      
    ) 
    
  })
  
  #TE Database
  output$tedata <- DT::renderDataTable({
    DT::datatable(
      
      if(!is.null(input$te_vars)) {
        tedata[,input$te_vars, drop = FALSE]
      }
      , rownames = FALSE,filter = "top",
      options = list(lengthMenu = c(10,25,50,100))
      
    ) 
    
  })

  output$offyearly <- DT::renderDataTable({
    
    fanptsqb <- offyearqb$FP + (offyearqb$PassAtt*input$off_numberA) + (offyearqb$PassComp*input$off_numberB) +
      ((offyearqb$PassYards*input$off_numberC)-(offyearqb$PassYards/25)) +
      ((offyearqb$PassTDs*input$off_numberD)-(offyearqb$PassTDs*4)) -
      ((offyearqb$INTs*input$off_numberE)+(offyearqb$INTs*2)) +
      (offyearqb$RushAtt*input$off_numberF) + ((offyearqb$RushYards*input$off_numberH)-(offyearqb$RushYards*0.1)) +
      ((offyearqb$RushTDs*input$off_numberI)-(offyearqb$RushTDs*6)) + (offyearqb$ReturnYards*input$off_numberK)
    avgqb <- round(fanptsqb/offyearqb$Games,2)
    
    fanptsrb <- offyearrb$FP + (offyearrb$RushAtt*input$off_numberF) + 
      ((offyearrb$RushYards*input$off_numberH)-(offyearrb$RushYards*0.1)) +
      ((offyearrb$RushTDs*input$off_numberI)-(offyearrb$RushTDs*6)) +
      ((offyearrb$Receptions*input$off_numberG)-offyearrb$Receptions) +
      ((offyearrb$RecYards*input$off_numberH)-(offyearrb$RecYards*0.1)) +
      ((offyearrb$RecTDs*input$off_numberI)-(offyearrb$RecTDs*6)) + (offyearrb$ReturnYards*input$off_numberK)
    avgrb <- round(fanptsrb/offyearrb$Games,2)
    
    fanptswr <- offyearwr$FP + 
      ((offyearwr$Receptions*input$off_numberG)-offyearwr$Receptions) +
      ((offyearwr$RecYards*input$off_numberH)-(offyearwr$RecYards*0.1)) +
      ((offyearwr$RecTDs*input$off_numberI)-(offyearwr$RecTDs*6)) + (offyearwr$ReturnYards*input$off_numberK)
    avgwr <- round(fanptswr/offyearwr$Games,2)
    
    fanptste <- offyearte$FP + 
      ((offyearte$Receptions*input$off_numberJ)-offyearte$Receptions) +
      ((offyearte$RecYards*input$off_numberH)-(offyearte$RecYards*0.1)) +
      ((offyearte$RecTDs*input$off_numberI)-(offyearte$RecTDs*6)) + (offyearte$ReturnYards*input$off_numberK)
    avgte <- round(fanptste/offyearte$Games,2)
    
    fanptsflex <- offyearflex$FP + (offyearflex$RushAtt*input$off_numberF) + 
      ((offyearflex$RushYards*input$off_numberH)-(offyearflex$RushYards*0.1)) +
      ((offyearflex$RushTDs*input$off_numberI)-(offyearflex$RushTDs*6)) +
      ((offyearflex$Receptions*input$off_numberG)-offyearflex$Receptions) +
      ((offyearflex$RecYards*input$off_numberH)-(offyearflex$RecYards*0.1)) +
      ((offyearflex$RecTDs*input$off_numberI)-(offyearflex$RecTDs*6)) + (offyearflex$ReturnYards*input$off_numberK)
    avgflex <- round(fanptsflex/offyearflex$Games,2)
    
    
    DT::datatable({
      
      if(input$offyear_pos == "QB") {
        offyearly <- cbind(offyearqb2, fanptsqb, avgqb)
        offyearly <- setNames(offyearly, c("Year","Player","Team","Games","PassAtt","PassComp","Comp%","PassYards",
                                       "PassTDs","INTs","RushAtt","RushYards","RushTDs","FanPts","Avg"))
      }
      
      if(input$offyear_pos == "RB") {
        offyearly <- cbind(offyearrb2, fanptsrb, avgrb)
        offyearly <- setNames(offyearly, c("Year","Player","Team","Games","RushAtt","RushYards","YPC","RushTDs","Targets",
                                           "Receptions","Reception%","RecYards","RecTDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$offyear_pos == "WR") {
        offyearly <- cbind(offyearwr2, fanptswr, avgwr)
        offyearly <- setNames(offyearly, c("Year","Player","Team","Games","Targets","Receptions",
                                           "Reception%","RecYards","RecTDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$offyear_pos == "TE") {
        offyearly <- cbind(offyearte2, fanptste, avgte)
        offyearly <- setNames(offyearly, c("Year","Player","Team","Games","Targets","Receptions",
                                           "Reception%","RecYards","RecTDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$offyear_pos == "Flex") {
        offyearly <- cbind(offyearflex2, fanptsflex, avgflex)
        offyearly <- setNames(offyearly, c("Year","Player","Pos","Team","Games","RushAtt","RushYards","YPC","RushTDs","Targets",
                                           "Receptions","Reception%","RecYards","RecTDs","ReturnYards","FanPts","Avg"))
      }

    offyearly[order(c(-offyearly$Year,-offyearly$FanPts)),]
      
    }, rownames = FALSE, filter = "top", options = list(lengthMenu = c(12,24,36,50,100)))
    
  })
  
  output$defyearly <- renderDataTable({
    
    fanptsdl <- idpyeardl$FP + ((idpyeardl$Tackle*input$def_numberA)-idpyeardl$Tackle) +
      ((idpyeardl$Assists*input$def_numberB)-(idpyeardl$Assists*0.5)) +
      ((idpyeardl$Sacks*input$def_numberC)-(idpyeardl$Sacks*4)) +
      ((idpyeardl$PassDef*input$def_numberD)-idpyeardl$PassDef) +
      ((idpyeardl$INTs*input$def_numberE)-(idpyeardl$INTs*5)) +
      ((idpyeardl$FumbleForced*input$def_numberF)-(idpyeardl$FumbleForced*3)) +
      ((idpyeardl$FumbleRec*input$def_numberG)-(idpyeardl$FumbleRec*2)) +
      ((idpyeardl$Safeties*input$def_numberH)-(idpyeardl$Safeties*2)) +
      ((idpyeardl$TDs*input$def_numberI)-(idpyeardl$TDs*6)) + (idpyeardl$ReturnYards*input$def_numberJ)
    avgdl <- round(fanptsdl/idpyeardl$Games,2)
    
    fanptsdb <- idpyeardb$FP + ((idpyeardb$Tackle*input$def_numberA)-idpyeardb$Tackle) +
      ((idpyeardb$Assists*input$def_numberB)-(idpyeardb$Assists*0.5)) +
      ((idpyeardb$Sacks*input$def_numberC)-(idpyeardb$Sacks*4)) +
      ((idpyeardb$PassDef*input$def_numberD)-idpyeardb$PassDef) +
      ((idpyeardb$INTs*input$def_numberE)-(idpyeardb$INTs*5)) +
      ((idpyeardb$FumbleForced*input$def_numberF)-(idpyeardb$FumbleForced*3)) +
      ((idpyeardb$FumbleRec*input$def_numberG)-(idpyeardb$FumbleRec*2)) +
      ((idpyeardb$Safeties*input$def_numberH)-(idpyeardb$Safeties*2)) +
      ((idpyeardb$TDs*input$def_numberI)-(idpyeardb$TDs*6)) + (idpyeardb$ReturnYards*input$def_numberJ)
    avgdb <- round(fanptsdb/idpyeardb$Games,2)
    
    fanptslb <- idpyearlb$FP + ((idpyearlb$Tackle*input$def_numberA)-idpyearlb$Tackle) +
      ((idpyearlb$Assists*input$def_numberB)-(idpyearlb$Assists*0.5)) +
      ((idpyearlb$Sacks*input$def_numberC)-(idpyearlb$Sacks*4)) +
      ((idpyearlb$PassDef*input$def_numberD)-idpyearlb$PassDef) +
      ((idpyearlb$INTs*input$def_numberE)-(idpyearlb$INTs*5)) +
      ((idpyearlb$FumbleForced*input$def_numberF)-(idpyearlb$FumbleForced*3)) +
      ((idpyearlb$FumbleRec*input$def_numberG)-(idpyearlb$FumbleRec*2)) +
      ((idpyearlb$Safeties*input$def_numberH)-(idpyearlb$Safeties*2)) +
      ((idpyearlb$TDs*input$def_numberI)-(idpyearlb$TDs*6)) + (idpyearlb$ReturnYards*input$def_numberJ)
    avglb <- round(fanptslb/idpyearlb$Games,2)
    
    fanpts <- idpyear$FP + ((idpyear$Tackle*input$def_numberA)-idpyear$Tackle) +
      ((idpyear$Assists*input$def_numberB)-(idpyear$Assists*0.5)) +
      ((idpyear$Sacks*input$def_numberC)-(idpyear$Sacks*4)) +
      ((idpyear$PassDef*input$def_numberD)-idpyear$PassDef) +
      ((idpyear$INTs*input$def_numberE)-(idpyear$INTs*5)) +
      ((idpyear$FumbleForced*input$def_numberF)-(idpyear$FumbleForced*3)) +
      ((idpyear$FumbleRec*input$def_numberG)-(idpyear$FumbleRec*2)) +
      ((idpyear$Safeties*input$def_numberH)-(idpyear$Safeties*2)) +
      ((idpyear$TDs*input$def_numberI)-(idpyear$TDs*6)) + (idpyear$ReturnYards*input$def_numberJ)
    avg <- round(fanpts/idpyear$Games,2)
      
      if(input$defyear_pos == "DL") {
        defyearly <- cbind(idpyeardl2,fanptsdl,avgdl)
        defyearly <- setNames(defyearly, c("Year","Player","Team","Games","Tackles","Assists",
                                           "Sacks","PassDef","INTs","FumbleForced","FumbleRec","Safeties",
                                           "TDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$defyear_pos == "DB") {
        defyearly <- cbind(idpyeardb2,fanptsdb,avgdb)
        defyearly <- setNames(defyearly, c("Year","Player","Team","Games","Tackles","Assists",
                                           "Sacks","PassDef","INTs","FumbleForced","FumbleRec","Safeties",
                                           "TDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$defyear_pos == "LB") {
        defyearly <- cbind(idpyearlb2,fanptslb,avglb)
        defyearly <- setNames(defyearly, c("Year","Player","Team","Games","Tackles","Assists",
                                           "Sacks","PassDef","INTs","FumbleForced","FumbleRec","Safeties",
                                           "TDs","ReturnYards","FanPts","Avg"))
      }
      
      if(input$defyear_pos == "All") {
        defyearly <- cbind(idpyear2,fanpts,avg)
        defyearly <- setNames(defyearly, c("Year","Player","Pos","Team","Games","Tackles","Assists",
                                           "Sacks","PassDef","INTs","FumbleForced","FumbleRec","Safeties",
                                           "TDs","ReturnYards","FanPts","Avg"))
      }
      
    DT::datatable({
    defyearly <- defyearly[order(c(-defyearly$Year,-defyearly$FanPts)),]
    }, rownames = FALSE,filter = "top", options = list(lengthMenu = c(12,24,36,50,100)))

  })
  
  output$zscoretable <- renderDataTable({
    
    if(input$z_pos == "QB"){
      zscore <- zscoreqb
    }
    
    if(input$z_pos == "RB"){
      zscore <- zscorerb
    }
    
    if(input$z_pos == "WR"){
      zscore <- zscorewr
    }
    
    if(input$z_pos == "TE"){
      zscore <- zscorete
    }
    zscore
  }, rownames = FALSE,filter = "top", options = list(lengthMenu = c(12,24,36,50,100)))
  
  output$z_graph <- renderPlot({
    
    if(input$z_tog == "Age" & input$z_pos2 == "QB"){
      bp <- barplot(zscoreqbage$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for QB by Age",col = ifelse(zscoreqbage$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscoreqbage$Age)
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "RB"){
      bp <- barplot(zscorerbage$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for RB by Age",col = ifelse(zscorerbage$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscorerbage$Age)
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "WR"){
      bp <- barplot(zscorewrage$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for WR by Age",col = ifelse(zscorewrage$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscorewrage$Age)
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "TE"){
      bp <- barplot(zscoreteage$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for TE by Age",col = ifelse(zscoreteage$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscoreteage$Age)
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "QB"){
      bp <- barplot(zscoreqbseason$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for QB by Season",col = ifelse(zscoreqbseason$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscoreqbseason$Season)
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "RB"){
      bp <- barplot(zscorerbseason$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for RB by Season",col = ifelse(zscorerbseason$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscorerbseason$Season)
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "WR"){
      bp <- barplot(zscorewrseason$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for WR by Season",col = ifelse(zscorewrseason$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscorewrseason$Season)
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "TE"){
      bp <- barplot(zscoreteseason$Average,ylim = c(-1,1),xlab = "Age",ylab = "Average Z-Score",main = "Average Z-Score for TE by Season",col = ifelse(zscoreteseason$Average>0,"blue","red"))
      axis(1, at=bp, labels = zscoreteseason$Season)
    }
    
  })
  
  output$z_graphtable <- renderDataTable({
    
    if(input$z_tog == "Age" & input$z_pos2 == "QB"){
      zscore <- zscoreqbage
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "RB"){
      zscore <- zscorerbage
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "WR"){
      zscore <- zscorewrage
    }
    
    if(input$z_tog == "Age" & input$z_pos2 == "TE"){
      zscore <- zscoreteage
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "QB"){
      zscore <- zscoreqbseason
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "RB"){
      zscore <- zscorerbseason
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "WR"){
      zscore <- zscorewrseason
    }
    
    if(input$z_tog == "Season" & input$z_pos2 == "TE"){
      zscore <- zscoreteseason
    }
    zscore
  },rownames = FALSE,options = list(dom = 't',paging=FALSE))
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Consistency" = consistency, "Weekly" = total_weekly_data, "Yearly" = yearly,
           "Defenses" = defenses,"Defenses (Avg)" = defenses_avg,"QB Database" = qbdata,
           "RB Database" = rbdata,"WR Database" = wrdata,"TE Database" = tedata,
           "IDP Database" = idpyear)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

