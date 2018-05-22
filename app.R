# DFF Rankings

library(shiny)
library(shinydashboard)
library(DT)

url <- a("Devy Watch", href="https://dynastyfootballfactory.com/2018-devy-watch-dff/", target="_blank")

rankings <- read.csv("rankings.csv")
posrankings <- read.csv("posrankings.csv")
colnames(rankings) <- c("Player","Pos","Team","Consensus","Cipes","Crandall","Hayes",
                        "Walker","Player","Pos","Team","Consensus","August","Crandall","Sinclair",
                        "Stephenson","Player","Pos","Team","Consensus","Hogue","Koutoulas","Manila",
                        "Rasmussen","Player","Pos","Team","Consensus","Cook","Orr",
                        "Osterloh","Winstead","Player","Pos","Consensus","Brandt",
                        "Francis","Stefan","Player","Pos","Consensus","Cook","Johnson",
                        "Orr","Winstead","Player","Pos","School",
                        "Year","Consensus","Chaney","DiRienzo","Garrett","McDaniel",
                        "Player","Pos","School","Year","Consensus","Cook","Hanmore",
                        "Orr","Winstead","Player","School","Pos","Consensus",
                        "Francis","Mount","Player","Position","Consensus","Cook","Johnson","Winstead")
colnames(posrankings) <- c("Player","Pos","Team","Consensus","Cipes","Crandall","Hayes",
                           "Walker","Player","Pos","Team","Consensus","August","Crandall","Sinclair",
                           "Stephenson","Player","Pos","Team","Consensus","Hogue","Koutoulas","Manila",
                           "Rasmussen","Player","Pos","Team","Consensus","Cook","Orr",
                           "Osterloh","Winstead","Player","Pos","Consensus","Brandt",
                           "Francis","Stefan","Player","Pos","Consensus","Cook","Johnson",
                           "Orr","Winstead","Player","Pos","School",
                           "Year","Consensus","Chaney","DiRienzo","Garrett","McDaniel",
                           "Player","Pos","School","Year","Consensus","Cook","Hanmore",
                           "Orr","Winstead","Player","School","Pos","Consensus",
                           "Francis","Mount","Player","Position","Consensus","Cook","Johnson","Winstead")


dynasty <- rankings[1:250,1:8]
redraft <- rankings[1:300,9:16]
superflex <- rankings[1:250,17:24]
idp <- rankings[1:100,25:32]
rookie <- rankings[1:75,33:38]
idprookie <- rankings[1:45,39:45]
devy <- rankings[1:20,46:54]
idpdevy <- rankings[1:30,55:63]
cff <- rankings[1:300,64:69]
combo <- rankings[1:100,70:75]

dynastypos <- posrankings[,1:8]
posdynasty <- dynastypos$Pos
dynastyqb <- subset(dynastypos,posdynasty == "QB")
dynastyqbrank <- rank(dynastyqb[,4])
dynastyqb2 <- subset(dynastyqb, dynastyqbrank <= 30)
dynastyrb <- subset(dynastypos,posdynasty == "RB")
dynastyrbrank <- rank(dynastyrb[,4])
dynastyrb2 <- subset(dynastyrb, dynastyrbrank <= 75)
dynastywr <- subset(dynastypos,posdynasty == "WR")
dynastywrrank <- rank(dynastywr[,4])
dynastywr2 <- subset(dynastywr, dynastywrrank <= 100)
dynastyte <- subset(dynastypos,posdynasty == "TE")
dynastyterank <- rank(dynastyte[,4])
dynastyte2 <- subset(dynastyte, dynastyterank <= 35)

redraftpos <- posrankings[,9:16]
posredraft <- redraftpos$Pos
redraftqb <- subset(redraftpos,posredraft == "QB")
redraftqbrank <- rank(redraftqb[,4])
redraftqb2 <- subset(redraftqb, redraftqbrank <= 30)
redraftrb <- subset(redraftpos,posredraft == "RB")
redraftrbrank <- rank(redraftrb[,4])
redraftrb2 <- subset(redraftrb, redraftrbrank <= 90)
redraftwr <- subset(redraftpos,posredraft == "WR")
redraftwrrank <- rank(redraftwr[,4])
redraftwr2 <- subset(redraftwr, redraftwrrank <= 115)
redraftte <- subset(redraftpos,posredraft == "TE")
redraftterank <- rank(redraftte[,4])
redraftte2 <- subset(redraftte, redraftterank <= 40)

superflexpos <- posrankings[,17:24]
possuperflex <- superflexpos$Pos
superflexqb <- subset(superflexpos,possuperflex == "QB")
superflexqbrank <- rank(superflexqb[,4])
superflexqb2 <- subset(superflexqb, superflexqbrank <= 35)
superflexrb <- subset(superflexpos,possuperflex == "RB")
superflexrbrank <- rank(superflexrb[,4])
superflexrb2 <- subset(superflexrb, superflexrbrank <= 75)
superflexwr <- subset(superflexpos,possuperflex == "WR")
superflexwrrank <- rank(superflexwr[,4])
superflexwr2 <- subset(superflexwr, superflexwrrank <= 100)
superflexte <- subset(superflexpos,possuperflex == "TE")
superflexterank <- rank(superflexte[,4])
superflexte2 <- subset(superflexte, superflexterank <= 30)

idppos <- posrankings[,25:32]
posidp <- idppos$Pos
idpdl <- subset(idppos,posidp == "DL")
idpdlrank <- rank(idpdl[,4])
idpdl2 <- subset(idpdl, idpdlrank <= 25)
idplb <- subset(idppos,posidp == "LB")
idplbrank <- rank(idplb[,4])
idplb2 <- subset(idplb, idplbrank <= 40)
idpdb <- subset(idppos,posidp == "DB")
idpdbrank <- rank(idpdb[,4])
idpdb2 <- subset(idpdb, idpdbrank <= 25)

rookiepos <- posrankings[,33:38]
posrookie <- rookiepos$Pos
rookieqb <- subset(rookiepos,posrookie == "QB")
rookieqbrank <- rank(rookieqb[,3])
rookieqb2 <- subset(rookieqb, rookieqbrank <= 8)
rookierb <- subset(rookiepos,posrookie == "RB")
rookierbrank <- rank(rookierb[,3])
rookierb2 <- subset(rookierb, rookierbrank <= 20)
rookiewr <- subset(rookiepos,posrookie == "WR")
rookiewrrank <- rank(rookiewr[,3])
rookiewr2 <- subset(rookiewr, rookiewrrank <= 25)
rookiete <- subset(rookiepos,posrookie == "TE")
rookieterank <- rank(rookiete[,3])
rookiete2 <- subset(rookiete, rookieterank <= 7)

idprookiepos <- posrankings[,39:45]
posidprookie <- idprookiepos$Pos
idprookiedl <- subset(idprookiepos,posidprookie == "DL")
idprookiedlrank <- rank(idprookiedl[,3])
idprookiedl2 <- subset(idprookiedl, idprookiedlrank <= 15)
idprookielb <- subset(idprookiepos,posidprookie == "LB")
idprookielbrank <- rank(idprookielb[,3])
idprookielb2 <- subset(idprookielb, idprookielbrank <= 15)
idprookiedb <- subset(idprookiepos,posidprookie == "DB")
idprookiedbrank <- rank(idprookiedb[,3])
idprookiedb2 <- subset(idprookiedb, idprookiedbrank <= 10)

devypos <- posrankings[,46:54]
posdevy <- devypos$Pos
devyqb <- subset(devypos,posdevy == "QB")
devyqbrank <- rank(devyqb[,5])
devyqb2 <- subset(devyqb, devyqbrank <= 3)
devyrb <- subset(devypos,posdevy == "RB")
devyrbrank <- rank(devyrb[,5])
devyrb2 <- subset(devyrb, devyrbrank <= 10)
devywr <- subset(devypos,posdevy == "WR")
devywrrank <- rank(devywr[,5])
devywr2 <- subset(devywr, devywrrank <= 10)
devyte <- subset(devypos,posdevy == "TE")
devyterank <- rank(devyte[,5])
devyte2 <- subset(devyte, devyterank <= 2)

idpdevypos <- posrankings[,55:63]
posidpdevy <- idpdevypos$Pos
idpdevydl <- subset(idpdevypos,posidpdevy == "DL")
idpdevydlrank <- rank(idpdevydl[,5])
idpdevydl2 <- subset(idpdevydl, idpdevydlrank <= 10)
idpdevylb <- subset(idpdevypos,posidpdevy == "LB")
idpdevylbrank <- rank(idpdevylb[,5])
idpdevylb2 <- subset(idpdevylb, idpdevylbrank <= 10)
idpdevydb <- subset(idpdevypos,posidpdevy == "DB")
idpdevydbrank <- rank(idpdevydb[,5])
idpdevydb2 <- subset(idpdevydb, idpdevydbrank <= 5)

cffpos <- posrankings[,64:69]
poscff <- cffpos$Pos
cffqb <- subset(cffpos,poscff == "QB")
cffqbrank <- rank(cffqb[,4])
cffqb2 <- subset(cffqb, cffqbrank <= 40)
cffrb <- subset(cffpos,poscff == "RB")
cffrbrank <- rank(cffrb[,4])
cffrb2 <- subset(cffrb, cffrbrank <= 40)
cffwr <- subset(cffpos,poscff == "WR")
cffwrrank <- rank(cffwr[,4])
cffwr2 <- subset(cffwr, cffwrrank <= 40)
cffte <- subset(cffpos,poscff == "TE")
cffterank <- rank(cffte[,4])
cffte2 <- subset(cffte, cffterank <= 40)

combopos <- posrankings[,70:75]
poscombo <- combopos$Pos
comboqb <- subset(combopos,poscombo == "QB")
comboqbrank <- rank(comboqb[,4])
comboqb2 <- subset(comboqb, comboqbrank <= 8)
comborb <- subset(combopos,poscombo == "RB")
comborbrank <- rank(comborb[,4])
comborb2 <- subset(comborb, comborbrank <= 20)
combowr <- subset(combopos,poscombo == "WR")
combowrrank <- rank(combowr[,4])
combowr2 <- subset(combowr, combowrrank <= 25)
combote <- subset(combopos,poscombo == "TE")
comboterank <- rank(combote[,4])
combote2 <- subset(combote, comboterank <= 7)
combodl <- subset(combopos,poscombo == "DL")
combodlrank <- rank(combodl[,3])
combodl2 <- subset(combodl, combodlrank <= 15)
combolb <- subset(combopos,poscombo == "LB")
combolbrank <- rank(combolb[,3])
combolb2 <- subset(combolb, combolbrank <= 15)
combodb <- subset(combopos,poscombo == "DB")
combodbrank <- rank(combodb[,3])
combodb2 <- subset(combodb, combodbrank <= 10)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "Factory Rankings"),
  
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dynasty Rankings", tabName = "dynasty", icon = icon("table")),
    menuItem("Redraft Rankings", tabName = "redraft", icon = icon("table")),
    menuItem("IDP Rankings", tabName = "idp", icon = icon("table")),
    menuItem("Rookie Rankings", tabName = "rookie", icon = icon("table")),
    menuItem("IDP Rookie Rankings", tabName = "idprookie", icon = icon("table")),
    menuItem("Combo Rookie Rankings", tabName = "combo", icon = icon("table")),
    menuItem("Devy Rankings", tabName = "devy", icon = icon("table")),
    menuItem("IDP Devy Rankings", tabName = "idpdevy", icon = icon("table"))
  )),
  
  dashboardBody(
    tags$head(tags$style(HTML(
      '.skin-black .sidebar-menu>li.active>a {
      color: #fff;
      background: #001126;
      border-left-color: #19477f;
      }
      .skin-black .sidebar-menu>li:hover>a {
      background: #102e52;
      border-left-color: #19477f;
      }
      .skin-black .sidebar a {
      color: #c1c1c1;
      }
      .sidebar {
      color: #FFF;
      position: fixed;
      width: 230px;
      white-space: nowrap;
      overflow: visible;
      background: #0c2443;
      }
      .main-header {
      position: fixed;
      width:100%;
      }
      .content-wrapper, .right-side {
      min-height: 100%;
      background-color: #efefef;
      z-index: 800;
      }
      table.dataTable thead th, table.dataTable thead td {
      padding: 10px 18px;
      border-bottom: 1px solid #111;
      background: #ffffff;
      }
      aside.main-sidebar {
      background: #0c2443!important;
      }
      .content {
      padding-top: 120px!important;
      }
      
      @media (min-width:768px) {
      .content {
      padding-top: 80px!important;
      }
      }
      @media (max-width:767px) {
      .skin-black .main-header>.logo {
      text-align: left;
      }
      }'))),
    
    tabItems(
      tabItem(tabName = "dynasty",
              fluidRow(
                column(4,selectInput("positionA", "Choose Position:",
                                     choices = c("All","QB","RB","WR","TE")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("dynasty")
                       )),
      tabItem(tabName = "redraft",
              fluidRow(
                column(4,selectInput("positionB", "Choose Position:",
                                     choices = c("All","QB","RB","WR","TE")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("redraft"))),
      tabItem(tabName = "idp",
              fluidRow(
                column(4,selectInput("positionD", "Choose Position:",
                                     choices = c("All","DL","LB","DB")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("idp"))),
      tabItem(tabName = "rookie",
              fluidRow(
                column(4,selectInput("positionE", "Choose Position:",
                                     choices = c("All","QB","RB","WR","TE")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("rookie"))),
      tabItem(tabName = "idprookie",
              fluidRow(
                column(4,selectInput("positionF", "Choose Position:",
                                     choices = c("All","DL","LB","DB")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("idprookie"))),
      tabItem(tabName = "combo",
              fluidRow(
                column(4,selectInput("positionI", "Choose Position:",
                                     choices = c("All","QB","RB","WR","TE","DL","LB","DB")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("combo"))),
      tabItem(tabName = "devy",
              fluidRow(
                column(4,selectInput("positionG", "Choose Position:",
                                     choices = c("All","QB","RB","WR","TE")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("devy")),
              tagList(strong("To see the full DFF Devy rankings, purchase the 2018"),url)              ),
      tabItem(tabName = "idpdevy",
              fluidRow(
                column(4,selectInput("positionH", "Choose Position:",
                                     choices = c("All","DL","LB","DB")))),
              fluidRow(style = "overflow-x: scroll; font-size: 12px",DT::dataTableOutput("idpdevy")))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
   
  output$dynasty <- DT::renderDataTable({
    DT::datatable({
      
      if(input$positionA == "All") {
        rankings <- dynasty[order(dynasty$Consensus),]
      }
      
      if(input$positionA == "QB") {
        rankings <- dynastyqb2
        rankings <- dynastyqb2[order(dynastyqb2$Consensus),]
      }
      
      if(input$positionA == "RB") {
        rankings <- dynastyrb2
        rankings <- dynastyrb2[order(dynastyrb2$Consensus),]
      }
      
      if(input$positionA == "WR") {
        rankings <- dynastywr2
        rankings <- dynastywr2[order(dynastywr2$Consensus),]
      }
      
      if(input$positionA == "TE") {
        rankings <- dynastyte2
        rankings <- dynastyte2[order(dynastyte2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$redraft <- DT::renderDataTable({
    DT::datatable({  
  
      if(input$positionB == "All") {
        rankings <- redraft[order(redraft$Consensus),]
      }
      
      if(input$positionB == "QB") {
        rankings <- redraftqb2
        rankings <- redraftqb2[order(redraftqb2$Consensus),]
      }
      
      if(input$positionB == "RB") {
        rankings <- redraftrb2
        rankings <- redraftrb2[order(redraftrb2$Consensus),]
      }
      
      if(input$positionB == "WR") {
        rankings <- redraftwr2
        rankings <- redraftwr2[order(redraftwr2$Consensus),]
      }
      
      if(input$positionB == "TE") {
        rankings <- redraftte2
        rankings <- redraftte2[order(redraftte2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$superflex <- DT::renderDataTable({
    DT::datatable({  
  
      if(input$positionC == "All") {
        rankings <- superflex[order(superflex$Consensus),]
      }
      
      if(input$positionC == "QB") {
        rankings <- superflexqb2
        rankings <- superflexqb2[order(superflexqb2$Consensus),]
      }
      
      if(input$positionC == "RB") {
        rankings <- superflexrb2
        rankings <- superflexrb2[order(superflexrb2$Consensus),]
      }
      
      if(input$positionC == "WR") {
        rankings <- superflexwr2
        rankings <- superflexwr2[order(superflexwr2$Consensus),]
      }
      
      if(input$positionC == "TE") {
        rankings <- superflexte2
        rankings <- superflexte2[order(superflexte2$Consensus),]
      }
    
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$idp <- DT::renderDataTable({
    DT::datatable({ 
  
      if(input$positionD == "All") {
        rankings <- idp[order(idp$Consensus),]
      }
      
      if(input$positionD == "DL") {
        rankings <- idpdl2
        rankings <- idpdl2[order(idpdl2$Consensus),]
      }
      
      if(input$positionD == "LB") {
        rankings <- idplb2
        rankings <- idplb2[order(idplb2$Consensus),]
      }
      
      if(input$positionD == "DB") {
        rankings <- idpdb2
        rankings <- idpdb2[order(idpdb2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$idprookie <- DT::renderDataTable({
    DT::datatable({ 
  
      if(input$positionF == "All") {
        rankings <- idprookie[order(idprookie$Consensus),]
      }
      
      if(input$positionF == "DL") {
        rankings <- idprookiedl2
        rankings <- idprookiedl2[order(idprookiedl2$Consensus),]
      }
      
      if(input$positionF == "LB") {
        rankings <- idprookielb2
        rankings <- idprookielb2[order(idprookielb2$Consensus),]
      }
      
      if(input$positionF == "DB") {
        rankings <- idprookiedb2
        rankings <- idprookiedb2[order(idprookiedb2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
  
  output$combo <- DT::renderDataTable({
    DT::datatable({ 
      
      if(input$positionI == "All") {
        rankings <- combo[order(combo$Consensus),]
      }
      
      if(input$positionI == "QB") {
        rankings <- comboqb2
        rankings <- comboqb2[order(comboqb2$Consensus),]
      }
      
      if(input$positionI == "RB") {
        rankings <- comborb2
        rankings <- comborb2[order(comborb2$Consensus),]
      }
      
      if(input$positionI == "WR") {
        rankings <- combowr2
        rankings <- combowr2[order(combowr2$Consensus),]
      }
      
      if(input$positionI == "TE") {
        rankings <- combote2
        rankings <- combote2[order(combote2$Consensus),]
      }
      
      if(input$positionI == "DL") {
        rankings <- combodl2
        rankings <- combodl2[order(combodl2$Consensus),]
      }
      
      if(input$positionI == "LB") {
        rankings <- combolb2
        rankings <- combolb2[order(combolb2$Consensus),]
      }
      
      if(input$positionI == "DB") {
        rankings <- combodb2
        rankings <- combodb2[order(combodb2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$idpdevy <- DT::renderDataTable({
    DT::datatable({  
  
      if(input$positionH == "All") {
        rankings <- idpdevy[order(idpdevy$Consensus),]
      }
      
      if(input$positionH == "DL") {
        rankings <- idpdevydl2
        rankings <- idpdevydl2[order(idpdevydl2$Consensus),]
      }
      
      if(input$positionH == "LB") {
        rankings <- idpdevylb2
        rankings <- idpdevylb2[order(idpdevylb2$Consensus),]
      }
      
      if(input$positionH == "DB") {
        rankings <- idpdevydb2
        rankings <- idpdevydb2[order(idpdevydb2$Consensus),]
      }
     
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$rookie <- DT::renderDataTable({
    DT::datatable({
  
      if(input$positionE == "All") {
        rankings <- rookie[order(rookie$Consensus),]
      }
      
      if(input$positionE == "QB") {
        rankings <- rookieqb2
        rankings <- rookieqb2[order(rookieqb2$Consensus),]
      }
      
      if(input$positionE == "RB") {
        rankings <- rookierb2
        rankings <- rookierb2[order(rookierb2$Consensus),]
      }
      
      if(input$positionE == "WR") {
        rankings <- rookiewr2
        rankings <- rookiewr2[order(rookiewr2$Consensus),]
      }
      
      if(input$positionE == "TE") {
        rankings <- rookiete2
        rankings <- rookiete2[order(rookiete2$Consensus),]
      }
     
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
      
  output$devy <- DT::renderDataTable({
    DT::datatable({
  
      if(input$positionG == "All") {
        rankings <- devy[order(devy$Consensus),]
      }
      
      if(input$positionG == "QB") {
        rankings <- devyqb2
        rankings <- devyqb2[order(devyqb2$Consensus),]
      }
      
      if(input$positionG == "RB") {
        rankings <- devyrb2
        rankings <- devyrb2[order(devyrb2$Consensus),]
      }
      
      if(input$positionG == "WR") {
        rankings <- devywr2
        rankings <- devywr2[order(devywr2$Consensus),]
      }
      
      if(input$positionG == "TE") {
        rankings <- devyte2
        rankings <- devyte2[order(devyte2$Consensus),]
      }
      
    })
    rankings
  }, rownames = FALSE, options = list(searching=FALSE,paging=FALSE))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

