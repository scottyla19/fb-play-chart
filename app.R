library(shiny)
source("helper.R")

#df <- read.csv("fb_chart.csv")
df <- data.frame(offTeam = character(),
                 drive=integer(),
                 playNum=integer(),
                 quarter=integer(),
                 time = character(),
                 down=integer(),
                 distance=integer(),
                 fieldPosition=character(),
                 spot=character(),
                 personnel=character(),
                 playType=character(),
                 runDirection=character(),
                 runGap=character(),
                 runPlay=character(),
                 rusher = character(),
                 passDirection=character(),
                 passDepth=integer(),
                 target=character(),
                 puntLength=integer(),
                 fgLength=integer(),
                 dFront = character(),
                 dBlitz = character(),
                 dCover = character(),
                 outcome=character(),
                 note=character(),
                 stringsAsFactors=FALSE)


shinyApp(
  ui = fluidPage(
          tags$head(
             tags$style(HTML('#submit{background-color:#337ab7; color:white}'))
          ),
          titlePanel("Football Play Chart App"),
          fluidRow(h3("Setup"),
                   column(3, textInput("homeTeam","Home Team")),
                   column(3, textInput("awayTeam", "Away Team")),
                   column(3, actionButton("setTeams", "Set Teams"))
                   ),
          conditionalPanel(condition = "input.setTeams > 0",
            
          
          fluidRow(h3("Pre Snap"),
                   column(1, selectInput("offTeam", "Offense",choices = NULL)),
                   #column(1,numericInput("drive","Series", 1 ,min = 1)),
                   #column(1,numericInput("playNum","Play", 1 ,min = 1)),
                   column(1,numericInput("quarter","Quarter", 1 ,min = 1)),
                   column(1,textInput("time","Time")),
                   column(1,numericInput("down","Down", 1 ,min = 1, max = 4)),
                   column(1,numericInput("distance","Distance", 10,  min = 1, max = 99)),
                   column(2,textInput("fieldPos","Field Position", placeholder = "OWN45 or OPP20") ),
                   column(1, selectInput("spot", "Spot", list("LH", "Mid", "RH"))),
                   column(1,textInput("personnel","Personnel")),
                   column(1,textInput("dFront","Def. Front"))
                   ),
          fluidRow( h3("Post Snap"),
                   column(1, selectInput("playType", "Play Type",list("Run","Pass","Punt", "FG"),selected = "Pass")),
                   conditionalPanel(
                     condition = "input.playType == 'Run'",
                        column(1,selectInput("runDirection","Direction",list("Right", "Left"))),
                        column(1,selectInput("runGap","Gap", list("A","B","C","D"))),
                        column(2,textInput("play", "Play")),
                        column(1,textInput("rusher", "Rusher"))
                   ),
                        
                   conditionalPanel(
                     condition = "input.playType == 'Pass'",
                     column(2,selectInput("passDirection","Direction",list("Right","Mid","Left"))),
                     column(1,numericInput("passDepth", "Pass Depth", 0)),
                     column(1,textInput("target", "Target")),
                     column(1,textInput("dCover","Def. Cover", placeholder = "Cover 0, Cover 1, etc."))
                   ),
                   
                   conditionalPanel(
                     condition = "input.playType == 'Punt'",
                     column(2,numericInput("puntLength", "Punt Length", 0))),
                   
                   conditionalPanel(
                     condition = "input.playType == 'FG'",
                     column(1,numericInput("fgLength", "FG Length", 15))),
                   
                     column(2,textInput("dBlitz","Def. Blitz", placeholder = "Blitzers separated by comma")),
                     column(1,textInput("outcome", "Outcome")),
                     column(2, textInput("note", "Notes"))
          
                   
                   
                   ),
          fluidRow(column(6,actionButton("submit","Submit Play")),
                   
                   column(6, downloadButton('downloadData')))
          ),
          fluidRow(column(12, dataTableOutput('tbl')))
          )
          ,
  server = function(input, output, session) {
    values <- reactiveValues(dfWorking = df)
    
    observeEvent(input$setTeams,{
      updateSelectInput(session, "offTeam",choices = list(input$homeTeam, input$awayTeam))
    })
    
    observeEvent(input$submit,{
      curPlay = getCurrentPlay(values$dfWorking, input$offTeam)
      curDrive = getCurrentDrive(values$dfWorking, input$offTeam)
      
      newLine <- c(input$offTeam, curDrive, curPlay , input$quarter,
                   input$time, input$down, input$distance,
                   input$fieldPos, input$spot, input$personnel,
                   input$playType, input$runDirection,input$runGap,
                   input$play, input$rusher,input$passDirection,
                   input$passDepth,input$target, input$puntLength,
                   input$fgLength, input$dFront, input$dBlitz, input$dCover,input$outcome, input$note)
      
      
      values$dfWorking[nrow(values$dfWorking)+1,] <- newLine
      values$dfWorking <- values$dfWorking[order(as.numeric(rownames(values$dfWorking)),decreasing = TRUE),]
      
     
      
      
      #update and reset inputs
      updateTextInput(session, "playNum", value = curPlay + 1)
      updateTextInput(session, "target", value = "")
      updateTextInput(session, "personnel", value = "")
      updateTextInput(session, "play", value = "")
      updateTextInput(session, "outcome", value = "")
      updateTextInput(session, "rusher", value = "")
      updateNumericInput(session, "passDepth", value = 0)
      
      
      
      
     
      #series	playNum	quarter	time	down	distance	fieldPosition	spot	personnel	playType	runDirection	
      #runGap	runPlay	rusher	passDirection	passDepth	target	puntLength	fgLength	outcome
     
      
      
      output$tbl = renderDataTable(
        values$dfWorking,
        options = list(
          
        )
        
      )
      
      output$downloadData <- downloadHandler(
        filename = function() { paste(date(), '.csv', sep='') },
        content = function(file) {
          write.csv(values$dfWorking, file)
        }
      )
    })
    
    
    
  }
  
)