library(shiny)
library(rhandsontable)

source("helper.R")

#df <- read.csv("fb_chart.csv")
df <- data.frame(offTeam = character(),
                 drive=integer(),
                 playNum=integer(),
                 quarter=integer(),
                 time = character(),
                 down=integer(),
                 distance=integer(),
                 fieldPosition=integer(),
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
                 
                 dFront = character(),
                 dBlitz = character(),
                 dCover = character(),
                 yardage=integer(),
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
                   column(1,numericInput("quarter","Quarter", 1 ,min = 1)),
                   column(1,textInput("time","Time")),
                   column(1,numericInput("down","Down", 1 ,min = 1, max = 4)),
                   column(1,numericInput("distance","Distance", 10,  min = 1, max = 99)),
                   column(2,tags$div(title = "On your own side of field, dist = 100 - LOS otherwise dist = LOS",
                                     textInput("fieldPos","Distance From GL")) ),
                   column(1, selectInput("spot", "Spot", list("LH", "Mid", "RH"))),
                   column(1,textInput("personnel","Personnel")),
                   column(1,textInput("dFront","Def. Front"))
                   ),
          fluidRow( h3("Post Snap"),
                   column(1, selectInput("playType", "Play Type",list("Run","Pass","Punt","FG"),selected = "Pass")),
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
                   
                     column(2,textInput("dBlitz","Def. Blitz", placeholder = "Blitzers separated by comma")),
                     column(1,textInput("yardage", "Yardage")),
                     column(2, textInput("note", "Notes"))
          
                   
                   
                   ),
          fluidRow(column(1),
            
                   column(4,actionButton("submit","Submit Play")),
                   
                   column(4, downloadButton('downloadData')))
          ),
          fluidRow(column(12, tabsetPanel(
                                tabPanel("Data",rHandsontableOutput('tbl')),
                                tabPanel("Summary", "Put summary stats here"),
                                tabPanel("Heat Maps", "Put run/pass maps here")
                              )
                          )
                   )
          )
          ,
  server = function(input, output, session) {
    
    values <- reactiveValues(dfWorking = df)
    
    #check for and allow users to update rows on the data table
    observe({
      if(!is.null(input$tbl))
        values$dfWorking <- hot_to_r(input$tbl)
      
    })
    observeEvent(input$setTeams,{
      updateSelectInput(session, "offTeam",choices = list(input$homeTeam, input$awayTeam))
    })
    
    observeEvent(input$submit,{
      validate(
        need(input$down != "", "Please enter in a down from 1-4"),
        need(input$distance != "", "Please enter in a distance"),
        need(input$fieldPos != "", "Please enter in a field position"),
        need(input$yardage != "", "Please enter in a yardage gained for the play.")
        
        
        
      )
      #get values for updated down, distance, play, and drive counts
      curPlay = getCurrentPlay(values$dfWorking, input$offTeam)
      curDrive = getCurrentDrive(values$dfWorking, input$offTeam)
      downAndDist <- getDownAndDistance(input$down,input$distance, input$yardage, input$fieldPos )
      curDist = downAndDist[["dist"]]
      curDown = downAndDist[["down"]]
      curFieldPos = downAndDist[["fieldPos"]]
      
      #get data from form
      newLine <- c(input$offTeam, curDrive, curPlay , input$quarter,
                   input$time, input$down, input$distance,
                   input$fieldPos, input$spot, input$personnel,
                   input$playType, input$runDirection,input$runGap,
                   input$play, input$rusher,input$passDirection,
                   input$passDepth,input$target,
                   input$dFront, input$dBlitz, input$dCover,input$yardage, input$note)
      
      #add a new line to working dataframe
      values$dfWorking[nrow(values$dfWorking)+1,] <- newLine
      values$dfWorking <- values$dfWorking[order(as.numeric(rownames(values$dfWorking)),decreasing = TRUE),]
      
      #update and reset inputs
      updateTextInput(session, "playNum", value = curPlay + 1)
      updateTextInput(session, "distance", value = curDist)
      updateTextInput(session, "down", value = curDown)
      updateTextInput(session, "fieldPos", value = curFieldPos)
      updateTextInput(session, "target", value = "")
      updateTextInput(session, "personnel", value = "")
      updateTextInput(session, "play", value = "")
      updateTextInput(session, "yardage", value = "")
      updateTextInput(session, "rusher", value = "")
      updateNumericInput(session, "passDepth", value = 0)
      
  #render hands on table with updated values
      output$tbl <- renderRHandsontable({
        rhandsontable(values$dfWorking)
      })
      
      #download as csv
      output$downloadData <- downloadHandler(
        filename = function() { paste(date(), '.csv', sep='') },
        content = function(file) {
          write.csv(values$dfWorking, file)
        }
      )
    })
    
    
    
  }
  
)