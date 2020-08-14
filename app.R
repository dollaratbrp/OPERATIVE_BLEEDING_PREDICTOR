library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Operative Bleeding Probability Calculator"),
  sidebarLayout(      
    sidebarPanel(
      radioButtons("race", label = h3("BlackRace"),
                   choices = list("Yes" = 1, "No/ Any Other Race" = 0), 
                   selected = 1),
      radioButtons("transfusion", label = h3("PreOperativeTransfusion"),
                   choices = list("Yes" = 1, "No" = 0), 
                   selected = 1),
      radioButtons("open", label = h3("Open"),
                   choices = list("Yes (Open/ Abdominal Route of Surgery)" = 1, "Laproscopic/ MIS Route of Surgery" = 0), 
                   selected = 1),
      div(
        numericInput("hematocrit", "Hematocrit:", 40,min = 1, max = 100), style="font-size:170%; font-family:Arial;"),
      submitButton(text = "Calculate"),
      hr(),
      helpText("Please make your selection and click calculate to claculate the operative bleeding probability")),
    mainPanel(
      htmlOutput("text_calc"))))
)

server <- shinyServer(function(input, output,session){
  output$text_calc <- renderText({
    race <- as.numeric(input$race)
    transfusion <- as.numeric(input$transfusion)
    open <- as.numeric(input$open)
    validate(
      need(input$hematocrit != "", "Please enter a valid continious pre-operative hematocrit value")
    )
    hematocrit<-as.numeric(input$hematocrit)
    e<-2.718
    paste("The predicted probablity of an Intra/ Post Operative Bleeding for a",
          ifelse(race=='1',"Black individual ","Non-Black individual "),
          ifelse(transfusion=='1',"with a PreOperative Transfusion ","without a PreOperative Transfusion, "),
          ifelse(open=='1',"opting for an Open/ Abdominal route of surgery ","opting for a Laproscopic/ MIS route of surgery "),
          "with a hematocrit value of", hematocrit,"is ",  "<font size= \"4px\"><b>",
          "<font color=\"#FF0000\"><b>",
          e^(2.1003+(0.5004*race)+(0.9807*transfusion)+(1.6976*open)+(-0.1623*hematocrit))/
            1+e^(2.1003+(0.5004*race)+(0.9807*transfusion)+(1.6976*open)+(-0.1623*hematocrit)), "</b></font>")
  })
})

shinyApp(ui = ui, server = server)
