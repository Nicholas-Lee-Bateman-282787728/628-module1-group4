library(shiny)

data = read.csv("../Data/BodyFat_cleaned.csv", header = TRUE)
data = data[,c("BODYFAT","AGE","ABDOMEN","WRIST")]

ui = shinyUI(pageWithSidebar(
  headerPanel("Bodyfat Calculator"),
  
  sidebarPanel(
    numericInput("Age", "Age (years):", min = 0, max = 150, value = NA),
    helpText("The number you enter must be an integer between 0 and 150"),   
    numericInput("Abdomen", "Abdomen circumference (cm):", min = 50, max = 150, value = NA),
    helpText("The number you enter must between 0 and 200"),
    numericInput("Wrist", "Wrist circumference (cm):", min = 10, max = 30, value = NA),
    helpText("The number you enter must between 0 and 50"),
    
    helpText("If you change the input, please reclick the calculate button again."),
    submitButton("calculate"),
    
    h3('Any issue?'),
    h4('Contact with our team!'),
    h4('Shuo Qiang: sqiang@wisc.edu'),
    h4('Yixin Chen: chen777@wisc.edu'),
    h4('Jiatong Li: jli872@wisc.edu')
    ),
  
  
  mainPanel(tabsetPanel(
    tabPanel("Result", tableOutput("table"), imageOutput("conclusion")),
    tabPanel("Fomula", imageOutput("fomula")),
    tabPanel("Male Body Fat Chart", imageOutput("image"))
  ))
))

server = shinyServer(function(input, output) {
  model = lm(formula = BODYFAT ~ AGE + ABDOMEN + WRIST + 0, data = data)
  prediction <- reactive({
    new_data = data.frame(  "AGE" = round(input$Age),
                            "ABDOMEN" = input$Abdomen,
                            "WRIST" = input$Wrist)
    
    if (is.na(new_data[1,1]) == TRUE | is.na(new_data[1,2]) == TRUE | is.na(new_data[1,3]) == TRUE){
      pre = c("", "", "", "")
    }
    else if (new_data[1,1] < 0 | new_data[1,1] > 150 | new_data[1,2] < 0 | new_data[1,2] > 200
        | new_data[1,3] < 0 | new_data[1,3] > 50){
      pre = c("", "", "","Input error")
    }
    else{
      pre1 = predict(model, new_data, interval = "confidence", level = 0.95)
      pre2 = predict(model, new_data, interval = "prediction", level = 0.90)
      pre = c(paste(min(100,max(0, round(pre1[1],1))), "%"), paste("[", min(100,max(0, round(pre1[2],1))),",", min(100,max(0, round(pre1[3],1))),"]"), 
              paste("[", min(100,max(0, round(pre2[2],1))),",", min(100,max(0, round(pre2[3],1))),"]"), min(100,max(0, round(pre1[1],1))))
    }
    pre
  })
  
  output$table = renderTable({
    d = data.frame("prediction" = prediction()[1], "confidence interval" = prediction()[2], "prediction interval" = prediction()[3])
    names(d) = c("body fat percentage", "95% confidence interval","90% prediction interval")
    d
  })
  
  output$conclusion <- renderImage({
    if (prediction()[4] == ""){
      text = normalizePath(file.path('../Images/open.png'))
    }
    else if (prediction()[4] == "Input error"){
      text = normalizePath(file.path('../Images/error.png'))
    }
    else{
      if(as.numeric(prediction()[4]) >= 26){
        text = normalizePath(file.path('../Images/over.png'))
      }
      if (as.numeric(prediction()[4]) >= 17 & as.numeric(prediction()[4]) < 26){
        text = normalizePath(file.path('../Images/normal.png'))
      }
      if (as.numeric(prediction()[4]) >= 11 & as.numeric(prediction()[4]) < 17){
        text = normalizePath(file.path('../Images/fit.png'))
      }
      if (as.numeric(prediction()[4]) >= 7 & as.numeric(prediction()[4]) < 11){
        text = normalizePath(file.path('../Images/athlete.png'))
      }
      if (as.numeric(prediction()[4]) < 7){
        text = normalizePath(file.path('../Images/essential.png'))
      }
    }

    list(src = text,         
         width = 630,
         height = 45)
  }, deleteFile = FALSE)
  
  output$fomula <- renderImage({
    filename <- normalizePath(file.path('../Images/fomula.png'))
    list(src = filename,         
         width = 600,
         height = 50)
  }, deleteFile = FALSE)
  
  output$image <- renderImage({
    filename <- normalizePath(file.path('../Images/bodyfat.jpeg'))
    list(src = filename,         
         width = 600,
         height = 400)
  }, deleteFile = FALSE)
  
})

shinyApp(ui = ui, server = server)
