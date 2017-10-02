library(shiny)

data<-read.csv("https://raw.githubusercontent.com/aixarodriguez/ShinyProject/master/data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
  
  # Application title
  headerPanel("Body Mass Index Calculator (BMI)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Body mass index (BMI) is a measure of body fat based on height and weight that applies to adult men and women."),
      numericInput("num_height", label = h4("Height (cms)"),value=160),
      numericInput("num_weight", label = h4("Weight (Kg)"),value=58.50),
      selectInput("select_gender", label = h4("Gender"), choices = list("Female"="FEMALE", "Male"="MALE"), selected = "FEMALE"),
      selectInput("select_age", label = h4("Age Range"), choices = list("14-16"="X14.16", "17-19"="X17.19","20-24"="X20.24","25-29"="X25.29","30-39"="X30.39","40-49"="X40.49","50-59"="X50.59","60-69"="X60.69"), selected = "X30.39"),
      actionButton("action_calc", label = "Calculate")        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Output",
                 p(h4("Entered values:")), textOutput("text_weight"),textOutput("text_height"),  
                 p(h4("Calculated values:")),textOutput("text_bmi"), textOutput("text_type"),
                 p(h4("Expected Weight:")),textOutput("text_gender"),textOutput("text_age"),textOutput("text_expected")
        ),
        tabPanel("Documentation",
                 p(h4("Body Mass Index (BMI):")),
                 helpText("BMI is a useful measure of overweight and obesity. It is calculated from your height and weight. BMI is an estimate of body fat and a good gauge of your risk for diseases that can occur with more body fat. The higher your BMI, the higher your risk for certain diseases such as heart disease, high blood pressure, type 2 diabetes, gallstones, breathing problems, and certain cancers."),
                 HTML("<br><h4>Method for calculation: </h4>
                      <br> <table cellspacing=\"0\" cellpadding=\"0\" style=\"padding-left: 30px;\"><tbody><tr>
                      <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; BMI =&nbsp;</td>
                      <td><table><tbody><tr><td>mass (kg)</td></tr><tr><td bgcolor=\"#000000\" height=\"1\"></td></tr><tr><td>height<sup>2</sup> (m)</td></tr></tbody></table></td>
                      <td>&nbsp;=&nbsp;</td>
                      <td><table><tbody><tr><td>72.57</td></tr><tr><td bgcolor=\"#000000\" height=\"1\"></td></tr><tr><td>1.78<sup>2</sup></td></tr></tbody></table></td>
                      <td>&nbsp;= 22.90</td>
                      <td><table><tbody><tr><td>kg</td></tr><tr><td bgcolor=\"#000000\" height=\"1\"></td></tr><tr><td>m<sup>2</sup></td></tr></tbody></table></td>
                      </tr></tbody></table>"),
                 HTML("<br>
                      <table style=width:100%>
                      <tr>
                      <th></th>
                      <th>BMI</th> 
                      </tr>
                      <tr>
                      <td>Underweight</td>
                      <td>Below 18.5</td> 
                      </tr>
                      <tr>
                      <td>Healthy</td>
                      <td>18.5-24.9</td> 
                      </tr>
                      <tr>
                      <td>Overweight</td>
                      <td>25.0-29.9</td> 
                      </tr>
                      <tr>
                      <td>Obesity</td>
                      <td>30.0 and 39.9</td> 
                      </tr>
                      <tr>
                      <td>Morbid Obesity</td>
                      <td>40 and Above</td> 
                      </tr>
                      </table><br>"),
                 img(src='https://raw.githubusercontent.com/aixarodriguez/ShinyProject/master/bmi.jpg', align = "center", height="333", width="450")
                 )
                 )
      )
        )
)

server <- function(input, output,session) {
  values <- reactiveValues()

  # Display values entered
  output$text_weight <- renderText({
    input$action_calc
    paste("Weight (kg): ", isolate(input$num_weight))
  })
  
  output$text_height <- renderText({
    input$action_calc
    paste("Height (cms): ", isolate(input$num_height))
  })
  
  output$text_bmi <- renderText({
    input$action_calc
    values$bmi<-isolate({input$num_weight/((input$num_height/100)*(input$num_height/100))})
    paste("BMI: ", isolate(values$bmi))
  })
  
  output$text_type <- renderText({
    input$action_calc
    values$bmi<-isolate({input$num_weight/((input$num_height/100)*(input$num_height/100))})
    if(values$bmi<18.5){
      paste("Underweight")
    }else
      if(values$bmi>=18.5 & values$bmi<20.0){
        paste("Healthy")
      }else
        if(values$bmi>=20.0 & values$bmi<24.9){
          paste("Overweight")
        }else
          if(values$bmi>=24.9 & values$bmi<29.9){
            paste("Obesity")
          }else
            if(values$bmi>=29.9){
              paste("Morbid Obesity")}
  })
  
  output$text_gender <- renderText({
    input$action_calc
    paste("You are a: ", switch(isolate(input$select_gender),"FEMALE"="Female","MALE"="Male")) 
  })
  
  output$text_age <- renderText({
    input$action_calc
    paste("And age between: ", switch(isolate(input$select_age),"X14.16"="14-16","X17.19"="17-19","X20.24"="20-24","X25.29"="25-29","X30.39"="30-39","X40.49"="40-49","X50.59"="50-59","X60.69"="60-69"))
  })
  
  output$text_expected <- renderText({
    input$action_calc
    res<-subset(data,GENDER==isolate(input$select_gender) & HEIGHT==isolate(input$num_height))[,isolate(input$select_age)]
    paste("The expected weight it's: ", isolate(res)) 
  })
  
}

shinyApp(ui, server)