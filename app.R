library(shiny)
library(shinythemes)
library(flexdashboard)
library(tidyverse)

## Set images resource path
addResourcePath("images", "images")

## Box HTML and CSS
get_box <- function(name, price, wage, amount) {
    div(tagList(
        div(img(src = paste0("images/", name, ".png"), width = "135", height = "100"), br(), strong(name)),
        div(ifelse(name=="Netflix subscription", paste0("Cost per month: ", scales::dollar(price)), paste0("Cost: ", scales::dollar(price)))),
        div(paste0("Cost for one in Man-Hours: ", round(price / wage))),
        div(paste0("Total number you can Afford: ", floor(amount / price)))
    )
    , style = "border-style: dashed; border-width: 2px; text-align: center;")
}

ui <- fluidPage(theme = shinytheme("cerulean"),
                
    includeCSS("css/styles.css"),
    
    titlePanel("Time Value of Money"),
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       a(href = "https://oaiti.org", target = "_blank", img(src = "images/logo/logo_trans.png", width = "135"))
                ),
                column(6,
                       a(href = "https://oaiti.org", target = "_blank", img(src = "images/logo/oaiti_transparent.png", width = "95"))
                )
            ),
            h4("About"),
            HTML("\"<i>Your time is valuable, so value your time</i>\" - Unknown<br><br>The old adage 'Time is Money' still rings true to this day. This application was designed to help put it into context. Simply input a dollar amount and a wage to learn more about your money's purchasing power, and the time it would take to earn that amount."),
            
            h4("Configuration"),
            numericInput("amount", "Amount", value = 100000, step = 10000),
            numericInput("wage", "Wage", value = 15, step = .5)
        ),
        
        mainPanel(
            h4("Time Required"),
            htmlOutput("result"),
            
            hr(),
            h4("Purchasing Power"),
            htmlOutput("default"),
            conditionalPanel(condition = "input.more", htmlOutput("selected_camera")),
            selectizeInput("more", label = "Add More", multiple = TRUE,
                           choices = c("Top notch camera", "Dinner at expensive restaurant", "Persian Cat", "Bose Headphones"), selected = NULL)
        )
    )
)

server <- function(input, output) {
    
    mydays <- reactive({
        return(ceiling(input$amount / (8 * input$wage)))
    })
    
    output$result <- renderText({
        return(paste0("<h4>To earn ", scales::dollar(input$amount), ", you would need to work ", mydays(), " days (", round(mydays() / 365, digits = 2), " years) at ", scales::dollar(input$wage), "/hr</h4>"))
    })
    
    output$default <- renderUI({
        return(
            tagList(
                fluidRow(
                    column(6,
                           get_box("New Toyota Camry 2019", price = 22500, wage = input$wage, amount = input$amount)
                    ),
                    column(6,
                           get_box("Apple iPhone XS", price = 999, wage = input$wage, amount = input$amount)
                    )
                ),
                br(),
                fluidRow(
                    column(6,
                           get_box("Netflix subscription", price = 12.99, wage = input$wage, amount = input$amount)
                    ),
                    column(6,
                           get_box("Latest Android", price = 700, wage = input$wage, amount = input$amount)
                    )
                ),
                br(),
                fluidRow(
                    column(6,
                           get_box("DJI drone", price = 1200, wage = input$wage, amount = input$amount)
                    ),
                    column(6,
                           get_box("Gucci Pants", price = 1000, wage = input$wage, amount = input$amount)
                    )
                ),
                br()
            )
        )
    })
    output$selected_camera <- renderUI({
        output_camera <- output_dinner <- output_cat <- output_bose <- div()
        
        if("Top notch camera" %in% input$more)
            output_camera <- get_box("Top notch camera", price = 2000, wage = input$wage, amount = input$amount)
        if("Dinner at expensive restaurant" %in% input$more)
            output_dinner <- get_box("Dinner at the most expensive restaurant in Des Moines", price = 2000, wage = input$wage, amount = input$amount)
        if("Persian Cat" %in% input$more)
            output_cat <- get_box("Persian Cat", price = 2000, wage = input$wage, amount = input$amount)
        if("Bose Headphones" %in% input$more)
            output_bose <- get_box("Bose Headphones", price = 2000, wage = input$wage, amount = input$amount)
        return(
            tagList(
                fluidRow(
                    column(6, output_camera),
                    column(6, output_cat),
                    column(6, output_bose),
                    column(6, output_dinner)
                )
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
