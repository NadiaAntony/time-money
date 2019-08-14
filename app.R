library(shiny)
library(shinythemes)
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
    , style = "border-style: dashed; border-width: 2px; margin:10px; text-align: center;")
}

more_choices <- c(2300, 200, 1200, 349.99)
names(more_choices) <- c("Top notch camera", "Dinner at expensive restaurant", "Persian Cat", "Bose Headphones")

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
            numericInput("amount", "Amount ($)", value = 100000, step = 10000),
            numericInput("wage", "Hourly Wage ($)", value = 15, step = .5),
            checkboxInput(label = "Enter Weekly Hours", inputId = "whours", value = FALSE),
            conditionalPanel(condition = "input.whours", 
                             numericInput("hours", "Weekly Hours", value = 40, step = .5))
        ),
        
        mainPanel(
            h4("Time Required"),
            htmlOutput("result"),
            
            hr(),
            h4("Purchasing Power"),
            htmlOutput("default"),
            conditionalPanel(condition = "input.more", htmlOutput("selected_more")),
            div(h2(selectizeInput("more", label = "View More", multiple = TRUE,
                               choices = names(more_choices), selected = NULL)),
                style = "border-style: dashed; border-width: 2px; margin:15px; padding:20px; text-align: left;")
            
        )
    )
)

server <- function(input, output) {
    
    workweeks<- reactive({
        return(ceiling(input$amount / (input$hours * input$wage)))
    })
    
    output$result <- renderText({
        return(paste0("<h4>To earn <strong>", scales::dollar(input$amount), "</strong>, you would need to work <strong>", workweeks(), " weeks (", round(workweeks() / 52, digits = 2), " years)</strong> at <strong>", scales::dollar(input$wage), "</strong>/hr</h4>"))
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
                fluidRow(
                    column(6,
                           get_box("Netflix subscription", price = 12.99, wage = input$wage, amount = input$amount)
                    ),
                    column(6,
                           get_box("Latest Android", price = 700, wage = input$wage, amount = input$amount)
                    )
                ),
                fluidRow(
                    column(6,
                           get_box("DJI drone", price = 1200, wage = input$wage, amount = input$amount)
                    ),
                    column(6,
                           get_box("Gucci Pants", price = 1000, wage = input$wage, amount = input$amount)
                    )
                )
            )
        )
    })
    output$selected_more <- renderUI({
        return(
            tagList(
                fluidRow(
                    lapply(input$more, function(x) column(6, get_box(x, price = more_choices[[x]], wage = input$wage, amount = input$amount)))
                )
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
