

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$app_basket <- renderText({
        
        create_basket(num = 1, first_word = "appetizer", n_ingredients = 5)
    })
})
