#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("KillTeam - MathHammer"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
               
           selectInput("att_unit", "Select Attacking Unit", ""),
               
               
           selectInput("att", h4("Number of Attacks"), 
                  choices = list("1" = 1, "2" = 2,
                                 "3" = 3, "4" = 4,
                                 "5" = 5), selected = 1),
            selectInput("bs", h4("BS"), 
                        choices = list("2+" = 2, "3+" = 3,
                                       "4+" = 4, "5+" = 5,
                                       "6+" = 6), selected = 1),
            selectInput("str", h4("Strength"), 
                        choices = list("1" = 1, "2" = 2,
                                       "3" = 3, "4" = 4,
                                       "5" = 5, "6" = 6, 
                                       "7" = 7, "8" = 8), selected = 1),
            selectInput("ap", h4("AP"), 
                        choices = list("None" = 0, "-1" = 1, "-2" = 2,
                                       "-3" = 3, "-4" = 4, "-5" = 5
                                       ), selected = 0)
        ),
        column(3,
            selectInput("t", h4("Toughness"), 
                        choices = list("1" = 1, "2" = 2,
                                       "3" = 3, "4" = 4,
                                       "5" = 5, "6" = 6,
                                       "7" = 7), selected = 3),
            selectInput("sv", h4("Armor Save"), 
                        choices = list("2+" = 2, "3+" = 3,
                                       "4+" = 4, "5+" = 5,
                                       "6+" = 6, "None" = 9),
                                       selected = 9),
            selectInput("inv", h4("Invul Armor Save"), 
                        choices = list("2+" = 2, "3+" = 3,
                                       "4+" = 4, "5+" = 5,
                                       "6+" = 6, "None" = 9),
                        selected = 9)
        ),
        # Output the text
        column(3,
            textOutput("hit_chance"),
            textOutput("wound_chance"),
            textOutput("save_chance"),
            textOutput("num_wounds"),
            textOutput("att_unit")
        )
    )
)

hit_chance_calc <- function(bs){
    if(bs < 3){
        hit_chance <- 5/6
    }
    if(bs == 3){
        hit_chance <- 4/6
    }
    if(bs == 4){
        hit_chance <- 3/6
    }
    if(bs == 5){
        hit_chance <- 2/6
    }
    if(bs == 6){
        hit_chance <- 1/6
    }
    if(bs > 6){
        hit_chance <- 0
    }
    return(hit_chance)
}

wound_chance_calc <- function(str, t){
    if(str>t){
        wound_chance <- 4/6
    }
    if(str>=(2*t)){
        wound_chance <- 5/6
    }
    if(str==t){
        wound_chance <- 3/6
    }
    if(str<t){
        wound_chance <- 2/6
    }
    if(2*str<=(t)){
        wound_chance <- 1/6
    }
    return(wound_chance)
}

break_armor_calc <- function(sv, ap, invul=7){
    # Caculate the save roll after armor pen is applied
    mod_save <- sv + ap
    # If the invul is a better roll, then replace with that
    if(mod_save > invul){
        mod_save <- invul
    }
    # the best roll is a 2+ no matter how good the save
    if(mod_save < 3){
        break_chance <- 1/6
    }
    if(mod_save == 3){
        break_chance <- 2/6
    }
    if(mod_save == 4){
        break_chance <- 3/6
    }
    if(mod_save == 5){
        break_chance <- 4/6
    }
    if(mod_save == 6){
        break_chance <- 5/6
    }
    if(mod_save > 6){
        break_chance <- 6/6
    }
        
    return(break_chance)
}



# Define server logic/
server <- function(input, output, session) {

   observe({
      # Load your csv file
      x <- read.csv("data/units.csv", header = TRUE, sep = ",")
          
      # Update selection att_unit (passed to UI)    
      updateSelectInput(session, "att_unit",
                        label = "Select Attacking Unit",
                        choices = x[, 1],
                        selected = x[, 1][1]
            )
        })
    
    output$att_unit <- renderText({
        x <- input$att_unit
        paste("Attacking Unit: ", x)
    })
    
    output$hit_chance <- renderText({
        x <- 100 * hit_chance_calc(input$bs)
        x <- floor(x)
        paste("Chance to hit: ", x,"%")
    })
    
    output$wound_chance <- renderText({ 
        str <- as.numeric(input$str)
        t <- as.numeric(input$t)
        wound_chance <- 100 * wound_chance_calc(str, t)
        wound_chance <- floor(wound_chance)
        paste("Chance to wound: ", wound_chance,"%")
    })
    
    output$save_chance <- renderText({ 
        save <- as.numeric(input$sv)
        ap <- as.numeric(input$ap)
        inv <- as.numeric(input$inv)
        x <- break_armor_calc(save, ap, inv)* 100
        x <- floor(x)
        paste("Chance to beat armor: ", x,"%")
    })
    
    output$num_wounds <- renderText({
        att <- as.numeric(input$att)
        bs <- as.numeric(input$bs)
        str <- as.numeric(input$str)
        t <- as.numeric(input$t)
        sv <- as.numeric(input$sv)
        ap <- as.numeric(input$ap)
        inv <- as.numeric(input$inv)
        num_wounds <- att * hit_chance_calc(bs)
        num_wounds <- num_wounds * wound_chance_calc(str, t)
        num_wounds <- num_wounds * break_armor_calc(sv, ap, inv)
        num_wounds <- round(num_wounds, 2)
        paste("Number of Wounds: ", num_wounds)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
