### tutorial test app based on code club guidance 

### Basic app.R consists of 5 parts

# 1. 
# A section at the top of the script loading any packages needed for the app to run. 
# shiny is required at the very least, but others like dplyr or ggplot2 could be added as they are needed:

# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from


# 2.
# A section loading any data needed by the app:

# Loading data ----
Barley <- as.data.frame(beaven.barley)


# 3.
# An object called ui, which contains information about the layout of the app as it appears in your web browser. 
# fluidPage() defines a layout that will resize according to the size of the browser window. 
# All the app code will be placed within the brackets.

# ui.R ----
ui <- fluidPage(
    
    titlePanel("Barley Yield"), # add title panel - separate panel at the top of the page in which we can put the title
    
    sidebarLayout( # make the layout a sidebarLayout
        
        sidebarPanel( # inside sidebarLayout, make sidebarPanel - often contain input widgets like sliders, text input boxes, radio buttons etc.
            
            selectInput(inputId = "gen", # Give the input a name "genotype". Useful later when referencing this input
                        label = "1. Select genotype", # Give the input a label to be displayed in the app
                        choices = c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e", "F" = "f", "G" = "g", "H" = "h"), selected = "a"), 
            # Create the choices that can be selected e.g. Display "A" and link to value "a". 
            # A, B, etc Will be listed in drop-down menu in app, and the value that is actually gathered from that choice for use in the output (a, b, etc. - as in the dataset)
            # Note that choices = c("A" = "a" ... could be replaced with choices = unique(Barley$gen) to simply use the groups directly from the dataset.
            # selected = "a" will be the default one 
            
            selectInput(inputId = "colour",
                        label = "2. Select histogram colour",
                        choices = c("blue", "green", "red", "purple", "grey"), selected = "grey"),
            
            sliderInput(inputId = "bin",
                        label = "3. Select number of histogram bins",
                        min=1, max=25, value = c(10)),
            
            textInput(inputId = "text",
                      label = "4. Enter some text to be displayed", "")
            
        ), 
        
        mainPanel( # inside sidebarLayout, make mainPanel - often contain the output of the app, whether it is a table, map, plot or something else.
            
            plotOutput("myhist"), # make histogram appear in main panel
            tags$hr(), # puts a horizontal line in 
            tableOutput("mytable"), # make table appear in main panel 
            tags$hr(),
            textOutput("mytext"), # make text appear in main panel 
            tags$br(), # adds break
            tags$div(style="color:red", # text is red
                     tags$p("Visit us at:"), # creates paragrpah of text 
                     tags$a(href = "https://ourcodingclub.github.io", "Coding Club") # include link
            ),
            tags$br() # adds break 
            
        ) 
    )
)


# 4. 
# An object called server, which contains information about the computation of the app, 
# creating plots, tables, maps etc. using information provided by the user. 
# All the app code will be placed within the curly brackets.

# server.R ----
server <- function(input, output) {
    
    output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) + # create object called output$plot with a ggplot inside it
                                  geom_histogram(bins = input$bin, # add histogram to plot, make bins whatever is selected in selectInput called bin
                                                 fill = input$colour, # make fill whatever the value is in the selectInput called colour
                                                 data = Barley[Barley$gen == input$gen,], # use data from 'barley'
                                                 # tells geom_histogram() to only use data where the value in column gen is equal to (==) the value given by input$gen. 
                                                 # Note the , after input$gen which indicates that we are selecting columns and that all the rows should be selected.
                                                 colour = "black") # outline the bins in black
                              )
    # renderPlot renders a reactive plot that is suitable for assigning to an output slot.It wraps the ggplot() command
    
    output$mytext <- renderText(input$text)
    
    output$mytable <- renderTable(Barley %>% # use dplyr to filter and summarise results 
                                      filter(gen == input$gen) %>%
                                      summarise("Mean" = mean(yield),
                                                "Median" = median(yield),
                                                "STDEV" = sd(yield),
                                                "Min" = min(yield),
                                                "Max" = max(yield)))
    
}


# 5.
# A command to run the app. 
# This should be included at the very end of app.R. 
# It tells shiny that the user interface comes from the object called ui 
# and that the server information (data, plots, tables, etc.) comes from the object called server.

# Run the app ----
shinyApp(ui = ui, server = server)











