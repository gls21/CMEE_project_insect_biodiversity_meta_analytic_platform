# Shiny apps tutorial https://ourcodingclub.github.io/tutorials/shiny/index.html

# Creating interactive web apps using the R language (rather than the traditionally used javascript or HTML5)
# Basically an R package 
# Good for applications designed around data presentation and analysis 
# Good for sharing results with collaborators and communicating science in an accessible way 


#############################################################################
# 1.Downloading Shiny and tutorial resources
#############################################################################

# install shiny package
install.packages("shiny")
install.packages("rsconnect")  # For publishing apps online
install.packages("agridat")  # For the dataset in today's tutorial


#############################################################################
# 2.The Shiny app file structure
#############################################################################

# Selected file/newfile/shinywebapp 
# Called app 'tutorial', single file application type, 
# and saved in onedrive/mastersproject/shinyappstutorial folder
# Clicked create

# Generated template R script called app.R. 
# Deleted all code in this template 

# Has made a folder in my shinyappstutorial folder called 'tutorial'
# In here is the app.R, which must remain named in this way 

# In this 'tutorial' folder, make folder called 'Data' and folder called 'www' 
# Data  will hold any data used by the app
# www will hold any images and other web elements.


#############################################################################
# 3. app.R layout
#############################################################################

# See app.R script 

# Shiny apps are structured using panels, which are laid out in different arrangements. 
# Panels can contain text, widgets, plots, tables, maps, images, etc.

# The most basic layout uses fluidRow() and column() to manually create grids of a given size. 
# fluidRow() allows a lot of customisation, but is more fiddly. 
# In this tutorial, we will be using sidebarLayout(), which creates a large panel and a smaller inset side panel.


#############################################################################
# 4. Creating a Shiny App - Basic Syntax
#############################################################################

# Explore some data on the productivity of Barley genotypes.

# the app has a sidebarLayout with a sidebarPanel, mainPanel and titlePanel. 
# It uses a selectInput to choose the genotype of barley shown in the histogram and the table, 
# another selectInput for the colour of the histogram, 
# a sliderInput to choose the number of bins in the histogram 
# and a textInput to display some text in the app. 
# The histogram is located in the mainPanel along with a summary table of the data being shown, 
# while the inputs are in the sidebarPanel.

# See app.R to see how we add these things to the ui object 

# Once basic structure is put together, can fill it with input and output widgets 
# Example app has four input widgets: 
#   a selectInput for genotype, 
#   a selectInput for histogram colour, 
#   a sliderInput for number of bins 
#   a textInput to add some arbitrary text. 
# Each of these widgets provides information on how to display the histogram and its accompanying table. 
# In the example app, all the widgets are found in the sidebarPanel 
# so the code for these widgets should be put in the sidebarPanel command

# Other input widgets include actionButton() and radioButtons()
# All inputs require an inputId and label argument 


### Running a shiny app 
# Preview app by clicking on the down arrow by the run app and selecting run external
# Then run app 
# Can't use console while app is running - need to press ESC or click stop button 
# So far we just see the inputs e.g. number of bins, histogram colour etc.


### Outputs
# So now need to add outputs which can be in the form of plots, tables, maps or text.
# Here we use ggplot2 to create a histogram 
# Have to place the code in the curly brackets in the server object - see app.R script 

# And need to link our input widgets to our output object 

# Also add output object which will contain inputted text

# Also make a table showing summary statistics 


### Displaying outputs
# To make the outputs appear on your app in the mainPanel, they need to be added to the ui object inside mainPanel()


### Additional elements
# Add shiny equivalent of HTML tags e.g. tags$hr() adds horizontal line 
# tags$br() adds break
# Tags can be stacked to apply many arguments to the same object/text, just as in HTML
# Can add to sidebar panel or main panel 



#############################################################################
# 5. Exporting a finished app 
#############################################################################

### Option 1.
# Send app.R alongside data (and any other resources) in a zip file to be unzipped by recipient and run through R


### Option 2.
# Github 
# Made new git repository called test_shiny_app - has to be public 
# Uploaded app.R file to here (couldn't add Data and www folders as empty)
# Also made README by using Create new file 

# Now people can run the following to see the app:
# runGitHub(repo = "Test_shiny_app", username = "gls21", ref = "main")


### Option 3.
# shinyapps.io app
# can also host Shiny apps on www.shinyapps.io, 
# a webhosting platform run by RStudio that is especially built for Shiny apps.

# Signed up with gls21@ic.ac.uk email 
# Need the rsconnect package which I installed earlier 
# Then authorised account by copying and pasting token and secret from shinyapps.io into console

# To deploy app:
library(rsconnect)
rsconnect::deployApp('/Users/gr8ce/OneDrive - Imperial College London/Masters Project/Shiny apps tutorial/tutorial/')
### This technically isn't the correct path anymore - fine for now but be aware if there are issues later 

# App can now be seen on my shinyapps.io account, and under overview, has the following URL:
#  https://r26dnk-grace-skinner.shinyapps.io/tutorial/

# Can make changes to app and use deployApp again 










