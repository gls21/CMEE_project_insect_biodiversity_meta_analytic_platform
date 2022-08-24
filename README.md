# Imperial College London CMEE master's project with the Natural History Museum
## Developing an interactive platform for insect biodiversity meta-analyses using R Shiny

#### Description
This project aims to develop an interactive platform to make the best use of the data available now and in the future to untangle the complex patterns and drivers of insect biodiversity trends. I achieved this by developing an app using R Shiny.
The app performs what we call 'meta-meta-analysis' defined as analysing multiple meta-analysis studies together by combining effect sizes collected for each of these.
It allows users to run custom robust linear mixed-effects models to investigate their hypotheses and interests.
When new meta-analyses are conducted, the data can be uploaded to the app and included in future models. 

#### Main stages of the project
* Wrote two essays - one on insect declines, one on meta-analyses. 
* Finalised project approach - I aim to build a Shiny app as they are interactive and accessible to anyone (even those who do not code in R)
* Background reading on Shiny apps and followed a Shiny app tutorial to make a basic one. 
* Decided to build the app using Christina Raw's data collected for her meta-meta-analysis of the effect of agricultural systems on biodiversity. Went through all code written for this project to understand the process, and picked out and replicated bits that would be necessary for building the Shiny app. 
* Built the Shiny app, starting with a basic app that produced a graph with some reactivity based on user input.
* Incorporated more and more features over time, including changing the layout to create different tabs, giving more options for user input and reactivity, and giving the user the option to upload their data for use in the app. 
* Throughout the project, have also completed tutorials on SQL, metafor, making R packages, R markdown, and attended NHM workshops focusing on reading literature and writing. 

#### Directories/Scripts
* christina_mma (note data is not publicly available so not included here):
    * bayesian_analysis - replicating Christina's approach to modelling the data with Bayesian models (not used in the Shiny app)
    * dummy_data - making dummy data to practise uploading to the Shiny app
    * meta_meta_analysis - the insect biodiversity meta-analytic Shiny app (global.R, ui.R, server.R, etc.)
    * relevant_analysis - condensed version of replicating Christina's scripts that are relevant to the Shiny app
    * replicating_christina_analysis - replicating Christina's scripts that are relevant to the Shiny app
    * robust_analysis - replicating Christina's approach to modelling the data with robust models (used in the Shiny app)
    * testing_efficiency - quantitative data collected and code run to assess the effectiveness of the Shiny app. Also contains qualitative data collected for user experience testing
* essays - essays on insect declines and meta-analyses completed at start of project
* glitrs_presentation - presentation completed quite early on in the process of making the app to get expert opinion on how to improve it and discuss its use
* literature_reviews - notes on the reading I have done on insect declines, meta-analysis, and papers describing tools with similarities to the Shiny app
* mini_presentation_to_lab_group - short presentation to lab group to help with narrative of thesis
* project_proposal
* project_report
    * The word version can be found in all/all_v3_grace_skinner.docx
    * The LaTeX version can be found in project_report_natbib.tex
    * project_report_bibliography_edited.bib contains bibliography entries used for LaTeX
    * images_and_figures contains figures used in report
* tutorials - Shiny apps, metafor, making R packages, SQL, R markdown, dplyr, and tidyverse style guide 
* project_activities_record.docx - log of what I did each day during the project

#### Languages
* R version 4.2.0

#### Dependencies (R packages)
* agridat - contains the data for the practise Shiny app
* blme - for Bayesian analysis
* broom - for getting model summary
* bslib - for themes to customise appearance of shiny app
* devtools - to use the install_github function to download an R package
* dplyr - for data manipulation
* ggplot2 - for making plots
* googlesheets4 - for uploading files to googlesheets in Shiny app
* here - for specifying file paths
* influence.ME - for testing for influential data
* kableExtra - for pretty rmd tables
* knitr - for making tables
* lme4 - to run more complex linear models
* lmtest - to perform likelihood ratio tests
* maps - for mapping
* metafor - for running meta-analyses
* MuMIn - for model selection
* readxl - for reading excel files
* rmarkdown - for creating rmarkdown documents
* robustlmm - for running robust models
* roxygen2 - for making help files when making an R package
* rsconnect - for deploying a Shiny app
* shiny version 1.7.1 - required to run any Shiny app
* shinycssloaders - for loading symbols (while models run) in Shiny app
* shinydisconnect - for displaying nice error message if whole Shiny app disconnects
* shinyjs - for enabling and disabling download button in Shiny app
* stringr - for checking first_name and second_name inputs only contain letters in Shiny app
* tibble - used to convert rownames to column
* tidyr - to tidy messy data 
* tidyverse - for data manipulation
* tools - for getting file extension
* writexl - for exporting to an excel file

#### Installations
* R should already be installed (check version installed with R --version)
* Install the packages in RStudio using the install.packages() function


# Further information on the insect biodiversity meta-analyses Shiny app
### Files for the Shiny app are in christina_mma/meta_meta_analysis

#### Running the Shiny app
Use the following web address the access the Shiny app:
https://r26dnk-grace-skinner.shinyapps.io/meta_meta_analysis/ 

#### Description
The motivation behind making the Shiny app comes from a desire to make the best use of the data we have available now and in the future to untangle the complex trends and drivers of insect biodiversity change. 
The app performs what we call 'meta-meta-analysis' defined as analysing multiple meta-analysis studies together by combining effect sizes collected for each of these.
It allows users to run custom robust linear mixed-effects models to investigate their specific interests.
When new meta-analyses are conducted, the data can be uploaded and included in future models. 

#### Main features of the app
* Introductory tab to give overview of where the data comes from and its geographic representativeness.
* Agricultural systems tab where users can either run a defualt model (on all of the data) or a custom model based on choices made by the user.
    * The app runs robust models on the effect of agricultural system on biodiveristy in terms of log response ratio. 
    * In future, we aim to incorporate new variables over which the user has choice, including a choice of which threat to investigate (e.g. land-use change, temperature), and filters for location and taxonomic groups.
    * The app produces a figure and table of the results.
    * The user is able to download their results.
* Uploading data tab where new meta-analysis data can be uploaded to Google Sheets (where the data is stored). This data can then be included in future models. 
* Reference tab for more details enabling the user to find the original papers used in the study.   

#### Directories/Scripts
* meta_meta_analysis - this is the Shiny app directory
   * data_and_models - directory containing the agricultural systems definitions file
   * rsconnect - contains file with information on the deployed app
   * global.R - run once when app is launched. Packages, variables, and functions loaded/created here are available to both server.R and ui.R
   * server.R - defines how the Shiny app works (back-end development)
   * ui.R - defines the way the Shiny app looks (front-end development)
   * meta_meta_analysis.Rproj - R project file

#### Languages used to build the Shiny app
* R version 4.2.0

#### R packages used by the Shiny app 
* broom - for getting model summary
* bslib - for themes to customise appearance of shiny app
* googlesheets4 - for uploading files to googlesheets in Shiny app
* maps - for mapping
* readxl - for reading excel files
* robustlmm - for running robust models
* shiny version 1.7.1 - required to run any Shiny app
* shinycssloaders - for loading symbols (while models run) in Shiny app
* shinydisconnect - for displaying nice error message if whole Shiny app disconnects
* shinyjs - for enabling and disabling download button in Shiny app
* stringr - for checking first_name and second_name inputs only contain letters in Shiny app
* tibble - used to convert rownames to column
* tidyverse - for data manipulation
* tools - for getting file extension

#### Author name and contact
* Grace Skinner
* gls21@ic.ac.uk
