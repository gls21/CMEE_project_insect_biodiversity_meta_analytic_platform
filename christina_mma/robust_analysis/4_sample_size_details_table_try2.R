# Grace Skinner 23/6/22
### Try 2
# Input: 07.Excel_Dataset_to_model_LRR_LONG
# Output: A table containing details of the sample sizes of the data 

# ----------------------------------------------------------------------------------

# Load packages

library(readxl)
library(tidyverse)

# ----------------------------------------------------------------------------------

# Load data

data <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

# ----------------------------------------------------------------------------------

# Subset data to just include columns relevant to sample size

subset_data <- data[, c("Paper_number","Paper_ID", "Synthesis_type", "Title", "Author", "N_Studies", "Sample_size", "Treatment")]

# ----------------------------------------------------------------------------------

# Initialise empty data frame 
sample_sizes_table <- data.frame(Paper = character(), Number_of_agricultural_systems = numeric(), Number_of_instances = numeric())

for (i in unique(subset_data$Paper_ID)) {
  
  paper_subset <- subset_data %>%
    filter(Paper_ID == i)
  
  # make new row which will add to sample_sizes_table
  new_row <- data.frame(
    Paper = i,
    Number_of_agricultural_systems = length(unique(paper_subset$Treatment)), # number of unique agricultural systems
    Number_of_instances = nrow(paper_subset) # number of instances
  )
  
  sample_sizes_table <- rbind(sample_sizes_table, new_row)

}

# Put table in alphabetical order based on paper_ID
sample_sizes_table <- sample_sizes_table %>%
  arrange(Paper)


# -----------------------------------------------------------------------------------------

# Now would want to make a detailed table for each paper
# I just need to demonstrate this works with one example here
# In the shiny app, can just filter the chosen paper from user input and will do it for any paper automatically

# Bowles.T:

selected_paper <- subset_data %>%
  filter(Paper_ID == "Bowles.T") # in Shiny app, this would be something like input$paperchoice 

paper_details_table <- data.frame(Agricultural_system = character(), Number_of_instances = numeric())

for (i in unique(selected_paper$Treatment)) {
  
  subset_paper_by_treatment <- selected_paper %>%
    filter(Treatment == i)
  
  new_row <- data.frame(
    Agricultural_system = i,
    Number_of_instances = nrow(subset_paper_by_treatment)
  )
  
  paper_details_table <- rbind(paper_details_table, new_row)
}

# ===========================================================================================















