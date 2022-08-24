# Grace Skinner 23/6/22
### Try 1
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

subset_data <- data[, c("Paper_number","Paper_ID", "Synthesis_type", "N_Studies", "Sample_size", "Treatment")]

# ----------------------------------------------------------------------------------

# Filter data to just be sustainable treatment and check there are 10 (as currently indicates in sample size table)
sustainable <- data %>%
  filter(Treatment == "Sustainable")

# ----------------------------------------------------------------------------------

# Data exploration

number_paper_number <- length(unique(subset_data$Paper_number))
number_paper_number # 23

number_paper_ID <- length(unique(subset_data$Paper_ID))
number_paper_ID # 22

# Why are these numbers different? 

# Find unique combinations of paper_number and paper_ID 
id_number <- unique(subset_data[,c("Paper_number","Paper_ID")]) # Delaquis.E appears twice

# Filter the data by this author 
dupl_author <- data %>%
  filter(Paper_ID == "Delaquis.E")
# Most of them have the paper ID as 36, but a couple have it as 35 - mistake?

# ----------------------------------------------------------------------------------

# Make sample sizes table 

# For now, just go by Paper_ID
sample_sizes_table <- as.data.frame(table(subset_data$Treatment))

# for each of the treatments, subset the data by this treatment, and count the number of unique studies, and calculate average instances per study 

# rename columns
colnames(sample_sizes_table) <- c("Agricultural_system", "Total_instances")

# Need to put agricultural systems as the rownames for the for loop to work correctly 
rownames(sample_sizes_table) <- sample_sizes_table$Agricultural_system


# Average instances isn't actually that helpful 
# Fill in table 
# for (i in c(sample_sizes_table$Agricultural_system)){
#   
#   # Filter by agricultural system
#   data_per_agri_system <- subset_data %>%
#     filter(Treatment == i)
#   
#   # make new column for Number of studies
#   sample_sizes_table[i, "Number_of_studies"] <- length(unique(data_per_agri_system$Paper_ID)) 
#   
#   # Within each agricultural system, filter further by paper_ID
#   vector <- c()
#   for (j in unique(data_per_agri_system$Paper_ID)) {
#     data_per_paper_ID <- data_per_agri_system %>%
#       filter(Paper_ID == j)
#     vector <- c(vector, nrow(data_per_paper_ID))
#   }
#   
#   sample_sizes_table[i, "Average_instances_per_study"] <- mean(vector)
# 
# }

# Because average instances isn't that helpful, 
# make a column which contains a vector of numbers, which represent the number of instances
# of that agricultural system in each study that includes it

# Fill in table 
for (i in c(sample_sizes_table$Agricultural_system)){
  
  # Filter by agricultural system
  data_per_agri_system <- subset_data %>%
    filter(Treatment == i)
  
  # make new column for Number of studies
  sample_sizes_table[i, "Number_of_studies"] <- length(unique(data_per_agri_system$Paper_ID)) 
  
  # Within each agricultural system, filter further by paper_ID
  vector <- c() # initialise empty vector
  for (j in unique(data_per_agri_system$Paper_ID)) { # for each paper that studies this agricultural_system
    data_per_paper_ID <- data_per_agri_system %>%
      filter(Paper_ID == j) # filter the data by each paper 
    vector <- c(vector, nrow(data_per_paper_ID)) # add to the vector the number of instances of the agri system included in this paper
  }
  
  sample_sizes_table[i, "Instances_per_study"] <- toString(sort(vector)) # convert to string and put in ascending order 
  
}

# round numbers in average column - don't need to do anymore 
# sample_sizes_table$Average_instances_per_study <- round(sample_sizes_table$Average_instances_per_study, digits = 1)


















