#### Making dummy data so can test uploading data to dropbox

# Set working directory to christina_mma

library(readxl) # for reading excel files
library(dplyr)

# --------------------------------------------------------------------------------------

#### Data:
dummy_data <- read_xlsx("meta_meta_analysis/data_and_models/christina_mma.xlsx")

# Original data to get size as csv file
write.csv(dummy_data, "dummy_data/dummy_data.csv", row.names = FALSE) # 383KB, 676 rows of data



# Take 100 rows at random
set.seed(123) # for reproducibility 
dummy_data1 <- dummy_data %>%
  sample_n(100)

# Save dummy data 1
write.csv(dummy_data1, "dummy_data/dummy_data1.csv", row.names = FALSE)


# Take different 100 rows at random
set.seed(1234) # for reproducibility 
dummy_data2 <- dummy_data %>%
  sample_n(100)

# Save dummy data 2
write.csv(dummy_data2, "dummy_data/dummy_data2.csv", row.names = FALSE)


# And another 100 rows at random 
set.seed(12345) # for reproducibility 
dummy_data3 <- dummy_data %>%
  sample_n(100)

# Save dummy data 3
write.csv(dummy_data3, "dummy_data/dummy_data3.csv", row.names = FALSE)


# And another 100 rows at random 
set.seed(123456) # for reproducibility 
dummy_data4 <- dummy_data %>%
  sample_n(100)

# Save dummy data 4
write.csv(dummy_data4, "dummy_data/dummy_data4.csv", row.names = FALSE)


# And another 100 rows at random but don't include LRR column 
set.seed(1234567) # for reproducibility 
dummy_data5_noLRR <- dummy_data %>%
  sample_n(100) %>%
  select(-LRR)

# Save dummy data 5
write.csv(dummy_data5_noLRR, "dummy_data/dummy_data5_noLRR.csv", row.names = FALSE)


##### Making dummy data to test efficiency of app in terms of how long it takes to upload files of varying sizes 

# Make files with 100,200,300,...,1000 rows 
for (i in seq(100,1000,100)){
  
  time_dummy_data <- dummy_data %>%
    sample_n(i, replace = TRUE)
  
  file_name <- paste0("dummy_data/time_dummy_data_", i, ".csv")
  
  write.csv(time_dummy_data, file_name, row.names = FALSE)
  
}








