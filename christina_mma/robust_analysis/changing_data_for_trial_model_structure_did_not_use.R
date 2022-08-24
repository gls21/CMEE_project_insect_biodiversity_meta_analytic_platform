### Altering long_LRR spreadsheet - didn't actually end up using this version

# Condense control categories to match treatment categories
# Delete control rows (halve number of rows in of dataframe)

# Save this new version of the data

# Then can upload this new version to Google Sheets (and delete old version) - see save_christina_data_to_googlesheet.R
# Then change model formula used by the app to having Paper ID and Control as random variables - see server.R

# -------------------------------------------------------------------------------------------

# for running robust models
library(robustlmm)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl) # for exporting to an excel file

#### read in the data for meta-analyses
meta_data <- read_excel("../output_files/07.Excel_Dataset_to_model_LRR_LONG.xlsx")

# convert percentage change - adds extra column at end (column 52)
meta_data$percentage_change <- 100 * (exp(meta_data$LRR) - 1) 

# -------------------------------------------------------------------------------------------

#### all manipulations of Control treatment

# View unique Treatment and Control agricultural systems
unique(meta_data$Treatment)
unique(meta_data$Control)

meta_data$Control[meta_data$Control == "conventional"] <- "Conventional"
meta_data$Control[meta_data$Control == "traditional"] <- "Traditional"
meta_data$Control[meta_data$Control == "Conventional_NonWeeded"] <- "Conventional"
meta_data$Control[meta_data$Control == "Conventional_Weeded"] <- "Conventional"
meta_data$Control[meta_data$Control == "Intercrop"] <- "Mixed"
meta_data$Control[meta_data$Control == "mixed"] <- "Mixed"
meta_data$Control[meta_data$Control == "Monoculture"] <- "Conventional"

unique(meta_data$Treatment)
unique(meta_data$Control)

# Low_intensity, Low_vegetation_cover, ipm not in Treatment - need to exclude these data points
# Sustainable not in Control

length(meta_data$Control[meta_data$Control == "Low_intensity"]) # 2
length(meta_data$Control[meta_data$Control == "Low_vegetation_cover"]) # 2
length(meta_data$Control[meta_data$Control == "ipm"]) # 4

# Remove the 8 data points with these Control agricultural systems
meta_data <- meta_data %>%
  filter(Control != "Low_intensity" & Control != "Low_vegetation_cover" & Control != "ipm")

unique(meta_data$Treatment)
unique(meta_data$Control)

# So now have 10 treatment agricultural systems, and 9 control agricultural systems (all matching apart from no sustainable in Control categories)

# -------------------------------------------------------------------------------------------

# create column for treatment/control, and then extract all treatments that fall in control row
meta_data$Treat_Control <- rep(c("Treatment", "Control"), nrow(meta_data)/2)
control_vec <- meta_data$Treatment[meta_data$Treat_Control == "Control"]

# duplicate each control, and then assign to the Control column
meta_data$Control <- rep(control_vec, each = 2)

# Relevel data set so conventional is the reference category
meta_data$Treatment <- as.factor(meta_data$Treatment)
meta_data$Treatment <- relevel(meta_data$Treatment, ref = "Conventional")

# filter out any rows that are the control variable 
meta_data <- meta_data %>%
  dplyr::filter(Treat_Control != "Control")

# -------------------------------------------------------------------------------------------

### Try out modelling, extracting coefficients, and plotting with this new data 

# weight function for robust model
rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 2.28))

# Change random variables to Control and Paper_ID
# build robust model for shiny app, with control and paper as random intercepts
best_robust_control <- rlmer(LRR ~  Treatment + (1|Control) + (1|Paper_ID), data = meta_data,
                             rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                             rho.sigma.b = rsb)

# extract coefficients 
robust_coefficients_all <- best_robust_control %>% summary() %>% .$coefficients %>%
  round(digits = 2) %>% as.data.frame()

names(robust_coefficients_all) <- c("Original_LRR", "SE", "t")

# Adjust reference level (intercept = conventional) to 0.
# Take away the intercept (conventional) value from each level
robust_coefficients_all$Adjusted_LRR <- ""  # New column where I will store adjusted values
robust_coefficients_all$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

for (i in 1:length(robust_coefficients_all$Original_LRR)) { # Adjust other levels
  if (i > 0) {
    
    # Had to change this because when new data is added, it won't necessarily be 0.02 to take away
    # robust_coefficients_all$Adjusted_LRR[i] <- robust_coefficients_all$Original_LRR[i] - 0.02
    
    robust_coefficients_all$Adjusted_LRR[i] <- robust_coefficients_all$Original_LRR[i] - robust_coefficients_all$Original_LRR[1]
  }
}

# Change Adjusted_LRR class to numeric (came out as character after the for loop)
robust_coefficients_all$Adjusted_LRR <- as.numeric(robust_coefficients_all$Adjusted_LRR)

# Obtain other coefficients: CI, percentage_change (plus CI for percentage change)
robust_coefficients_all <- robust_coefficients_all %>%
  mutate(LCI = Adjusted_LRR - (1.96 * SE),
         UCI = Adjusted_LRR + (1.96 * SE)) %>%
  round(digits = 2) %>%
  mutate(CI = paste(LCI, "to", UCI)) %>%
  mutate(percentage_change = 100 * (exp(Adjusted_LRR) - 1),
         LCI_percent = 100 * (exp(LCI) - 1),
         UCI_percent = 100 * (exp(UCI) - 1)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(CI_percent = paste(LCI_percent, "to", UCI_percent))

# make rownames into a column, rather than just the rownames - so can use to plot later
robust_coefficients_all <- tibble::rownames_to_column(robust_coefficients_all, "Treatment")

# Change Treatment class to factor (came out as character after the for loop)
robust_coefficients_all$Treatment <- as.factor(robust_coefficients_all$Treatment)

robust_coefficients <- robust_coefficients_all

robust_coefficients <- robust_coefficients %>%
  dplyr::arrange(desc(Adjusted_LRR), Treatment)

robust_coefficients <- robust_coefficients %>%
  dplyr::filter(Treatment != "(Intercept)")

robust_coefficients %>%
  ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # reorder the x variable (treatments) in order of ascending Adjusted_LRR value
  geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
  geom_point(stat="identity", size = 4, colour = "black") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
  # ylim(min_LRR(), max_LRR()) + # will eventually be on x axis
  labs(x = "Agricultural system", y = "Adjusted log response ratio") +
  coord_flip() + # horizontal so looks more like a forest plot
  # annotate("text", x = signif_results_plus_0.25(), y = adjusted_LRR(),   ######### Maybe need better way of adding significance stars?
  #         label = "*", size = 8, colour = "blue") +
  theme_bw() +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank())

#ggsave("control_figure_4.png", scale = 1.2, dpi = 350)



# -------------------------------------------------------------------------------------------












