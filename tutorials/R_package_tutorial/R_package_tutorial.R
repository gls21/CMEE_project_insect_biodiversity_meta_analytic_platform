### R package tutorial https://ourcodingclub.github.io/tutorials/writing-r-package/


##########################################################################
# 1. Introduction: What is an R package?
##########################################################################

# Packages on CRAN are published for the R community 
# and installed in RStudio using the function install.packages.

# But not every R package is or should be uploaded to CRAN. 
# Packages can uploaded and downloaded from GitHub, 
# or even just built for personal use 
# (some R users have their own personal R packages with documented functions that they have written and regularly use in their own research).

# Here I will walk through the process of writing a very simple R package, 
# uploading it to GitHub, and downloading it from GitHub.


##########################################################################
# 2. Packages that need to be installed
##########################################################################

#install.packages("devtools") # contains a large bundle of tools needed in package development
#install.packages("roxygen2") # used to easily write documentation.


##########################################################################
# 3. The most basic R package
##########################################################################

# Assume that we want to create an R package that includes two functions. 
# The first function will convert temperatures from degrees Fahrenheit to degrees Celsius, 
# while the second function will convert temperatures from degrees Celsius to degrees Fahrenheit.

# The first thing we need to do is create a new folder somewhere on our computer 
# that will hold the whole R package
# Made folder called test_R_package in R_package_tutorial folder 
# Inside this folder make folder called R - this is where we will store the actual R scripts with the coded functions
# Any number of '.R' files can be included in the folder, and each file can have any number of functions.
# You could, for example, give each function its own file, or just have one file with many R functions. 
# For large projects, I find it easiest to group similar functions in the same R file. 
# In our new R package, I will write both functions in the same file called 'temp_conversion.R'

# See temp_conversion.R file 


###

# The next thing that we need to do is create a new file called DESCRIPTION 
# in the test_R_package directory (note, not in 'R', but just outside of it). 
# This will be a plain text file with no extension, and it will hold some of the meta-data on the R package. 
# For now, the whole file is just the following four lines of code, specifying the package name, type, title, and version number.
# Plain text file - On windows, press windows key and s, and then type notepad.
# Save name of file with double quotes around it to stop it adding an extension.


###

# This is technically an R package 
# Can load it like this:
# Set working directory as test_R_package
library(devtools)
load_all(".")

# The F_to_C and C_to_F are now read into R and we can use them
F_to_C(79)
C_to_F(20)


### Also need to include documentation explaining what the package is for and what functions do 


##########################################################################
# 4. Making a new R project
##########################################################################

# To make proper R package with documentation, best to create new R project
# Go to File, then New Project, and box will appear

# We will select existing directory to build on what we already have
# But could have started with this to start with if we wanted 

# Use test_R_package as the project working directory 
# After clicking 'create project' can see project inside the package directory (test_R_package.Rproj)


##########################################################################
# 5. Adding documentation (help files)
##########################################################################

# Make a help document as a markdown file using the roxygen2 package
# To do this, need to add to the functions written in temp_conversion.R

# The first line (e.g., #' Fahrenheit conversion) shows the function title, 
# with the next line showing the description. 
# Additional tags such as @param and @examples are used to write different subsections of the help file. 
# param - what the parameters are
# return - what the function returns
# examples - e.g.s of how the function can be used 
# export - the actual function itself 

# Still with test_R_package as working , and with the project open:
# Build the help files with the following
library(roxygen2)
roxygenise()

# We now have a man directory, which holds the help files we have written. Ours has 2 markdown documents, one for each function. 
# Also have a NAMESPACE directory, which works with R to integrate them into the package correctly 

# So can now ask for help with the F_to_C function:
?F_to_C


##########################################################################
# 6. Uploading to and installing from GitHub
##########################################################################

# Add another folder arbitrarily named 'notebook' to test_R_package directory
# This will hold various files that I want to be available to me in development,
# but not actually present in the R package 
# To make the R package ignore this, open the '.Rbuildignore' file in Rstudio and add the following line of code 
#notebook*
# This make it ignore everything that follows 'notebook' in the directory 
# Added a testing.Rmd file to the notebook folder as a test 


###
# Now need to initialise new Github repo - called it Test_R_package
# Added all the files within test_R_package tutorial
# Had to add .Rbuildignore separately because it didn't add like all the others did
# Also made README

# The R package is now live 
# Anyone can download it by using the install_github function in the devtools package
library(devtools)
install_github("gls21/Test_R_package") # install the package with this
library(SCCTempConverter) # then load the package

F_to_C(20) # test the package
?C_to_F


##########################################################################
# 7. Other useful things to know
##########################################################################

# Additional folders/files that could be useful:
# data - data files provided in the R package. Saved in .rda format 
# docs - documents for a website for the package
# src - compiled code that is used by your R functions e.g. code written in C to speed up computations
# tests - files to test your code to ensure it is running properly. Folder can be created using testthat R package 
# vignettes - large documentation files - more like a package guide than a simple help file for package functions


###
# Building a source package (zipped version of the R package)
# When the project is open, go to build, then build source package
# Creates zipped package which would be needed if we wanted to submit our package to CRAN

###
# Tagging a version
# tag a particular commit in git to identify a particular version of your R package
# git tag -a v0.0.1.0 -m "my first version of test R package"


