library(tidyverse) ; library(fs) ; library(purrr)

setwd("C:/Users/klfiaa/OneDrive/KRC field course")
applicants = read.csv('2025_applicants_list.csv', header = T) %>%
  mutate(Motivation.letter = str_remove(Motivation.letter, "'"), #remove the ' before the file name
         CV = str_remove(CV, "'"),
         Motivation.letter = str_trim(Motivation.letter), #now remove beginning and trailing spaces
         CV = str_trim(CV))
head(applicants)
applicants$CV

rename_applicant_files <- function(applicants, directory) {
  # Ensure the directory path is correct
  file_paths <- dir(directory, full.names = TRUE)  # Get all file paths in directory
  file_names <- basename(file_paths)
  
  # Debugging: Print all detected files in the directory
  print("Files detected in the directory:")
  print(file_names)
  
  #iterate over each row in the dataframe
  pwalk(applicants, function(Surname, Name, Motivation.letter, CV, ...) {
    
    # Debugging: Print current row being processed
    print(paste("Processing:", Surname))
    
    # Rename Motivation Letter if the file exists
    if (Motivation.letter %in% file_names) {
      old_path <- file.path(directory, Motivation.letter)
      new_path <- file.path(directory, paste0(Surname, "_letter.pdf"))
      
      print(paste("Renaming:", old_path, "→", new_path))  # Debugging
      file.rename(old_path, new_path)
    } else {
      print(paste("Motivation Letter not found:", Motivation.letter))
    }
    
    # Rename CV if the file exists
    if (CV %in% file_names) {
      old_path <- file.path(directory, CV)
      new_path <- file.path(directory, paste0(Surname, "_CV.pdf"))
      
      print(paste("Renaming:", old_path, "→", new_path))  # Debugging
      file.rename(old_path, new_path)
    } else {
      print(paste("CV not found:", CV))
    }
  })
}

rename_applicant_files(applicants, directory = getwd())  # Change directory as needed

