##Data Import File
##Before trained on 6, 7, 105
##After trained on 6, 7, 105, 700
##BOTH+ configuration, resampling k = 10
##Import all data and organize into a data frame with the following variables:
##Time: Before/After
##Viewpoint: CP, I, SD, CPI, CPSD, ISD, CPISD
##MIDI pitch
##IC
##Entropy

library(tidyverse)

# Before -------------------------------------------------------------

file.names.before <- list.files(path = "IDyOM output/Before/", pattern = ".dat")

#format into data frame
format.data.before <- function(file.name.vector, time){
  data <- vector("list", length(file.names.before))
  for(i in seq_along(file.names.before)){
    file.name <- file.names.before[i]
    viewpoints <- unlist(str_extract_all(file.name, "cpitch|cpintfref|cpint"))
    viewpoints <- viewpoints[-1]
    file.path <- paste("IDyOM output/Before/", sep = "", file.name)
    file <- read_table2(file.path, col_names = TRUE)
    file <- file[,c(2:4, 16, 73:74)] #select melody.id, note.id, melody.name, cpitch, information.content, entropy
    file$Viewpoint <- str_c(viewpoints, collapse = "-")
    file$Time <- time
    data[[i]] <- file
  }
  data <- bind_rows(data)
}

before_data <- format.data.before(file.names.before, "Before")


# After -------------------------------------------------------------------

file.names.after <- list.files(path = "IDyOM output/After/", pattern = ".dat")
format.data.after <- function(file.name.vector, time){
  data <- vector("list", length(file.names.after))
  for(i in seq_along(file.names.after)){
    file.name <- file.names.after[i]
    viewpoints <- unlist(str_extract_all(file.name, "cpitch|cpintfref|cpint"))
    viewpoints <- viewpoints[-1]
    file.path <- paste("IDyOM output/After/", sep = "", file.name)
    file <- read_table2(file.path, col_names = TRUE)
    file <- file[,c(2:4, 16, 78:79)] #select melody.id, note.id, melody.name, cpitch, information.content, entropy
    file$Viewpoint <- str_c(viewpoints, collapse = "-")
    file$Time <- time
    data[[i]] <- file
  }
  data <- bind_rows(data)
}

after_data <- format.data.after(file.names.after, "After")

data <- rbind(before_data, after_data)
write.csv(data, file = "IDyOM_data.csv")
