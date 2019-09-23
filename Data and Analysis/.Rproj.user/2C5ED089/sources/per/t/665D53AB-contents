##Import and preparation of human data for analysis

library(tidyverse)

##Compile relevant file names
file.names <- list.files(pattern = ".txt")
#keep only files including "Before" or "After"
file.names.index <- grep(pattern = "Before|After", x = file.names)
file.names <- file.names[file.names.index]

#format into data frame
format.data <- function(file.name.vector){
  #Set up empty data frames to fill
  data <- vector("list", length = 5)
  practice1data <- NULL
  practice2data <- NULL
  predictiondata <- NULL
  closuredata <- NULL
  precisiondata <- NULL
  for(i in seq_along(file.names)){
    #File name details
    file.name <- file.names[i]
    part.id <- str_extract(file.name, "\\d\\d\\d")
    time <- str_extract(file.name, "Before|After")
    
    #----------------------------------------------------
    #Import file
    rawdata <- readLines(file.name, skipNul = TRUE)
    
    #----------------------------------------------------
    #Extract relevant information
    #Collect Procedure ID
    procedure.index <- grep(pattern = "Procedure", rawdata, useBytes = TRUE)
    procedure.info <- rawdata[procedure.index]
    mel.procedure.index <- grep(pattern = "Mel", procedure.info, useBytes = TRUE)
    procedure.info <- procedure.info[mel.procedure.index]
    
    #Collect played file info
    file.index <- grep(pattern = "filename:", rawdata, useBytes = TRUE)
    file.info <- rawdata[file.index]
    
            #There are repeated files in file.info, corresponding to extra Procedure information; remove the file info matching non-Mel procedures
            procedure.index <- c(1:length(procedure.index))
            indices.to.remove <- setdiff(procedure.index, mel.procedure.index)
            file.info <- file.info[-indices.to.remove]
    
    #Collect prediction response
    prediction.index <- grep(pattern = "PredResp", rawdata, useBytes = TRUE)
    prediction.collection <- rawdata[prediction.index]
    prediction.response.index <- grep(pattern = "RESP", prediction.collection, useBytes = TRUE)
    prediction.responses <- prediction.collection[prediction.response.index]
    
    #Collect prediction RT
    prediction.RT.index <- grep(pattern = "RT", prediction.collection, useBytes = TRUE)
    prediction.RTs <- prediction.collection[prediction.RT.index]
    
    #Collect closure response
    closure.index <- grep(pattern = "ClosResp", rawdata, useBytes = TRUE)
    closure.collection <- rawdata[closure.index]
    closure.response.index <- grep(pattern = "RESP", closure.collection, useBytes = TRUE)
    closure.responses <- closure.collection[closure.response.index]
    
    #Collect closure RT
    closure.RT.index <- grep(pattern = "RT", closure.collection, useBytes = TRUE)
    closure.RTs <- closure.collection[closure.RT.index]
    
    #Collect precision response
    precision.index <- grep(pattern = "CertResp", rawdata, useBytes = TRUE)
    precision.collection <- rawdata[precision.index]
    precision.response.index <- grep(pattern = "RESP", precision.collection, useBytes = TRUE)
    precision.responses <- precision.collection[precision.response.index]
    
    #Collect precision RT
    precision.RT.index <- grep(pattern = "RT", precision.collection, useBytes = TRUE)
    precision.RTs <- precision.collection[precision.RT.index]
    
    #Clean up -------------------------------------------
    #Remove Garbage
    clean_vector <- function(x){
      x <- str_split_fixed(string = x, pattern = " ",n = 2)
      x <- x[,2]
    }
    
    procedure.info <- clean_vector(procedure.info)
    file.info <- clean_vector(file.info)
    prediction.responses <- clean_vector(prediction.responses)
    prediction.RTs <- clean_vector(prediction.RTs)
    closure.responses <- clean_vector(closure.responses)
    closure.RTs <- clean_vector(closure.RTs)
    precision.responses <- clean_vector(precision.responses)
    precision.RTs <- clean_vector(precision.RTs)
    
    #Clean up file info a bit more, dividing into melody and note number
    file.info <- str_remove(file.info, ".wav")
    melody.ids <- unlist(str_extract_all(file.info, pattern = "PracMel1|PracMel2|SP|SI|SR|SRI|DQ1|DQ2|TB1|TB2|WL1|WL2|WL3|WL4|WQ1|WQ2|WQ3|WQ4"))
    note.ids <- unlist(str_sub(file.info, -2))
    
    
    #---------------------------------------------------
    #Group by rating type
    #---------------------------------------------------
    
    #Practice trials -----------------------------------
    #They are always five notes long and always contain one example of each type of rating so the first five values of each rating type = practice
    #Prediction and closure correspond to PracMel1 and precision to PracMel2
    
    #PracMel1
    pracmel1.index <- grep(pattern = "PracMel1", procedure.info)
    #Collect all relevant info
    pracmel1.info <- procedure.info[pracmel1.index]
    pracmel1.files <- file.info[pracmel1.index]
    pracmel1.prediction.resp <- prediction.responses[1:5]
    pracmel1.prediction.RT <- prediction.RTs[1:5]
    pracmel1.closure.resp <- closure.responses[1:5]
    pracmel1.closure.RT <- closure.RTs[1:5]
    
    pracmel1 <- as.tibble(cbind(pracmel1.info, pracmel1.files, pracmel1.prediction.resp, pracmel1.prediction.RT, pracmel1.closure.resp, pracmel1.closure.RT))
    
    #PracMel2
    pracmel2.index <- grep(pattern = "PracMel2", procedure.info)
    #Collect all relevant info
    pracmel2.info <- procedure.info[pracmel2.index]
    pracmel2.files <- file.info[pracmel2.index]
    pracmel2.precision.resp <- precision.responses[1:5]
    pracmel2.precision.RT <- precision.RTs[1:5]
    
    pracmel2 <- as.tibble(cbind(pracmel2.info, pracmel2.files, pracmel2.precision.resp, pracmel2.precision.RT))
    
    #Prediction/closure trials ------------------------
    predclose.index <- grep(pattern = "PredClos", procedure.info)
    #Collect all relevant info
    predclose.info <- procedure.info[predclose.index]
    predclose.files <- file.info[predclose.index]
    predclose.prediction.resp <- prediction.responses[6:length(prediction.responses)]
    predclose.prediction.RT <- prediction.RTs[6:length(prediction.RTs)]
    predclose.closure.resp <- closure.responses[6:length(closure.responses)]
    predclose.closure.RT <- closure.RTs[6:length(closure.RTs)]
    
    pred <- as.tibble(cbind(predclose.info, predclose.files, predclose.prediction.resp, predclose.prediction.RT))
    clos <- as.tibble(cbind(predclose.info, predclose.files, predclose.closure.resp, predclose.closure.RT))
    
    #Precision trials ---------------------------------
    cert.index <- grep(pattern = "Cert", procedure.info)
    #Collect all relevant info
    cert.info <- procedure.info[cert.index]
    cert.files <- file.info[cert.index]
    cert.precision.resp <- precision.responses[6:length(precision.responses)]
    cert.precision.RT <- precision.RTs[6:length(precision.responses)]
    
    cert <- as.tibble(cbind(cert.info, cert.files, cert.precision.resp, cert.precision.RT))
    
    #-------------------------------------------------
    #Clean up each rating type dataframe
    #-------------------------------------------------
    
    #PracMel1
    pracmel1$Participant <- part.id
    pracmel1$Time <- time
    prac1.procedure.index <- grep(pattern = "PracMel1", procedure.info)
    pracmel1$MelodyID <- melody.ids[prac1.procedure.index]
    pracmel1$NoteID <- note.ids[prac1.procedure.index]
    pracmel1 <- select(pracmel1, Time, Participant, MelodyID, NoteID, pracmel1.prediction.resp, pracmel1.prediction.RT, pracmel1.closure.resp, pracmel1.closure.RT)
    colnames(pracmel1) <- c("Time", "Participant", "MelodyID", "NoteID", "PredictionRESP", "PredictionRT", "ClosureRESP", "ClosureRT")
    
    #PracMel2
    pracmel2$Participant <- part.id
    pracmel2$Time <- time
    prac2.procedure.index <- grep(pattern = "PracMel2", procedure.info)
    pracmel2$MelodyID <- melody.ids[prac2.procedure.index]
    pracmel2$NoteID <- note.ids[prac2.procedure.index]
    pracmel2 <- select(pracmel2, Time, Participant, MelodyID, NoteID, pracmel2.precision.resp, pracmel2.precision.RT)
    colnames(pracmel2) <- c("Time", "Participant", "MelodyID", "NoteID", "PrecisionRESP", "PrecisionRT")
    
    #Prediction
    pred$Participant <- part.id
    pred$Time <- time
    pred$RatingType <- "Prediction"
    predclos.procedure.index <- grep(pattern = "PredClos", procedure.info)
    pred$MelodyID <- melody.ids[predclos.procedure.index]
    pred$NoteID <- note.ids[predclos.procedure.index]
    pred <- select(pred, Time, Participant, MelodyID, NoteID, RatingType, predclose.prediction.resp, predclose.prediction.RT)
    colnames(pred) <- c("Time", "Participant", "MelodyID", "NoteID", "RatingType", "Rating", "RT")
    
    #Closure
    clos$Participant <- part.id
    clos$Time <- time
    clos$RatingType <- "Closure"
    predclos.procedure.index <- grep(pattern = "PredClos", procedure.info)
    clos$MelodyID <- melody.ids[predclos.procedure.index]
    clos$NoteID <- note.ids[predclos.procedure.index]
    clos <- select(clos, Time, Participant, MelodyID, NoteID, RatingType, predclose.closure.resp, predclose.closure.RT)
    colnames(clos) <- c("Time", "Participant", "MelodyID", "NoteID", "RatingType", "Rating", "RT")
    
    #Precision
    cert$Participant <- part.id
    cert$Time <- time
    cert$RatingType <- "Precision"
    cert.procedure.index <- grep(pattern = "Cert", procedure.info)
    cert$MelodyID <- melody.ids[cert.procedure.index]
    cert$NoteID <- note.ids[cert.procedure.index]
    cert <- select(cert, Time, Participant, MelodyID, NoteID, RatingType, cert.precision.resp, cert.precision.RT)
    colnames(cert) <- c("Time", "Participant", "MelodyID", "NoteID", "RatingType", "Rating", "RT")
  }
  
  ##Combine all participants' data
  practice1data <- rbind(practice1data, pracmel1)
  practice2data <- rbind(practice2data, pracmel2)
  predictiondata <- rbind(predictiondata, pred)
  closuredata <- rbind(closuredata, clos)
  precisiondata <- rbind(precisiondata, cert)
  ##Insert data frame of each rating type into a list for output
  data[[1]] <- as.data.frame(practice1data)
  data[[2]] <- as.data.frame(practice2data)
  data[[3]] <- as.data.frame(predictiondata)
  data[[4]] <- as.data.frame(closuredata)
  data[[5]] <- as.data.frame(precisiondata)
  data
}

#create data frame by applying function
data <- format.data(file.names)

##Write data to files, keeping practice data in case needed for exploratory analysis
write.csv(data[[1]], "PracticeMel1Data.csv")
write.csv(data[[2]], "PracticeMel2Data.csv")

#Combine rating types into one file of human data
human.data <- as.data.frame(rbind(data[[3]], data[[4]], data[[5]]))
human.data$Participant <- as.numeric(human.data$Participant)
human.data$Time <- as.factor(human.data$Time)
human.data$MelodyID <- as.numeric(human.data$MelodyID)
human.data$NoteID <- as.numeric(human.data$NoteID)
human.data$RatingType <- as.factor(human.data$RatingType)
human.data$Rating <- as.numeric(human.data$Rating)
human.data$RT <- as.numeric(human.data$RT)
#Write to file
write.csv(human.data, "Human_data.csv")
