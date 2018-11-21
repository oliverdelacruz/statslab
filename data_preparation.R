#####################################################
### load data
#####################################################
rm(list=ls())
cat("\014")  
path_data <-  "../data/"

# Load data from students
d1 <- readRDS(paste(path_data,"dBel.rds",sep=""))
str(d1)
head(d1)

# Load data from quiz results
d2 <- readRDS(paste(path_data,"quizAnonym.rds",sep=""))
str(d2)
head(d2, 1)

# Load data from list of quizzes available
d3 <- readRDS(paste(path_data,"quizList.rds",sep=""))
str(d3)
head(d3)

# Load data from log file 
d4 <- readRDS(paste(path_data,"logDataAnonym.rds",sep=""))
str(d4)
head(d4)
table(d4$Ereigniskontext)
table(d4$Ereignisname)

# Load data from group selection
rs1 <- read.csv(paste(path_data,"CourseRoomSelectionsHS16_PV1.csv",sep=""), header = T, sep=";")
rs2 <- read.csv(paste(path_data,"CourseRoomSelectionsHS16_PV2.csv",sep=""), header = T, sep=";")
rs3 <- read.csv(paste(path_data,"CourseRoomSelectionsHS16_PV3.csv",sep=""), header = T, sep=";")
rs4 <- read.csv(paste(path_data,"CourseRoomSelectionsHS16_PV4.csv",sep=""), header = T, sep=";")
rs5 <- read.csv(paste(path_data,"CourseRoomSelectionsHS16_PV5.csv",sep=""), header = T, sep=";")
head(rs1)

# Load data from profile/group roles
gr1 <- read.csv(paste(path_data,"grouproles_sept.csv",sep=""), header = T, sep=";")
gr2 <- read.csv(paste(path_data,"grouproles_dec.csv",sep=""), header = T, sep=";")
head(gr1)

# Load data from exam results
ex1 <- read.csv(paste(path_data,"exam_jan17.csv",sep=""), header = T, sep=";")
head(ex1)

##########################################################################################################
### Moodle-Quiz-Data
##########################################################################################################

#####################################################
### Data cleaning 
#####################################################
library(plyr)
library(dplyr)
rows_data <- NROW(d2)

for(i in 1:rows_data){
  # get quiz data for the ith quiz
  d.t <- cbind("quiz_nr" = i, "attempt" = 0, d2[[i]][, -c(1,2)])

  # index of the total score
  index.total  <-  7
  
  # split colnames to extract colname/max value
  strings <- strsplit(colnames(d.t), "[.]")

  # do we have any scores? (not the case for 23, 33, 52)
  if(strings[[index.total]][1] == "Bewertung"){
    
      # remove maximum value from total score colname
      colnames(d.t)[index.total] <- strings[[index.total]][1]
      
      # convert chr to num
      d.t[,index.total:dim(d.t)[2]] <- as.numeric(as.matrix(d.t[,index.total:dim(d.t)[2]]))
    
      # what is the maximum of the total cnt? 
      max.points.total <- as.numeric(strings[[index.total]][2]) + as.numeric(strings[[index.total]][3])/100
    
      # standardize total cnt
      d.t[,index.total] <- d.t[,index.total] / max.points.total
      
      # handling all the subscores available in quiz i
      for(j in (index.total+1):(length(strings)-1)){
        # prittify subscore colnames
        colnames(d.t)[j] <- paste(strings[[j]][1], strings[[j]][2], sep="")
        
        # standardize subscore values
        max.points.sub <- as.numeric(strings[[j]][4]) + as.numeric(strings[[j]][5])/100
        if (max.points.sub ==0) {
          d.t[,j] <- 0
        }else {
          d.t[,j] <- round(d.t[,j]/max.points.sub, digits = 4)
        }
      }
      cat(i, " | ")
  } else{
    cat(i, " error - no values | ")
  }
 
  # covert dates for pre-ordering
  d.original<- d.t[, "Begonnen.am"] 
  d.t[, "Begonnen.am"]  <- gsub("Oktober", "October", d.t[, "Begonnen.am"] )
  d.t[, "Begonnen.am"]  <- gsub("Dezember", "December", d.t[, "Begonnen.am"] )
  d.t[, "Begonnen.am"]  <- gsub("Januar", "January", d.t[, "Begonnen.am"] )
  d.t[, "Begonnen.am"]  <- gsub("Februar", "February", d.t[, "Begonnen.am"] )
  d.t[, "Beendet"]  <- gsub("Oktober", "October", d.t[, "Beendet"])
  d.t[, "Beendet"] <- gsub("Dezember", "December", d.t[, "Beendet"])
  d.t[, "Beendet"]  <- gsub("Januar", "January", d.t[, "Beendet"] )
  d.t[, "Beendet"] <- gsub("Februar", "February", d.t[, "Beendet"])
  d.t[, "Begonnen.am"]  <- as.POSIXct( d.t[, "Begonnen.am"], format = "%d. %B %Y %H:%M", tz = "")
  d.t[, "Beendet"]  <- as.POSIXct(d.t[, "Beendet"], format = "%d. %B %Y %H:%M", tz = "")
  
  # enrich the data with the attempt
  d.t$ones <- 1
  agg <- aggregate(ones~id, data=d.t, cumsum)
  d.t[order(d.t$id,d.t$Begonnen.am),"attempt"] <- unlist(agg$ones)
  d.t$ones <- NULL

  # enrich quiz data with "Departement", "Studiengang", etc. 
  result_quiz <- inner_join(d1, d.t, by=c("id"))
  
  # rbind with previous quizes
  if(i == 1)
    results <- result_quiz
  if(i != 1)
    results <- unique(rbind.fill(results, result_quiz))
}
dim(results)

# convert Begonnen.am/Beendet into date/time
results$Start.time   <- results[, "Begonnen.am"]
results$End.time     <- results[, "Beendet"]
results$used.time.min <- as.numeric(difftime(results$End.time, results$Start.time, units = "m"))
temp_list <- strsplit(results$Verbrauchte.Zeit, " ")
f_duration_count <- function(x){
  if(is.na(x[2])){
    x[2] <- "" 
  }
  if(x[2] == "Sekunden"){
    res <- as.numeric(x[1])/60
  }else {
    res <- as.numeric(x[1]) + as.numeric(x[3]) /60
  }
}
results$duration <- as.numeric(lapply(temp_list, f_duration_count)) 

# rearange columns 
results <- results[,c("id", "quiz_nr", "attempt", "Departement", "Studiengang", "Fachrichtung", "Studienplansemester", "Status", "Start.time", "End.time","used.time.min", "duration", "Verbrauchte.Zeit", "Bewertung", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11")]
results <- results[order(results$id, results$quiz_nr, results$attempt),]

# check if id/quiz_nr/attempt is key
nrow(unique(results)) == nrow(unique(results[,1:3]))

# what does the result look like? 
dim(results)
head(results)

# what we made here is called moodle_quiz_data
moodle_quiz_data <- results

#####################################################
### aggregated/calculated values
#####################################################
## summary totalscore per quiz and id
data <- moodle_quiz_data[,c("id", "quiz_nr","attempt", "Bewertung")]

# max
agg <- aggregate(Bewertung~id+quiz_nr, data=data, max) 
colnames(agg)[3] <- "Max.total.score" 
moodle_quiz_data <- left_join(moodle_quiz_data, agg, by=c("id", "quiz_nr"))

# mean
agg <- aggregate(Bewertung~id+quiz_nr, data=data, mean) 
colnames(agg)[3] <- "Avg.total.score" 
moodle_quiz_data <- left_join(moodle_quiz_data, agg, by=c("id", "quiz_nr"))

# min
agg <- aggregate(Bewertung~id+quiz_nr, data=data, min) 
colnames(agg)[3] <- "Min.total.score" 
moodle_quiz_data <- left_join(moodle_quiz_data, agg, by=c("id", "quiz_nr"))

## count of subscores available
data <- moodle_quiz_data[,c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11")]
cnt.subscores.f <- function(r) sum(!is.na(r))
moodle_quiz_data$cnt.subscores.available <-  apply(data,1, cnt.subscores.f)

## average of available subscores
avg.subscores.f <- function(r) mean(r[which(!is.na(r))])
moodle_quiz_data$avg.subscore <-  apply(data, 1, avg.subscores.f)

## what it looks like now:
head(moodle_quiz_data)
dim(moodle_quiz_data)

###################################################
## save the object:
###################################################
try(saveRDS(moodle_quiz_data, file= paste(path_data,"moodle_quiz_data.rds",sep="")))

###################################################
## Tests
###################################################
table(moodle_quiz_data$quiz_nr)

##########################################################################################################
### Room-Selection
##########################################################################################################
head(rs1)
rs <- rs1

## Extract the number from the string and join all the groups together. 
loop_rs <- function(df, session){
  
  df <- df[!(df$id == "#NV"),]
  df$id <- as.numeric(as.character(df$id))
  df$grp_old <- as.character(df$grp)
  df$grp <- numeric(nrow(df))
  
  for(j in 1:nrow(df))
    df$grp[j] <- as.numeric(strsplit(df$grp_old[j], split=" ")[[1]][2])
  
  colnames(df) <- c("id", paste0("session_", session), "old")
  
  if(session == 1)
    assign("rs", df[,c(1:2)], envir = .GlobalEnv)
  else
    assign("rs", full_join(get("rs", envir = .GlobalEnv), df[,c(1:2)], by=c("id" = "id")), envir = .GlobalEnv)
}

head(rs)
loop_rs(rs1, 1)
loop_rs(rs2, 2)
loop_rs(rs2, 3)
loop_rs(rs2, 4)
loop_rs(rs2, 5)
head(rs, 20)

###################################################
## save the object:
###################################################
room_selection <- rs
try(saveRDS(room_selection, file= paste(path_data,"room_selection.rds",sep="")))

##########################################################################################################
### Grouproles
##########################################################################################################
summary(gr1)

colnames(gr1) <- c("id", paste0(colnames(gr1)[-1], "_1"))
colnames(gr2) <- c("id", paste0(colnames(gr2)[-1], "_2"))

gr <- full_join(gr1, gr2, by=c("id" = "id"))
head(gr)
###################################################
## save the object:
###################################################
grouproles <- gr
try(saveRDS(grouproles, file= paste(path_data,"grouproles.rds",sep="")))

##########################################################################################################
### Exam
##########################################################################################################
ex1 <- ex1[!(ex1$id == "#NV"),]

ex1$id <- as.numeric(as.character(ex1$id))
str(ex1)


ex1$Rep <- ifelse(ex1$Rep. == "Rep.", 1, 0)
ex1$Rep. <- NULL

if(is.factor(ex1$Failed)){
  ex1$Failed <- ifelse(ex1$Failed == "ja", 1, 0)
}
exam <- na.omit(ex1)

###################################################
## save the object:
###################################################
try(saveRDS(exam, file= paste(path_data,"exam.rds",sep="")))

##########################################################################################################
### Mastertable
##########################################################################################################
head(moodle_quiz_data)
head(exam)
head(grouproles)
head(room_selection)

mt1 <- moodle_quiz_data
mt2 <- right_join(mt1, exam, by=c("id" = "id"))
mt3 <- left_join(mt2, grouproles, by = c("id" = "id"))
mt4 <- left_join(mt3, room_selection, by = c("id" = "id"))

stopifnot(nrow(moodle_quiz_data) == nrow(mt4))
mastertable <- mt4
head(mastertable)

###################################################
## save the object:
###################################################
try(saveRDS(mastertable, file= paste(path_data,"mastertable.rds",sep="")))

##########################################################################################################
### Exploratory data analysis
##########################################################################################################
# Read files
rm(list=ls())
cat("\014") 
path_data <- "C:\\ETH\\StatsLab\\data\\"
master_data <- readRDS(paste(path_data,"mastertable.rds",sep=""))
log_data <-  readRDS(paste(path_data,"logDataAnonym.rds",sep=""))
course_data <-  readRDS(paste(path_data,"quizList.rds",sep=""))
exam_data <-  readRDS(paste(path_data,"exam.rds",sep=""))

# Add libraries
library(psych)
library(corrplot)

# Formatting data - log data
colnames(log_data)
col_log <- c( "idVollstName", "idBetroffNutzer","Zeit","Ereigniskontext",
              "Komponente" ,"Ereignisname", "Beschreibung")
log_data <- log_data[,col_log] 
log_data$Zeit <- as.POSIXct(log_data$Zeit, format = "%d.%m.%Y %H:%M", tz = "")

# Select ids that have grades
id_students <- unique(c(master_data$id))
NROW(id_students)
NROW(log_data[!(log_data$idVollstName %in% id_students) ,])
log_data <- log_data[log_data$idVollstName %in% id_students ,]

# Difference of students interacted with moodle and those who not
NROW(id_students) - NROW(unique(log_data$idVollstName))

# Remove students who did not interact with moodle and went to the exam
id_students_not_participated <- c(setdiff(id_students,log_data$idVollstName))
exam_data <- exam_data[!(exam_data$id %in% id_students_not_participated),]
master_data <- master_data[!(master_data$id %in% id_students_not_participated),]

# Plot data
table_quiz <- table(master_data$quiz_nr)
plot(table_quiz,ylab ="Participation - Counts", xlab="Quiz")
hist(exam_data$Grade,breaks=10,main = "Histogram - Grades", xlab= "Grades")

# Analysis
log_data$Beschreibung <- gsub('[[:digit:]]+|The user with id+|id.*|with.*|[[:punct:]]|^\\s+|\\s+$', "" , log_data$Beschreibung)
table_analysis <-  as.data.frame(table(log_data$idVollstName,log_data$Komponente,log_data$Beschreibung))
grades <- as.matrix(exam_data[order(exam_data$id), "Total"])


# Check length of the matrices
nid <-  NROW(unique(table_analysis$Var1))
ncomp <- NROW(unique(table_analysis$Var2))
nstatus <-  NROW(unique(table_analysis$Var3))
ngrades <- NROW(exam_data)
ngrades * nstatus * ncomp
NROW(table_analysis$Freq)

# Determine correlations 
comp <- paste0(rep(as.character(unique(table_analysis$Var2))), " - ", rep(as.character(unique(table_analysis$Var3)),each = ncomp))
count_test <-  matrix(c(table_analysis$Freq),nrow=NROW(exam_data),ncol= ncomp*nstatus)
colnames(count_test) <- comp
test.corr <- c()
for (i in 1:NCOL(count_test)){
  res.test <- cor.test(count_test[,i],grades)
  test.corr[i] <- res.test[[3]]
}
corr_coef <- cor(count_test,grades)
correlation_output <- as.data.frame(na.omit(cbind(corr_coef,test.corr)))
write.csv(correlation_output,file = paste(path_data, "correlation_log.csv", sep="")) 

# Correlation matrix
count_select <- count_test[, colSums(count_test)> 5]
ff <- table_analysis$Var1[1:246]
write.csv(ff ,file =  paste(path_data, "Variableds.csv", sep="")) 
write.csv(cbind(ff,count_select) ,file = paste(path_data, "Variables.csv", sep=""))
test_holm <- corr.test(count_select,grades,adjust="holm")
test_holm_self <- corr.test(count_select,adjust="holm")
write.csv(test_holm$p,file = paste(path_data, "test_corr.csv", sep=""))
write.csv(test_holm_self$r ,file = paste(path_data, "self_corr.csv", sep=""))

# Plot correlations
names <- colnames(count_select) 
colnames(count_select) <- seq(1:26)
colnames(count_select) <- names
corr_mat <- cor(count_select)
corrplot(corr_mat,tl.cex = 0.5, cl.offset = 20)
legend(2000,9.5,legend = names)