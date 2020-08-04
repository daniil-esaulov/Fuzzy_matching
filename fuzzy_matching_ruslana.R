#load data
#table ICSID at first
df <- read.csv2("Raw_VET_0708v2.csv", header = T, blank.lines.skip = TRUE, stringsAsFactors = FALSE)
#take necessary columns
sdf <- df[,c("reg_idnew", "inn", "name", "address", "firm")]
#delete symbols of new line and return of carriage
library(stringr)
sdf$firm <- str_replace_all(sdf$firm, "[\r\n]" , " ")
sdf$address <- str_replace_all(sdf$address, "[\r\n]" , " ")
sdf$address <-  gsub("\\s+", " ",sdf$address)

#add column with city
sdf$city <- sub(".*?((г\\..*?)|(с\\..*?)|(город .*?)|( г .*?)|(гор\\..*?)|(пгт\\..*?)|(пгт.*?)|(п\\..*?)|(п .*?)|(пос\\..*?)|(дер\\..*?)|(р\\.п\\..*?)|(рп .*?)|(рабочий поселок.*?)|(село .*?)|(с .*?)|(ст-ца.*?)|(станица.*?)|(совхоз.*?))((\\,.*$)|$|( ул\\..*$)|(улица.*$)|(пр\\..*$)|(\\(.*$))", "\\1", sdf$address, perl = TRUE, ignore.case = TRUE)

#add missing value in columns with values from the nonempty row above
library(zoo)
sdf$inn[sdf$inn == ""] <- NA
sdf$name[sdf$name == ""] <- NA
sdf$address[sdf$address == ""] <- NA
sdf$city[sdf$city == ""] <- NA
sdf$inn <- na.locf(sdf$inn)
sdf$name <- na.locf(sdf$name)
sdf$address <- na.locf(sdf$address)
sdf$city <- na.locf(sdf$city)
sdf$reg_idnew <- na.locf(sdf$reg_idnew)

#assign row_id to each row
library(plyr)
library(dplyr)
row_id<- as.numeric(rownames(sdf))
sdf<-mutate(sdf, row_id)

#remove all rows with blank or zero ("0") "firm" cell
library(stringi)
sdf2 <- sdf[!(stri_isempty(sdf$firm)) & sdf$firm!="0" ,]
#take 4 columns
sdf2 <- sdf2[,c("row_id", "reg_idnew", "firm", "city")]

#add columns for "firm" and "city" editing
sdf2 <- mutate(sdf2, ed_firm=firm)
sdf2 <- mutate(sdf2, ed_city=city)

#extract just the city name
sdf2$ed_city <-gsub(paste0('\\<', "город", '\\>'), paste(""), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "село", '\\>'), paste(""), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "рабочий поселок", '\\>'), paste(""), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "станица", '\\>'), paste(""), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "колхоз", '\\>'), paste(""), sdf2$ed_city, ignore.case = TRUE)

sdf2$ed_city <-gsub(paste0('\\<', "г", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "п", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "с", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "р", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "к", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "о", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "т", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "гп", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "рп", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "сп", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "го", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "гк", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "пгт", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "пос", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "дер", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "гор", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)
sdf2$ed_city <-gsub(paste0('\\<', "ст-ца", '\\>'), paste(" "), sdf2$ed_city, ignore.case = TRUE)

#Change hyphen "-" and dash " - " to space " "
sdf2$ed_city <- gsub("-", " ", sdf2$ed_city)
sdf2$ed_city <- gsub("–", " ", sdf2$ed_city)
sdf2$ed_city <- gsub("—", " ", sdf2$ed_city)

#Change hyphen "-" and dash " - " to space " " in "firm" column
sdf2$ed_firm <- gsub("-", " ", sdf2$ed_firm)
sdf2$ed_firm <- gsub("–", " ", sdf2$ed_firm)
sdf2$ed_firm <- gsub("—", " ", sdf2$ed_firm)

#separate firm name if parentheses. make 2 new columns with info inside and outside of parentheses
sdf2$ed_firm_par_in <- lapply(sdf2$ed_firm, function(a) {
  gsub("[\\(\\)]", "", regmatches(a, gregexpr("\\(.*?\\)", a)))
})

sdf2$ed_firm_par_in<- unlist(sdf2$ed_firm_par_in)

sdf2$ed_firm_par_out <- lapply(sdf2$ed_firm, function(a) {
  sub('\\([^"]*\\)', "", a)
})

sdf2$ed_firm_par_out<- unlist(sdf2$ed_firm_par_out)

sdf2$is_parenthes <- !(sdf2$ed_firm_par_in=="character0")
sdf2$ed_firm_par_in <- lapply(sdf2$ed_firm_par_in, function(a) {
  gsub("character0", "", a)
})

sdf2$ed_firm_par_in <- unlist(sdf2$ed_firm_par_in)

#remove all punctuation from the copy of sdf
sdf2$ed_firm <- gsub("[[:punct:]]"," ", enc2utf8(sdf2$ed_firm))
sdf2$ed_firm_par_in <- gsub("[[:punct:]]"," ", enc2utf8(sdf2$ed_firm_par_in))
sdf2$ed_firm_par_out <- gsub("[[:punct:]]"," ", enc2utf8(sdf2$ed_firm_par_out))

sdf2$ed_city <- gsub("[[:punct:]]"," ", enc2utf8(sdf2$ed_city))

#convert to lower case
sdf2$ed_firm <- tolower(sdf2$ed_firm)
sdf2$ed_firm_par_in <- tolower(sdf2$ed_firm_par_in)
sdf2$ed_firm_par_out <- tolower(sdf2$ed_firm_par_out)

sdf2$ed_city <- tolower(sdf2$ed_city)

#remove multiple whitespaces and ones from start and ending of the string
library(stringr)
sdf2$ed_firm <- gsub("\\s+", " ", str_trim(sdf2$ed_firm))
sdf2$ed_firm_par_in <- gsub("\\s+", " ", str_trim(sdf2$ed_firm_par_in))
sdf2$ed_firm_par_out <- gsub("\\s+", " ", str_trim(sdf2$ed_firm_par_out))

sdf2$ed_city <- gsub("\\s+", " ", str_trim(sdf2$ed_city))

#standarize  "№..."
sdf2$ed_firm <- gsub("№ ", "№", sdf2$ed_firm)
sdf2$ed_firm_par_in <- gsub("№ ", "№", sdf2$ed_firm_par_in)
sdf2$ed_firm_par_out <- gsub("№ ", "№", sdf2$ed_firm_par_out)

sdf2$ed_city <- gsub("№ ", "№", sdf2$ed_city)

##add column with transliteration from latin to cyrrilic
#sdf2$ed_firm_transl <- stri_trans_general(sdf2$ed_firm, "cyrillic")
#sdf2$ed_firm_par_in_transl <-  stri_trans_general(sdf2$ed_firm_par_in, "cyrillic")
#sdf2$ed_firm_par_out_transl <-  stri_trans_general(sdf2$ed_firm_par_out, "cyrillic")

#add column with transliteration from latin to cyrrilic
sdf2$ed_firm <- stri_trans_general(sdf2$ed_firm, "cyrillic")
sdf2$ed_firm_par_in <-  stri_trans_general(sdf2$ed_firm_par_in, "cyrillic")
sdf2$ed_firm_par_out <-  stri_trans_general(sdf2$ed_firm_par_out, "cyrillic")

#remove all rows with blank or zero ("0") "firm" cell (it means that there are no companies corresponding to school)
sdf2 <- sdf2[!(stri_isempty(sdf2$ed_firm)) & sdf2$ed_firm!="0" ,]

#----------------------------------------------------------------------------------------

#create vector of region id (excluding cell with NA)
region <- unique(sdf2$reg_idnew)
region <- region[!is.na(region)]


#create table for vlook up in the end
lookup_school<-sdf[, c("row_id", "inn", "name", "address", "city")]


#create table for the final report
report <- data.frame(region = integer(),
                     phase = integer(),
                     unique_rows = integer(),
                     matches_amnt = integer(),
                     matches_percent = character(),
                     unique_firms = integer(),
                     f_matches_amnt = integer(),
                     f_matches_percent = character(),
                     ruslana_rows = integer(),
                     final_rows = integer(),
                     stringsAsFactors = FALSE)

#----------------------------------------------------------------------------------------
#library(scales)

ptm <- proc.time()

#create tables - each for each reg_id
for(i in region) {
  

  sum <- NULL
  
  #make separate table for the unique region
  df<-sdf2[sdf2$reg_idnew==i,] 
  
  #compute number of unique rows found in the previous iteration
  l1<-length(unique(df$row_id))
  f1<-length(unique(df$firm))
  

  #compute number of rows in this table
  assign(paste("num",i,sep = ""),length(unique(df$row_id)))
  
  #make separate table for the unique region from Ruslana DB
  Ruslana <- read.csv2(paste("D:\\Esaulov\\Ruslana_by_regions_new\\Ruslana_", i, ".csv", sep = ""), header = T, stringsAsFactors = FALSE, sep =";", encoding = "UTF-8")
 
  #add column names
  colnames(Ruslana) <- c("Mark", "firm_long", "firm_short", "BvD", "OKPO", "INN", "region", "city_rusl", "status", "status_change_year", "found_year","form", "region_id")
  
  #changing format of "year" columns to numeric
  Ruslana[c("status_change_year", "found_year")] <- sapply(Ruslana[c("status_change_year", "found_year")],as.numeric)
  
  #shrinking Ruslana db: deleting all the companies that were founded after year 2013
  Ruslana <- Ruslana[which(Ruslana$found_year <=2013), ]
  #shrinking Ruslana db: deleting all the companies that became inactive before 2013
  Ruslana[is.na(Ruslana$status_change_year),"status_change_year"] <- 2017
  Ruslana <- Ruslana[(Ruslana$status == "Действующее")|(Ruslana$status_change_year >=2013), ]
  
  r<-length(Ruslana$Mark)
  
  
  #add phase 0 to report table 
  report[nrow(report)+1,] <- c(i, "0", l1, "0", "0", f1, "0", "0", r, length(sum$row_id))
  
  
  #add columns for editing and remove all punctuation from the copy of Ruslana
  Ruslana <- mutate(Ruslana, ed_firm_long = firm_long, ed_firm_short = firm_short, ed_city_rusl = city_rusl)
  
  #if the ed_city_rusl name consists of 2 parts (separated with comma), extract just the first one
  Ruslana$ed_city_rusl <- sub("(.*?)(\\,.*$)", "\\1", Ruslana$ed_city_rusl, ignore.case = TRUE)
  
  Ruslana$ed_firm_short <- gsub("-|–|—"," ", enc2utf8(Ruslana$ed_firm_short))
  Ruslana$ed_firm_long <- gsub("-|–|—"," ", enc2utf8(Ruslana$ed_firm_long))
  Ruslana$ed_city_rusl <- gsub("-|–|—"," ", enc2utf8(Ruslana$ed_city_rusl))
  
  Ruslana$ed_firm_short <- gsub("[[:punct:]]"," ", enc2utf8(Ruslana$ed_firm_short))
  Ruslana$ed_firm_long <- gsub("[[:punct:]]"," ", enc2utf8(Ruslana$ed_firm_long))
  Ruslana$ed_city_rusl <- gsub("[[:punct:]]"," ", enc2utf8(Ruslana$ed_city_rusl))
  
  
  #convert to lower case
  Ruslana$ed_firm_short <- tolower(Ruslana$ed_firm_short)
  Ruslana$ed_firm_long <- tolower(Ruslana$ed_firm_long)
  Ruslana$ed_city_rusl <- tolower(Ruslana$ed_city_rusl)
  
  #remove multiple whitespaces and ones from start and ending of the string
  Ruslana$ed_firm_short <- gsub("\\s+", " ",str_trim(Ruslana$ed_firm_short))
  Ruslana$ed_firm_long <- gsub("\\s+", " ",str_trim(Ruslana$ed_firm_long))
  Ruslana$ed_city_rusl <- gsub("\\s+", " ",str_trim(Ruslana$ed_city_rusl))
  
  #edit "№..."
  Ruslana$ed_firm_short <- gsub("№ ", "№", Ruslana$ed_firm_short)
  Ruslana$ed_firm_long <- gsub("№ ", "№", Ruslana$ed_firm_long)
  Ruslana$ed_city_rusl <- gsub("№ ", "№", Ruslana$ed_city_rusl)
  
  #extract just the name of the city_rusl/selo/poselok
  
  Ruslana$ed_city_rusl <- sub("(рабочий заводской поселок )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(рабочий поселок )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(п г т )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(пгт )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(пос )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(дер )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(гор )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(п о )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(р п )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(рп )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(с п )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(сп )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(ст ца )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(ст )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(г )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(п )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(с )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(д )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(х )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  Ruslana$ed_city_rusl <- sub("(ж )(.*$)", "\\2", Ruslana$ed_city_rusl, ignore.case = TRUE)
  
  #add id to Ruslana table
  row_id_rusl<- as.numeric(rownames(Ruslana))
  Ruslana$Mark<-row_id_rusl
  
  
  #--------------------------------------------------------------
  
  #compute number of spaces in a string so that we know number of words
  df$num_space <- unlist(lapply(df$ed_firm, function(a){length(gregexpr(" ", a)[[1]])}))
  

  #Phase 1
  
  #Step 1: finding perfect match with long name of the company
  #creating table of matches
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed_firm_long")
  #row binding the result with previous one
  sum <- rbind.fill(sum, cong)
  
  #Step 2: finding perfect match with short name of the company
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  #deleting matches from the initial table
  
  #delete duplicated rows
  sum <- sum[!duplicated(sum[,c('row_id', 'BvD')]),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "1", l1, length(unique(sum$row_id)), 
                  round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                  f1, length(unique(sum$firm)), 
                  round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                  r, length(sum$row_id))
  
  
  #Phase 2
  
  #Step 3: adding "ооо" to the long name of the company and find match
  Ruslana <- mutate(Ruslana, ed2_firm_long = paste("ооо", ed_firm_long))
  #Ruslana$ed_firm_long <- paste("ооо", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #Step 4: adding "оао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("оао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #Step 5: adding "зао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("зао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #Step 6: adding "ао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 7: adding "муп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("муп", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 8: adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #Step 9: editing rows with "ип" - if #of words in the string with "ип" is more than 4 make them 4 (to leave only "ип иванов владимир викторович" instead of "ип иванов владимир викторович магазин ромашка")
  df$ed2_firm <- df$ed_firm
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 10: deleting initials and adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", word(Ruslana$ed_firm_long, 1))
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 11: getting initials from the full name and adding "ип" to the long name of the company
  Ruslana$num_spaces <- lapply(Ruslana$ed_firm_long, function(a){length(gregexpr(" ", a)[[1]])})
  Ruslana$ed2_firm_long <- Ruslana$ed_firm_long
  Ruslana[Ruslana$num_spaces==2,]$ed2_firm_long <- unlist(lapply(Ruslana[Ruslana$num_spaces==2,]$ed_firm_long, function(a){
    paste(word(a, 1), substring(word(a, 2), 1, 1), substring(word(a, 3), 1, 1))
  })
  )
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed2_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 12: the same as in step 9, but works for initials (example "ип иванов в в" instead of "ип иванов в в магазин ромашка")
  df$ed2_firm <- df$ed_firm
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 13: adding "фгуп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("фгуп", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #-------------------------------------------------------
  #editing long name again
  
  #step 24:
  Ruslana$ed2_firm_long <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 25:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 26:
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 27:
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 28:
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 29:
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 30:
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 31:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #delete duplicated rows
  sum <- sum[!duplicated(sum[,c('row_id', 'BvD')]),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "2", l1, length(unique(sum$row_id)), 
                               round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(sum$firm)), 
                               round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                               r, length(sum$row_id))
  
  
  
  #Phase 3
  
  #Same for the short name of the company
  
  #step 14: adding "ооо" to the short name of the company
  Ruslana <- mutate(Ruslana, ed2_firm_short = paste("ооо", ed_firm_short))
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  
  #step 15: adding "оао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("оао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 16: adding "зао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("зао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17: adding "ао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 18: adding "муп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("муп", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 19: adding "фгуп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("фгуп", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 20: change "ао" to "оао"
  Ruslana$ed2_firm_short <- gsub("ао", "оао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 21: change "oао" to "ао"
  Ruslana$ed2_firm_short <- gsub("оао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 22: change "ао" to "зао"
  Ruslana$ed2_firm_short <- gsub("ао", "зао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 23: change "зао" to "ао"
  Ruslana$ed2_firm_short <- gsub("зао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  

  #---------------- the same with short name (didn't work on Sverdlovsk)
  
  #step 32:
  Ruslana$ed2_firm_short <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 33:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 34:
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 35:
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 36:
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 37:
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 38:
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 39:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df, Ruslana, by.x="ed_firm", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #delete duplicated rows
  sum <- sum[!duplicated(sum[,c('row_id', 'BvD')]),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "3", l1, length(unique(sum$row_id)), 
                               round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(sum$firm)), 
                               round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                               r, length(sum$row_id))
  
  
  #Phase 4
  
  #---------------------working with ICSID db
  
  #step 40: adding "ооо" etc to "firm" in small table
  df <- mutate(df, ed2_firm = paste("ооо", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  
  df <- mutate(df, ed2_firm = paste("оао", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("зао", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("ао", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("открытое акционерное общество", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("акционерное общество", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("общество с ограниченной ответственностью", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("закрытое акционерное общество", ed_firm))
  cong <- merge(df, Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #delete duplicated rows
  sum <- sum[!duplicated(sum[,c('row_id', 'BvD')]),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "4", l1, length(unique(sum$row_id)), 
                               round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(sum$firm)), 
                               round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                               r, length(sum$row_id))
  
  

  #Phase 5
  
  #--------------------------
  #Step 41: matching names inside and outside of parentheses
  #finding perfect match with long name of the company
  
  cong <- merge(df[df$is_parenthes,], Ruslana, by.x="ed_firm_par_in", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  cong <- merge(df[df$is_parenthes,], Ruslana, by.x="ed_firm_par_out", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #finding perfect match with short name of the company
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  
  #adding "ооо" to the long name of the company
  Ruslana <- mutate(Ruslana, ed2_firm_long = paste("ооо", ed_firm_long))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  Ruslana <- mutate(Ruslana, ed2_firm_long = paste("ооо", ed_firm_long))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #adding "оао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  Ruslana$ed2_firm_long <- paste("оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "зао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  Ruslana$ed2_firm_long <- paste("зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "ао" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  Ruslana$ed2_firm_long <- paste("ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "муп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("муп", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  df$ed2_firm <- df$ed_firm_par_in
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #deleting initials and adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", word(Ruslana$ed_firm_long, 1))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #getting initials from the full name and adding "ип" to the long name of the company
  Ruslana$num_spaces <- lapply(Ruslana$ed_firm_long, function(a){length(gregexpr(" ", a)[[1]])})
  Ruslana$ed2_firm_long <- Ruslana$ed_firm_long
  Ruslana[Ruslana$num_spaces==2,]$ed2_firm_long <- unlist(lapply(Ruslana[Ruslana$num_spaces==2,]$ed_firm_long, function(a){
    paste(word(a, 1), substring(word(a, 2), 1, 1), substring(word(a, 3), 1, 1))
  })
  )
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed2_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #editing ип 2
  df$ed2_firm <- df$ed_firm_par_in
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "фгуп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("фгуп", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #adding "ooo"
  Ruslana <- mutate(Ruslana, ed2_firm_short = paste("ооо", ed_firm_short))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 9: adding "оао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 10: adding "зао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 11: adding "ао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 12: adding "муп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("муп", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 12: adding "фгуп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("фгуп", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step13:
  Ruslana$ed2_firm_short <- gsub("ао", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  Ruslana$ed2_firm_short <- gsub("оао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step14:
  Ruslana$ed2_firm_short <- gsub("ао", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step14:
  Ruslana$ed2_firm_short <- gsub("зао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 15:
  Ruslana$ed2_firm_long <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 18
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #---------------- the same with short name (didn't wotk on Sverdlovsk)
  
  #step 15:
  Ruslana$ed2_firm_short <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 18
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_in", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #---------------------
  #adding "ооо" etc to "firm" in small table
  df <- mutate(df, ed2_firm = paste("ооо", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("оао", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("зао", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("ао", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("открытое акционерное общество", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("акционерное общество", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("общество с ограниченной ответственностью", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("закрытое акционерное общество", ed_firm_par_in))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #------------------------the same with par_out
  #step 7: adding "муп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("муп", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 7: adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #editing ип 1
  df$ed2_firm <- df$ed_firm_par_out
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #step 7: deleting initials and adding "ип" to the long name of the company
  Ruslana$ed2_firm_long <- paste("ип", word(Ruslana$ed_firm_long, 1))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 7: getting initials from the full name and adding "ип" to the long name of the company
  Ruslana$num_spaces <- lapply(Ruslana$ed_firm_long, function(a){length(gregexpr(" ", a)[[1]])})
  Ruslana$ed2_firm_long <- Ruslana$ed_firm_long
  Ruslana[Ruslana$num_spaces==2,]$ed2_firm_long <- unlist(lapply(Ruslana[Ruslana$num_spaces==2,]$ed_firm_long, function(a){
    paste(word(a, 1), substring(word(a, 2), 1, 1), substring(word(a, 3), 1, 1))
  })
  )
  Ruslana$ed2_firm_long <- paste("ип", Ruslana$ed2_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #editing ип 2
  df$ed2_firm <- df$ed_firm_par_out
  df[df$num_space > 3,]$ed2_firm <- lapply(df[df$num_space > 3,]$ed_firm, function(a){
    word(a, 1,4)
  }
  )
  df$ed2_firm <- unlist(df$ed2_firm)
  
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #step 7: adding "фгуп" to the long name of the company
  Ruslana$ed2_firm_long <- paste("фгуп", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 8:
  Ruslana <- mutate(Ruslana, ed2_firm_short = paste("ооо", ed_firm_short))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 9: adding "оао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 10: adding "зао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 11: adding "ао" to the short name of the company
  Ruslana$ed2_firm_short <- paste("ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 12: adding "муп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("муп", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 12: adding "фгуп" to the short name of the company
  Ruslana$ed2_firm_short <- paste("фгуп", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step13:
  Ruslana$ed2_firm_short <- gsub("ао", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  Ruslana$ed2_firm_short <- gsub("оао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step14:
  Ruslana$ed2_firm_short <- gsub("ао", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step14:
  Ruslana$ed2_firm_short <- gsub("зао", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 15:
  Ruslana$ed2_firm_long <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("акционерное общество", "зао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 18
  Ruslana$ed2_firm_long <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_long <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_long)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_long")
  sum <- rbind.fill(sum, cong)
  
  
  #---------------- the same with short name (didn't wotk on Sverdlovsk)
  
  #step 15:
  Ruslana$ed2_firm_short <- gsub("общество с ограниченной ответственностью", "ооо", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("акционерное общество", "зао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 17
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "оао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 18
  Ruslana$ed2_firm_short <- gsub("открытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #step 16:
  Ruslana$ed2_firm_short <- gsub("закрытое акционерное общество", "ао", Ruslana$ed_firm_short)
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed_firm_par_out", by.y="ed2_firm_short")
  sum <- rbind.fill(sum, cong)
  
  #---------------------
  #adding "ооо" etc to "firm" in small table
  df <- mutate(df, ed2_firm = paste("ооо", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("оао", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("зао", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("ао", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_short")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("открытое акционерное общество", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("акционерное общество", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("общество с ограниченной ответственностью", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  df <- mutate(df, ed2_firm = paste("закрытое акционерное общество", ed_firm_par_out))
  cong <- merge(df[df$is_parenthes ,], Ruslana, by.x="ed2_firm", by.y="ed_firm_long")
  sum <- rbind.fill(sum, cong)
  
  #delete duplicated rows
  sum <- sum[!duplicated(sum[,c('row_id', 'BvD')]),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "5", l1, length(unique(sum$row_id)), 
                               round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(sum$firm)), 
                               round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                               r, length(sum$row_id))
  
  
  #Phase 6
  
  #leave only rows where cities of school and firms match. If there are no firms that match to school by city then leave all the firms found
  
  #1) check if city matches => write result to new column
  sum$same_city <- (sum$ed_city == sum$ed_city_rusl) 
  
  #2)calculate number of matching cities for each row_id
  sum <- ddply(sum, .(row_id), mutate, count = sum(same_city))
  
  #3) if this number is 0 => leave all corresponding rows. If >0 => leave only ones with matching cities
  sum <- sum[(sum$count == 0) | (sum$count >0 & sum$same_city == TRUE),]
  
  
  
  
  #account for the business form. Delete matches if business forms don't match for sure. Leave matches if there is no correspondence in form (the same logics as in correspondence of cities)
  sum$form <- tolower(sum$form)
  sum$form <- gsub("[[:punct:]]"," ", enc2utf8(sum$form))
  sum$form <- gsub("\\s+", " ", str_trim(sum$form))
  sum$form <- gsub("общество с ограниченной ответственностью", "ооо", sum$form)
  sum$form <- gsub("муниципальное унитарное предприятие", "муп", sum$form)
  sum$form <- gsub("индивидуальный предприниматель", "ип", sum$form)
  sum$form <- gsub("товарищество собственников жилья", "тсж", sum$form)
  sum$form <- gsub("потребительское общество", "по", sum$form)
  sum$form <- gsub("сельскохозяйственный производственный кооператив", "спк|схпк", sum$form)
  sum$form <- gsub("потребительское общество", "по", sum$form)
  sum$form <- gsub("непубличное акционерное общество", "нао", sum$form)
  sum$form <- gsub("публичное акционерное общество", "пао", sum$form)
  sum$form <- gsub("муниципальное предприятие", "мп", sum$form)
  sum$form <- gsub("федеральное государственное бюджетное учреждение", "фгбу", sum$form)
  sum$form <- gsub("муниципальное казенное учреждение", "мку", sum$form)
  sum$form <- gsub("товарищество с ограниченной ответственностью", "тоо", sum$form)
  sum$form <- gsub("государственное бюджетное учреждение субъекта российской федерации", "гбу", sum$form)
  sum$form <- gsub("муниципальное бюджетное учреждение", "мбу", sum$form)
  sum$form <- gsub("федеральное государственное унитарное предприятие", "фгуп", sum$form)
  sum$form <- gsub("индивидуальное частное предприятие", "ичп", sum$form)
  sum$form <- gsub("закрытое акционерное общество", "зао", sum$form)
  sum$form <- gsub("открытое акционерное общество", "оао", sum$form)
  sum$form <- gsub("автономная некоммерческая организация", "ано", sum$form)
  sum$form <- gsub("бюджетное учреждение", "бу", sum$form)
  sum$form <- gsub("государственное автономное учреждение субъекта российской федерации", "гау", sum$form)
  sum$form <- gsub("государственное казенное учреждение субъекта российской федерации", "гку", sum$form)
  sum$form <- gsub("государственное предприятие", "гп", sum$form)
  sum$form <- gsub("государственное унитарное препдриятие субъектов российской федерации", "гуп", sum$form)
  sum$form <- gsub("муниципальное автономное учреждение", "мау", sum$form)
  sum$form <- gsub("муниципальное казенное предприятие", "мкп", sum$form)
  sum$form <- gsub("муниципальное казенное учреждение", "мку", sum$form)
  sum$form <- gsub("профсоюзная организация", "по", sum$form)
  sum$form <- gsub("фермерское хозяйство", "кфх|кх|фх", sum$form)
  sum$form <- gsub("крестьянское фермерское хозяйство юл", "кфх|кх|фх", sum$form)
  sum$form <- gsub("глава крестьянского фермерского хозяйства", "кфх|кх|фх", sum$form)
  
  #is the form of firm in ISCID and Ruslana dbs the same?
  sum$same_form <- stri_detect(sum$ed_firm, regex = sum$form)
  
  sum <- ddply(sum, .(row_id), mutate, count_form = sum(same_form))
  
  sum <- sum[(sum$count_form == 0) | (sum$count_form >0 & sum$same_form == TRUE),]
  
  #add row to the report
  report[nrow(report)+1,] <- c(i, "6", l1, length(unique(sum$row_id)), 
                               round((length(unique(sum$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(sum$firm)), 
                               round((length(unique(sum$firm))/f1)*100, digits = 2) , 
                               r, length(sum$row_id))
  
  #make table of firms with no matches
  matches <- unique(sum$row_id)
  no_matches <- subset(df, !(df$row_id %in% matches) & !is.na(df$row_id))
  

  # #compute number of unique matched rows (from initial table)
  # assign(paste("num_match",i,sep = ""),length(matches))
  
  #vlook up the found info with the initial table
  sum_final <- join(sum, lookup_school, by = "row_id")
  sum_final <- sum_final[, c("row_id", "reg_idnew", "inn", "name", "address", "city", "firm", "firm_long", "firm_short", "BvD", "OKPO", "INN", "region", "city_rusl")]
  
  #make the file
  assign(paste("sum_final", i, sep=""), sum_final)
  saveRDS(sum_final, paste("D:/Esaulov/Direct_Summary/sum_final", i, ".rds", sep=""))
  
  #write the summary to the file
  write.csv2(sum_final, paste("D:/Esaulov/Direct_Summary/sum_final", i, ".csv", sep=""), fileEncoding = "UTF-8")
  
  #calculate general amount of matched unique rows/unqie firms from initial table
  #q <- q + length(matches)
  #f <- f + length(unique(sum$firm))
  
  rm(sum)
  #rm(sum_final)
  gc()
  
#--------------------------------------
#Phase 7
  
  #Fuzzy matching
  
  #Fuzzy matching with the long firm name
  dist_firm<-adist(no_matches$ed_firm,Ruslana$ed_firm_long, 
                   partial = TRUE, ignore.case = TRUE)
  
  min_firm<-apply(dist_firm, 1, min)
  
  fuzzy_matches_long<-NULL  
  for(j in 1:nrow(no_matches))
  {
    pos_ruslana_j<-match(min_firm[j],dist_firm[j,])
    pos_no_match<-j
    fuzzy_matches_long<-rbind(data.frame(fuzzy_firm_rusl=Ruslana[pos_ruslana_j,]$firm_long, 
                                         fuzzy_firm=no_matches[pos_no_match,]$firm, 
                                         adist=min_firm[j], 
                                         row_id=no_matches[pos_no_match,]$row_id, 
                                         Mark=Ruslana[pos_ruslana_j,]$Mark),fuzzy_matches_long)
  }
  
  rm(dist_firm)
  gc()
  
  fuzzy_matches_long <- join(fuzzy_matches_long, Ruslana, by = "Mark")
  fuzzy_matches_long <- join(fuzzy_matches_long, lookup_school, by = "row_id")
  
  fuzzy_matches_long <- fuzzy_matches_long[,c(4:5, 24, 27, 2, 1, 8:12)]
  
  assign(paste("fuzzy_matches_long_", i, sep=""), fuzzy_matches_long)
  
  #fuzzy_matches_long <- join(fuzzy_matches, Ruslana, by = "Mark")
  #fuzzy_matches_long <- join(fuzzy_matches, df, by = "row_id")
  
  #the same for the short firm name
  fuzzy_matches_short<-NULL  
  dist_firm<-adist(no_matches$ed_firm,Ruslana$ed_firm_short, 
                   partial = TRUE, ignore.case = TRUE)
  
  min_firm<-apply(dist_firm, 1, min)
  
  for(j in 1:nrow(no_matches))
  {
    pos_ruslana_j<-match(min_firm[j],dist_firm[j,])
    pos_no_match<-j
    fuzzy_matches_short<-rbind(data.frame(fuzzy_firm_rusl=Ruslana[pos_ruslana_j,]$firm_short, 
                                          fuzzy_firm=no_matches[pos_no_match,]$firm, 
                                          adist=min_firm[j], 
                                          row_id=no_matches[pos_no_match,]$row_id, 
                                          Mark=Ruslana[pos_ruslana_j,]$Mark),fuzzy_matches_short)
  }
  
  rm(dist_firm)
  gc()
  
  
  #  #delete duplicated rows (such that Mark from Ruslana and row_id from df are the same)
  #  fuzzy_matches <- fuzzy_matches[!duplicated(fuzzy_matches[,c('row_id', 'Mark')]),]
  
  #  #leave rows with min(dist) (considering results from fuzzy_matching with short and long name)
  #  fuzzy_matches %>%                  
  #    group_by(row_id) %>%         
  #    filter(adist == min(adist))
  
  
  fuzzy_matches_short <- join(fuzzy_matches_short, Ruslana, by = "Mark")
  fuzzy_matches_short <- join(fuzzy_matches_short, lookup_school, by = "row_id")
  
  fuzzy_matches_short <- fuzzy_matches_short[,c(4:5, 24, 27, 2, 1, 8:12)]
  
  assign(paste("fuzzy_matches_short_", i, sep=""), fuzzy_matches_short)
  
  #  assign(paste("fuzzy_matches_short_", i, sep=""), fuzzy_matches)
  write.csv2(fuzzy_matches_long, 
             paste("D:/Esaulov/Fuzzy_Summary/fuzzy_matches_long_", i, ".csv", sep=""), 
             fileEncoding = "UTF-8")
  
  write.csv2(fuzzy_matches_short, 
             paste("D:/Esaulov/Fuzzy_Summary/fuzzy_matches_short_", i, ".csv", sep=""), 
             fileEncoding = "UTF-8")
  
  #  sum<- rbind.fill(sum, fuzzy_matches)
  
  #  rm(no_matches)
  #  rm(fuzzy_matches)
  #rm(Ruslana)
  #rm(df)
  rm(fuzzy_matches_long)
  rm(fuzzy_matches_short)
  #rm(paste("fuzzy_matches_", i, sep=""))
  #rm(paste("sum_final", i, sep=""))
  
  #remind R to free CPU
  gc()
  
#----------------------------------------------------------------------------------- 
# report matching correctness after fuzzy matching procedure
# Use the following script for manually coded files 
# (with new column "Match_true" = 0 if fuzzy match is not correct; 1 if it is correct; 2 if it's not clear)

  
  # load the coded data (each region has 2 files: results for fuzzy matching with long name and short name)
  fuzzy_long <-  read.csv2(paste("D:/Esaulov/Fuzzy_manual/Done/fuzzy_matches_long_", i, ".csv", sep=""), 
                           header = TRUE, sep = ";", stringsAsFactors =  FALSE)
  
  fuzzy_long <- fuzzy_long[,-1]
  
  colnames(fuzzy_long) <- c("row_id", "Mark", 
                            "inn", "city", 
                            "firm", "firm_long", "BvD", "OKPO", "INN", "region", "city_rusl", "Match_true") 
  
  
  fuzzy_short <- read.csv2(paste("D:/Esaulov/Fuzzy_manual/Done/fuzzy_matches_short_", i, ".csv", sep=""), 
                           header = TRUE, sep = ";", stringsAsFactors =  FALSE)
  
  fuzzy_short<-fuzzy_short[,-1]
  
  colnames(fuzzy_short) <- c("row_id", "Mark", 
                             "inn", "city", 
                             "firm", "firm_short", "BvD", "OKPO", "INN", "region", "city_rusl", "Match_true") 
  
  # create data file with correct matches from "long" and "short" versions and delete duplicates
  fuzzy <- fuzzy_long[fuzzy_long$Match_true ==1,]
  fuzzy <- rbind.fill(fuzzy, fuzzy_short[fuzzy_short$Match_true==1,])
  
  fuzzy <- fuzzy[!duplicated(fuzzy[,'row_id']),]
  
  fuzzy <- fuzzy[,-2]
  fuzzy$reg_idnew <- i
  
  # combine correct matches with the summary file from direct matching
  match_final <- join(fuzzy, lookup_school, by = "row_id")
  match_final <- rbind.fill(sum_final, match_final)
  match_final <- match_final[, c("row_id", "reg_idnew", "inn", "name", "address", "city", "firm", "firm_long", "firm_short", "BvD", "OKPO", "INN", "region", "city_rusl")]
  
  
  report[nrow(report)+1,] <- c(i, "7", l1, length(unique(match_final$row_id)), 
                               round((length(unique(match_final$row_id))/l1)*100, digits = 2) , 
                               f1, length(unique(match_final$firm)), 
                               round((length(unique(match_final$firm))/f1)*100, digits = 2) , 
                               r, length(match_final$row_id))
  
  
  #make table with firms that still have no matches
  id<-fuzzy$row_id
  no_matches<-fuzzy_long[!(fuzzy_long$row_id %in% id), -(6:12)]
  
#------------------------------------------------------------------------------------
#Phase 8 (optional).
#Finding the second best result from fuzzy matching
#It works well for rows with Match_true == "2"

# For "almost correct" (Match_true == 2) long names 
 
  fuzzy_long2<-fuzzy_long[fuzzy_long$Match_true==2 & !(fuzzy_short$Match_true==1), -(6:12)]
  fuzzy_long2<-join(fuzzy_long2, sdf2, by="row_id")
  
  dist_firm<-adist(fuzzy_long2$ed_firm,Ruslana$ed_firm_long, partial = TRUE, ignore.case = TRUE)
 
  #finding second minimum for the values of adist
  min_firm<-apply(dist_firm, 1, function(x){sort(x,partial=2)[2]})
  fuzzy_matches_long2<-NULL  
  for(j in 1:nrow(fuzzy_long2))
  {
    pos_ruslana_j<-match(min_firm[j],dist_firm[j,])
    pos_no_match<-j
    fuzzy_matches_long2<-rbind(data.frame(fuzzy_firm_rusl=Ruslana[pos_ruslana_j,]$firm_long, 
                                         fuzzy_firm=fuzzy_long2[pos_no_match,]$firm, 
                                         adist=min_firm[j], 
                                         row_id=fuzzy_long2[pos_no_match,]$row_id, 
                                         Mark=Ruslana[pos_ruslana_j,]$Mark),fuzzy_matches_long2)
  }
  
  rm(dist_firm)
  gc()
  
  fuzzy_matches_long2 <- join(fuzzy_matches_long2, Ruslana, by = "Mark")
  fuzzy_matches_long2 <- join(fuzzy_matches_long2, lookup_school, by = "row_id")
  
  fuzzy_matches_long2 <- fuzzy_matches_long2[,c(4:5, 24, 27, 2, 1, 8:12)]
  
# The same for short name
  
  fuzzy_short2<-fuzzy_long[fuzzy_short$Match_true==2 & !(fuzzy_long$Match_true==1), -(6:12)]
  fuzzy_short2<-join(fuzzy_short2, sdf2, by="row_id")
  
  dist_firm<-adist(fuzzy_short2$ed_firm,Ruslana$ed_firm_short, partial = TRUE, ignore.case = TRUE)
  min_firm<-apply(dist_firm, 1, function(x){sort(x,partial=2)[2]})
  fuzzy_matches_short2<-NULL  
  for(j in 1:nrow(fuzzy_short2))
  {
    pos_ruslana_j<-match(min_firm[j],dist_firm[j,])
    pos_no_match<-j
    fuzzy_matches_short2<-rbind(data.frame(fuzzy_firm_rusl=Ruslana[pos_ruslana_j,]$firm_short, 
                                          fuzzy_firm=fuzzy_short2[pos_no_match,]$firm, 
                                          adist=min_firm[j], 
                                          row_id=fuzzy_short2[pos_no_match,]$row_id, 
                                          Mark=Ruslana[pos_ruslana_j,]$Mark),fuzzy_matches_short2)
  }
  
  rm(dist_firm)
  gc()
  
  fuzzy_matches_short2 <- join(fuzzy_matches_short2, Ruslana, by = "Mark")
  fuzzy_matches_short2 <- join(fuzzy_matches_short2, lookup_school, by = "row_id")
  
  fuzzy_matches_short2 <- fuzzy_matches_short2[,c(4:5, 24, 27, 2, 1, 8:12)]
   
#If needed the same can be done for the rows with "Match_true == 0". But new results aren't good
    
}

#no_matches <- subset(df, !(df$row_id %in% matches) & !is.na(df$row_id))


proc.time() - ptm


# fuzzy_long0<-fuzzy_long[fuzzy_long$Match_true==0 & (fuzzy_short$Match_true==0), -(6:12)]
# fuzzy_long0<-join(fuzzy_long0, sdf2, by="row_id")
# 
# dist_firm<-adist(fuzzy_long0$ed_firm,Ruslana$ed_firm_long, partial = TRUE, ignore.case = TRUE)
# min_firm<-apply(dist_firm, 1, function(x){sort(x,partial=2)[2]})
# fuzzy_matches_long0<-NULL
# for(j in 1:nrow(fuzzy_long0))
# {
#   pos_ruslana_j<-match(min_firm[j],dist_firm[j,])
#   pos_no_match<-j
#   fuzzy_matches_long0<-rbind(data.frame(fuzzy_firm_rusl=Ruslana[pos_ruslana_j,]$firm_long,
#                                        fuzzy_firm=fuzzy_long0[pos_no_match,]$firm,
#                                        adist=min_firm[j],
#                                        row_id=fuzzy_long0[pos_no_match,]$row_id,
#                                        Mark=Ruslana[pos_ruslana_j,]$Mark),fuzzy_matches_long0)
# }
# 
# rm(dist_firm)
# gc()
# 
# fuzzy_matches_long0 <- join(fuzzy_matches_long0, Ruslana, by = "Mark")
# fuzzy_matches_long0 <- join(fuzzy_matches_long0, lookup_school, by = "row_id")
# 
# fuzzy_matches_long0 <- fuzzy_matches_long0[,c(4:5, 24, 27, 2, 1, 8:12)]
