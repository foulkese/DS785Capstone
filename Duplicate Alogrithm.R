#install.packages("RecordLinkage")
#install.packages("dplyr")
#install.packages("stringr")
library(stringr)   
library(RecordLinkage)
library(dplyr)

# read in dataframe
df <- read.csv('C:/Users/other/Documents/Capstone/Data/all_cns_7_4_23.csv')

# drop unnecessary columns and format all data to lowercase characters
df <- df[,!(names(df) %in% c('X_score','address.address2', 'address.countryCode', 'address.countryID', 'address.timezone'))]
df <- data.frame(lapply(df, as.character))

df <- data.frame(lapply(df, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

# remove all records with unknown in first name
df <-  df[- grep("unknown", df$firstName),]

# replace all null and empty values - NA doesn't get counted in similarity scoring but empty strings do
df[df == 'NULL'] <- NA
df[df == ''] <- NA

# create a function to test for minimum similarity threshold
sub_sim <- function(x, similarity, threshold) {
  sim_comp <- similarity[x]
  sim_comp <- sim_comp[!is.na(sim_comp)]
  len <- length(sim_comp[sim_comp > .8])
  return (len >= threshold )
}

## Combinations Tested ##

# (firstname, lastname, companyName); (firstname, companyname, state); 
# (lastname, companyname, state); (firstname, lastname, occupation, state); 
# (firstname, lastname, city, state); (firstname, lastname, state, zip)

# create a function to test against each combination and return the id values if above threshold
comp <- function(x, y, df){
  
  sim <- levenshteinSim(df[x,], df[y,])
  
    if(sub_sim(c(1,2,4,5), sim, 2)){
      if(sub_sim(c(1,2,5), sim, 3)){
        return(c(df[x,3], df[y,3])) 
      }else{
        if(sub_sim(c(1,5,9), sim, 3)){
          return(c(df[x,3], df[y,3])) 
        }else{
          if(sub_sim(c(2,5,9), sim, 3)){
            return(c(df[x,3], df[y,3])) 
          }else{
            if(sub_sim(c(1,2,4,9), sim, 4)){
              return(c(df[x,3], df[y,3])) 
            }else{
              if(sub_sim(c(1,2,7,9), sim, 4)){
                return(c(df[x,3], df[y,3])) 
              }else{
                if(sub_sim(c(1,2,9,10), sim, 4)){
                  return(c(df[x,3], df[y,3])) 
                }
              }
            }
          }
        }
      }
    }
}

# loop through combinations first 2 initials in first name and first initial in last name and apply similarity function to each row-pair
results <- matrix(nrow = 0, ncol = 2)
for (fi in letters[1:26]) {
  first_initial <- subset(df, str_sub(df$firstName, 1, 1)  %in% c(fi) )
  for (si in letters[1:26]) {
    second_initial <- subset(first_initial, str_sub(first_initial$firstName, 2, 2)  %in% c(si) )
    for (li in letters[1:26]) {
      name_combo <- subset(second_initial, str_sub(second_initial$lastName, 1, 1)  %in% c(li) )
      if(nrow(name_combo)>1){
        for (i in 1:(nrow(name_combo)-1)) {
          for (j in (i+1):nrow(name_combo)) {
            value <- comp(i,j,name_combo)
            if(!is.null(value) & !setequal(value[1], value[2])){
              results <- rbind(results, value)
            }
          }
        }
      }
    }
  }
}

# save the results as a dataframe and export them to a csv
all_duplicates <- data.frame(results)
write.csv(all_duplicates, "C:/Users/other/Documents/Capstone/Data/all_duplicates.csv", row.names=FALSE)

# check for any values in the second column greater than the first, if so swap them
clean_dups <- transform(all_duplicates, X1 = ifelse(
  all_duplicates$X1 > all_duplicates$X2,
  X2,
  X1
),
X2 = ifelse(
  all_duplicates$X1 > all_duplicates$X2,
  X1,
  X2
)
)

# remove all duplicated rows and order by the first column ascending
clean_dups <- clean_dups[!duplicated(clean_dups), ]
clean_dups <- clean_dups[order(clean_dups$X1),]

# save the cleaned version of the duplicates dataframe
write.csv(clean_dups, "C:/Users/other/Documents/Capstone/Data/final_duplicates.csv", row.names=FALSE)
