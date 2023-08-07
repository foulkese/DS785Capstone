#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("viridis")
library(viridis)   
library(ggplot2)
library(stringr)
library(dplyr)

# Load in full dataset


# Reformat date from Epoch ms to Year
data_in[['dateAdded']] <- as.POSIXct(data_in[['dateAdded']]/1000,
                                   format = "%Y-%m-%d",
                                   origin = "1970-01-01")

data_in[['year']] <- format(as.POSIXct(data_in[['dateAdded']],format='%m/%d/%Y %H:%M:%S'),
                                 format='%Y')

# Reformat entire datafram to characters and replace null and empty values
df1 <- data.frame(lapply(data_in, as.character))
df1[df1 == 'NULL'] <- NA
df1[df1 == ''] <- NA

# Remove all non-alphanumeric characters from the source column
df1[['source']] <- str_replace_all(df1[['source']], "[^[:alnum:]]", "")

# Replace all Candidate Email Parser source values to CareerBuilder - are the same source technically
df1 <- df1 %>%
  mutate(across('source', str_replace, 'CandidateEmailParser', 'CareerBuilder'))

# Remove all sources except the main external sources
keep_sources <-  c('CareerBuilder', 'LinkedIn', 'Indeed', 'ZoomInfo')
df1$source[!(df1$source %in% keep_sources)] <- NA

## State Abbreviations ##

# Create a dataframe of just the records with State Abbreviations for analysis
state_abbrev <- df1[nchar(df1$address.state) == 2, ]
state_abbrev <- state_abbrev[!is.na(state_abbrev$address.state),]

# Plot the state abbreviations against year the record was added to the CRM
ggplot(data=subset(state_abbrev, !is.na(year)), aes(year)) + 
  geom_bar(fill="lightblue") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Year") + 
  ylab("Count") + 
  labs(title = "Records with State Abbreviation and Year Added")

# Plot the state abbreviations against source the record was added to the CRM from
ggplot(data=subset(state_abbrev, !is.na(source)), aes(source)) + 
  geom_bar(fill="lightblue") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(expand = c(.17, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Source") + 
  ylab("Count") + 
  labs(title = "Records with State Abbreviation and Source Added From")


## Zip Codes ##

# Create dataframe of records missing zip codes
no_zip <- df1[is.na(df1$address.zip), ]

# Plot missing zip codes against year the record was added to the CRM 
ggplot(no_zip, aes(year)) + 
  geom_bar(fill="lightblue") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Source") + 
  ylab("Count") + 
  labs(title = "Records with Missing Zip Code by Year Added")

# Plot missing zip codes against source the record was added to the CRM from
ggplot(data=subset(no_zip, !is.na(source)), aes(source)) + 
  geom_bar(fill="lightblue") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(expand = c(.17, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Source") + 
  ylab("Count") + 
  labs(title = "Records with Missing Zip Code by Source Added From")

# Plot missing zip codes against year the record was added to the CRM faceted by source from
ggplot(data=subset(no_zip, !is.na(source)), aes(year)) + 
  geom_bar(fill="lightblue") +
  facet_wrap(~source) + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        strip.background = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Source") + 
  ylab("Count") + 
  labs(title = "Records with Missing Zip Code by Year Added and Source")


## Name Initials Distribution ##

# First Name #

# clean the dataframe to all lowercase characters
df <- data.frame(lapply(data_in, as.character))

df <- data.frame(lapply(df, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

# remove unknown first names
df <-  df[- grep("unknown", df$firstName),]

# create a dataframe of how many occurances of each first initial for first names
first_initials <- data.frame(matrix(nrow = 0, ncol = 2))
for (fi in letters[1:26]) {
  first_initial <- subset(df, str_sub(df$firstName, 1, 1)  %in% c(fi) )
  first_initials <- rbind(first_initials, c(fi, nrow(first_initial)))
}

# format the count value as numeric
first_initials$X.84496. <- as.numeric(first_initials$X.84496.)

# plot the initials against the count
ggplot(first_initials, aes(x = reorder(X.a., -X.84496.), y = X.84496.)) + 
  geom_bar(fill="lightblue",
           stat = "identity") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        strip.background = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  xlab("First Initial") + 
  ylab("Count")  + 
  labs(title = "Distribution of First Initial of First Names")
  
# Last Name #

# create a dataframe of how many occurances of each first initial for last names
last_initials <- data.frame(matrix(nrow = 0, ncol = 2))
for (li in letters[1:26]) {
  last_initial <- subset(df, str_sub(df$lastName, 1, 1)  %in% c(li) )
  last_initials <- rbind(last_initials, c(li, nrow(last_initial)))
}

# format the count value as numeric
last_initials$X.43284. <- as.numeric(last_initials$X.43284.)

# plot the initials against the count
ggplot(last_initials, aes(x = reorder(X.a., -X.43284.), y = X.43284.)) + 
  geom_bar(fill="lightblue",
           stat = "identity") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        strip.background = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  xlab("First Initial") + 
  ylab("Count") + 
  labs(title = "Distribution of First Initial of Last Names")

# First and Last Name #

# create a dataframe of how many occurances of each first initial combination for both first and last names
initial_combo <- data.frame(matrix(nrow = 0, ncol = 3))
for (fi in letters[1:26]) {
  first_initial <- subset(df, str_sub(df$firstName, 1, 1)  %in% c(fi) )
  for (li in letters[1:26]){
    initials <- subset(first_initial, str_sub(first_initial$lastName, 1, 1)  %in% c(li) )
    initial_combo <- rbind(initial_combo, c(fi, li, nrow(initials)))
  }
}

# format the count value as numeric
initial_combo$X.4514. <- as.numeric(initial_combo$X.4514.)

# take the top 10 first initials for first names and last names
top_first <- first_initials[order(-first_initials$X.84496.),][1:10,1]
top_last <- last_initials[order(-last_initials$X.43284.),][1:10, 1]

# filter the initial combinations to just the top 10 for each
initial_combo <- initial_combo %>%  
  filter(X.a. %in% top_first& 
           X.a..1 %in% top_last)

# Plot a heatmap of the counts of each combination for the top 10 initials
ggplot(initial_combo, aes(x = reorder(X.a..1, -X.4514.), y = reorder(X.a., X.4514.))) + 
  geom_tile(aes(fill = X.4514.)) +
  coord_fixed() +
  scale_fill_gradientn(colors = viridis(10),
                       name = "Count") + 
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank()) +
  xlab("Last Initial") +
  ylab("First Initial") + 
  labs(title = "Heatmap of Occurrences of Initial Combinations for Top 10 Initials")

