VERSION <- "0.1"


# Import libraries
requiredpackages <- c(
  "tidyverse", "readxl", "writexl", "tidyr", "dplyr","rlist")

for (pkg in requiredpackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE) {
    install.packages(pkg)
  }
  if (pkg %in% rownames(.packages()) == FALSE) {
    library(pkg, character.only = TRUE)
  }
}


# Import data from excel
data_hotline <- read_excel('data.xlsx')

# Change name of variable from Details to Text
names(data_hotline)[names(data_hotline) == "Details...16"] <- "Text"

# Add space at the end of the text
data_hotline = data_hotline %>% mutate("Text" = paste0(Text," "))

# create an empty list
data <- list()

# create new variables with Text
text <- data_hotline$Text

# create new variable with names
name <- data_hotline$`Counterparty: Person`

# create new variable with organization information
organization <- data_hotline$`Counterparty: Organisation`

# split organization words into different tokens, based on "(" or ")".
org_tokens <- strsplit(organization,'\\(|\\)')

# Substitute NA values inside org_toneks with "missing_organization_in_data". In this way we avoid substituting "NA" inside the text.
org_tokens = lapply(org_tokens, function(x) replace_na(x,"missing_organization_in_data"))


for(i in 1: length(name)){
  
  # Remove names with length lower than  to 3 (1-2 digits) 
  name[i]= (gsub('\\b\\w{1,2}\\b','missing_name_data',name[i]))
  
  # Remove space after organization name (e.g Asahi Shimbun was problematic)
  org_tokens[i]<- lapply(org_tokens[i], function (x) sub("\\s+$", "", x))
  
}

# split names into tokens based on space (" ")
name_tokens <- strsplit(name,' ') 


# add arg_tokens in "data" list
data$organization <- org_tokens
# add text in "data" list
data$Text <- (text)
# add name_tokens in "data" list
data$name_tokens <- name_tokens

data$org_tokens <- org_tokens
# add name variable in "data" list
data$name <- (name)

# create empty sublists inside "data" list
data$names_with_commas = {}
data$names_with_space = {}
data$names_with_dot= {}
data$names_raw= {}



for(i in 1: length(name_tokens)){
  
  # inside "data" create a list of name information with comma, space and dot at the end 
  data$names_with_commas[[i]] = paste0(name_tokens[[i]], ',')
  data$names_with_space[[i]] =  paste0(name_tokens[[i]], ' ') 
  data$names_with_dot[[i]] =  paste0(name_tokens[[i]], '.')
  data$names_raw[[i]]=paste0(name_tokens[[i]], '')
  
  # for each user, append user's names (first, middle, last names) with comma, dot and space in a unique list called name_tokens
  data$name_tokens[[i]] <- append(data$names_with_commas[[i]],data$names_with_space[[i]])
  data$name_tokens[[i]] <- append(data$name_tokens[[i]],data$names_with_dot[[i]])
  data$name_tokens[[i]] <- append(data$name_tokens[[i]],data$names_raw[[i]])
}


org_tokens_large  =org_tokens[lapply(org_tokens,length)>1]
org_tokens_small  =org_tokens[lapply(org_tokens,length)<=1]

data$tok_large <- org_tokens_large

data$tok_small <- org_tokens_small
comma_large={}
space_large={}
dot_large={}
raw_large={}

comma_small={}
space_small={}
dot_small={}
raw_small={}

for(i in 1: length(org_tokens_large)){
  
  # inside "data" create a list of name information with comma, space and dot at the end 
  data$comma_large[[i]] = paste0(org_tokens_large[[i]], ',')
  data$space_large[[i]] =  paste0(org_tokens_large[[i]], ' ') 
  data$dot_large[[i]] =  paste0(org_tokens_large[[i]], '.')
  data$raw_large[[i]] =  paste0(org_tokens_large[[i]],'')
  
  # for each user, append user's names (first, middle, last names) with comma, dot and space in a unique list called name_tokens
  data$tok_large[[i]] <- append(data$comma_large[[i]],data$space_large[[i]])
  data$tok_large[[i]] <- append(data$tok_large[[i]],data$dot_large[[i]])
  data$tok_large[[i]] <- append(data$tok_large[[i]],data$raw_large[[i]])
}
for(i in 1: length(org_tokens_small)){
  # inside "data" create a list of organization information with comma, space and dot at the end 
  data$comma_small[[i]] <- paste0(' ',org_tokens_small[[i]], ',')
  data$space_small[[i]] <-  paste0(' ',org_tokens_small[[i]],' ')
  data$dot_small[[i]] <-  paste0(' ',org_tokens_small[[i]],'.')
  data$raw_small[[i]]<-  paste0(' ',org_tokens_small[[i]],'')
  
  # for each organization, append user's organization with comma, dot and space in a unique list called org_tokens
  data$tok_small[[i]] <- append(data$comma_small[[i]],data$space_small[[i]])
  data$tok_small[[i]] <- append(data$tok_small[[i]],data$dot_small[[i]])
  data$tok_small[[i]] <- append(data$tok_small[[i]],data$raw_small[[i]])
  
}


data$org_tokens= append(data$tok_small,data$tok_large)




#Remove email from text
email_detect={}

for (i in c(1:length(data$Text))){
  
  email_detect[[i]] <- unlist(strsplit(data$Text[[i]], "\\s+"))
  data$Text[[i]] = paste(email_detect[[i]][!grepl("@|\\.com|\\.org|www\\.|\\.org|\\.in", email_detect[[i]])], collapse=" ")
  
}



#Remove phone numbers from text which start with country code or 00 and country code or with spaces and - in between
for (i in c(1:length(data$Text))){
  
  
  data$Text[[i]] = gsub(pattern = "[((00\\d\\d+) |(00\\d)|(\\+\\d\\d))].[0-9]{2}\\s*-?\\s*[0-9]{2}\\s*-?\\s*[0-9]{2}\\s*-?\\s*[0-9]{2}\\s*-?\\s*[0-9]{2}", data$Text[[i]], replace=" PHONEXXX " )
}



data$Text_correct <- character(length(data$Text))

for (i in c(1:length(data$Text))){
  
  #Remove all Names and Surnames and substitute with "NAMEXXX"
  data$Text_correct[i] <- gsub(eval(paste(data$name_tokens[[i]],collapse = '|')),' NAMEXXX ',data$Text[[i]], ignore.case = TRUE)
  
  #Remove Organization information and substitute with "ORGANIZATIONXXX"
  data$Text_correct[i] <- gsub(eval(paste(data$org_tokens[[i]],collapse = '|')),' ORGANIZATIONXXX ',data$Text_correct[[i]], ignore.case = TRUE)
  
  
}


data$name_tokens <- NULL
data$org_tokens <- NULL
data$organization <- NULL

data$names_with_commas  <- NULL
data$names_with_space  <- NULL
data$names_with_dot <- NULL
data$names_raw <- NULL

data$comma_large <- NULL
data$space_large <- NULL
data$dot_large <- NULL
data$raw_large <- NULL

data$comma_small <- NULL
data$space_small <- NULL
data$dot_small <- NULL
data$raw_small <- NULL

data$tok_large <- NULL
data$tok_small <- NULL


data <- as.data.frame(data)

##Statistics
##Count how many times name has been replaced
sum(grepl('NAMEXXX', data$Text_correct ), na.rm=TRUE)

##Count how many times organization has been replaced
sum(grepl('ORGANIZATIONXXX', data$Text_correct ), na.rm=TRUE)
#101

##Count how many times phone number has been replaced
sum(grepl('PHONEXXX', data$Text_correct ), na.rm=TRUE)

#Remove all text after Sign Off
signoff <- c('Best regards','Kind_regards','thank you very much in advance','Thanks in advance', 'Yours faithfully','sincerely',
             'Thank you in advance', 'All the best', 'Your sincerely','Regards','Thanks for your kindness','With gratitude','Sincerely',
             'Respectfully','Best wishes', 'Yours sincerely', 'sincerely yours','Thank you very much in advance','Many thanks,','Best,','Best greetings')

for(i in 1: length(signoff)){
  
  data <- data %>%
    mutate ('Text_correct' =  sub(paste0(signoff[i],".*"),"",Text_correct,ignore.case = TRUE ))
  
}


# #Substitute text in the original table
data_hotline$Text <- data$Text_correct

data_hotline <- data_hotline %>% select(No.,`ECB Responsible`, Subject, `Sector of Occupation`,Category, Subcategory, `Created on`, `Created at`, Text)

# #Save new excel file
write_xlsx(data_hotline,'data_cleaned.xlsx')








##--------------------------------------------------
##----Additional Cleaning - Organizations and Person----
##--------------------------------------------------

# 
# # JRC-Names https://ec.europa.eu/jrc/en/language-technologies/jrc-names
# 
# entities <- read.delim("entities.gzip", header=FALSE, comment.char="#", stringsAsFactors=FALSE)
# 
# jrc_names <- entities %>% filter(V2 == 'P') %>% select(V4) %>% mutate('V4' = strsplit(V4, '\\+'))
# jrc_names <- unlist(jrc_names$V4)
# jrc_names <- as.data.frame(jrc_names, stringsAsFactors = FALSE) %>% mutate('n' = nchar(jrc_names)) %>% filter(n>2) %>% distinct(jrc_names) %>% arrange(jrc_names)
# #n>4
# 
# # exclude all names with special characters
# jrc_names <- jrc_names %>%
#   mutate('jrc_names_detect_numbers' = str_detect(jrc_names,'[:digit:]')) %>%
#   # mutate('jrc_names_detect_specialA' = str_detect(jrc_names,'\\Å')) %>%
#   mutate('jrc_names_detect_special' = str_detect(jrc_names,  '([^[:alnum:]^[:blank:]])')) %>%
#   filter(jrc_names_detect_special == FALSE & jrc_names_detect_numbers == FALSE )%>% #& jrc_names_detect_specialA == FALSE) %>%
#   distinct(jrc_names)
# 
# #jrc_names <- jrc_names$jrc_names %>% as.vector()
# jrc_names <- trimws(jrc_names$jrc_names %>% as.vector())
# 
# jrc_names_dot <- paste0(" ", jrc_names, ".")
# jrc_names_comma <- paste0(" ", jrc_names, ",")
# jrc_names_space <- paste0(" ", jrc_names, " ")
# jrc_names <- sort(c(jrc_names_dot, jrc_names_comma, jrc_names_space))
# rm(jrc_names_dot, jrc_names_comma, jrc_names_space)
# 
# 
# 
# jrc_corp <- entities %>% filter(V2 == 'O') %>% select(V4)%>% mutate('jrc_corp' = gsub( '\\+', ' ', V4)) %>% select(jrc_corp)
# jrc_corp <- jrc_corp %>%
#   mutate('jrc_corp_detect_numbers' = str_detect(jrc_corp,'[:digit:]')) %>%
#   # mutate('jrc_corp_detect_specialA' = str_detect(jrc_corp,'\\Å')) %>%
#   mutate('jrc_corp_detect_special' = str_detect(jrc_corp,  '([^[:alnum:]^[:blank:]])')) %>%
#   filter(jrc_corp_detect_special == FALSE & jrc_corp_detect_numbers == FALSE)%>%# & jrc_corp_detect_specialA == FALSE) %>%
#   distinct(jrc_corp)
# 
# #jrc_corp <- jrc_corp$jrc_corp %>% as.vector()
# 
# jrc_corp <- trimws(jrc_corp$jrc_corp %>% as.vector())
# 
# jrc_corp_dot <- paste0(" ", jrc_corp, ".")
# jrc_corp_comma <- paste0(" ", jrc_corp, ",")
# jrc_corp_space <- paste0(" ", jrc_corp, " ")
# jrc_corp <- sort(c(jrc_corp_dot, jrc_corp_comma, jrc_corp_space))
# rm(jrc_corp_dot, jrc_corp_comma, jrc_corp_space)
# 
# for (i in c(1:length(data$Text_correct))){
#   
#   print(i)
#   #Remove all Names and Surnames and substitute with "NAMEXXX"
#   #data$Text_correct[i] <- gsub(eval(paste(jrc_names,collapse = '|')),' NAMEXXX ',data$Text[[i]], ignore.case = TRUE)
#   data$Text_correct[i] <- str_replace_all(regex(pattern = eval(paste(jrc_names,collapse = '|')), ignore_case = TRUE), replacement = ' NAMEYYYY ', string = data$Text_correct[[i]])
#   
#   
#   #Remove Organization information and substitute with "ORGANIZATIONXXX"
#   #data$Text_correct[i] <- gsub(eval(paste(jrc_corp,collapse = '|')),' ORGANIZATIONXXX ',data$Text_correct[[i]], ignore.case = TRUE)
#   data$Text_correct[i] <- str_replace_all(regex(pattern = eval(paste(jrc_corp,collapse = '|')), ignore_case = TRUE), replacement = ' ORGANIZATIONYYYY ', string = data$Text_correct[[i]])
#   
# }
# 
# 
# 
# ##--------------------------------------------------
# ##Alternative with tidyverse
# ##--------------------------------------------------
# 
# #data <- data %>% 
# #  mutate('Text_correct' = str_replace_all(regex(pattern = eval(paste(jrc_names,collapse = '|')), ignore_case = TRUE), replacement = ' NAMEXXX ', string = Text_correct)) %>%
# #  mutate('Text_correct' = str_replace_all(regex(pattern = eval(paste(jrc_corp,collapse = '|')), ignore_case = TRUE), replacement = ' ORGANIZATIONXXX ', string = Text_correct))
# 
# 
# ##Count how many times name has been replaced
# sum(grepl('NAMEYYYY', data$Text_correct ), na.rm=TRUE)
# 
# ##Count how many times organization has been replaced
# sum(grepl('ORGANIZATIONYYYY', data$Text_correct ), na.rm=TRUE)
# 
# #Substitute text in the original table
# data_hotline$Text <- data$Text_correct
# 
# #Save new excel file
# write_xlsx(data_hotline,'data_cleaned.xlsx')



