# Cleaning scraped data

library(tidyverse)
library(lubridate)

#cleaning the small data set

complaint_disp <- read_csv("conh_complaint_disposition_small.csv") %>% 
  distinct()

complaint_disp <- complaint_disp %>% 
  filter(X1 == "Last Inspection:" |
           X1 == "Disposition:" |
           X1 == "Comments:") %>% 
  distinct()

complaint_disp_wide <- complaint_disp %>% 
  pivot_wider(id_cols = c("complaint_num", "address", "bin", "boro", "zip"),
              names_from = "X1",
              values_from = "X2")

complaint_disp_clean <- complaint_disp_wide %>% 
  janitor::clean_names() %>% 
  rename(details = comments) %>% 
  mutate(inspection_date = mdy(gsub( " .*$", "", last_inspection)),
         disposition_date = mdy(gsub( " .*$", "", disposition)),
         disposition_code = sub("^.*-\\s+(.*?)\\s+-.*$", "\\1", disposition),
         comments = sub("^.*\\s+-(.*?)$", "\\1", disposition)) %>% 
  select(complaint_num, address, bin, boro, zip,
         inspection_date, disposition_date, disposition_code, comments, details) %>% 
  group_by(complaint_num) %>% 
  mutate(disp_num = n()) #this is to mimic the disp history comments numbering, for these guys it'll always be one 

#3 dates were blank 

#some regex:
# ^.*-\\s+   from the start, consume everything up to and including the first dash
# (.*?)      then match and capture everything up until the second dash
# \\s+-.*$   consume everything after and including the second dash


complaint_disp_big <- read_csv("conh_complaint_disposition.csv") %>% 
  distinct()

#creating blank dataframe with the same column names
comp_filler <- complaint_disp_big %>% 
  arrange(complaint_num)

#to make sure the details vector is the same length as the org df
#this adds a period after any instance where a comment exists but not additional details
for (i in 1:length(comp_filler$X4)) {
  if(rowSums(!is.na(comp_filler[i,1:6]))== 6 & #checking that there are no NA values
     !str_detect(comp_filler[i,4], "Disposition") & #and that this isn't the disposition header rows
     !rowSums(is.na(comp_filler[i+1, 1:6])) == 5) 
     {

    comp_filler <- add_row(comp_filler, 
                           X4 = ".",
                           complaint_num = as.integer(comp_filler[i,7]),
                           address = as.character(comp_filler[i,8]),
                           bin = as.integer(comp_filler[i,9]),
                           boro = as.character(comp_filler[i,10]),
                           zip = as.integer(comp_filler[i,11]),
                           .before = i+1)
  
  }
} 

#comp num: 3591664
comp_filler <- add_row(comp_filler, 
                       X4 = ".",
                       complaint_num = as.integer(comp_filler[4609,7]),
                       address = as.character(comp_filler[4609,8]),
                       bin = as.integer(comp_filler[4609,9]),
                       boro = as.character(comp_filler[4609,10]),
                       zip = as.integer(comp_filler[4609,11]),
                       .before = 4609+1)
#comp num: 3664241
comp_filler <- add_row(comp_filler, 
                       X4 = ".",
                       complaint_num = as.integer(comp_filler[4979,7]),
                       address = as.character(comp_filler[4979,8]),
                       bin = as.integer(comp_filler[4979,9]),
                       boro = as.character(comp_filler[4979,10]),
                       zip = as.integer(comp_filler[4979,11]),
                       .before = 4979+1)
#comp num: 3671400
comp_filler <- add_row(comp_filler, 
                       X4 = ".",
                       complaint_num = as.integer(comp_filler[4609,7]),
                       address = as.character(comp_filler[4609,8]),
                       bin = as.integer(comp_filler[4609,9]),
                       boro = as.character(comp_filler[4609,10]),
                       zip = as.integer(comp_filler[4609,11]),
                       .before = 4609+1)


#copying the DF to extract the disposition details into a vector to be merged 
comp_details <- comp_filler

comp_details <- comp_details %>% 
  filter(rowSums(is.na(comp_details[,1:6])) == 5)  #okay so this filters out all the details 
  #select(X4,complaint_num)

comp_details_vec <- c(comp_details$X4)

#testing
comp_details_test <- comp_details %>% 
  group_by(complaint_num) %>% 
  mutate(row_id = row_number(),
         comp_row = paste(complaint_num, row_id, sep = "-"))


complaint_big_clean_test <- complaint_big_clean %>% 
  mutate(comp_row = paste(complaint_num, X1, sep = "-"))

test <- setdiff(complaint_big_clean_test$comp_row, comp_details_test$comp_row )
  
#adding the details to the org df 
complaint_big_clean <- complaint_disp_big %>% 
  filter(rowSums(is.na(complaint_disp_big[,1:6])) == 0) %>% #filtering out rows with any NA values 
  filter(!grepl('#', X1)) #removing the weird column names 

complaint_big_clean$details <- comp_details_vec

complaint_big_clean <- complaint_big_clean %>% 
  mutate(inspection_date = mdy(X6),
         disposition_date = mdy(X2),
         disposition_code = X3,
         comments = X4,
         disp_num = X1) %>% 
  select(complaint_num, address, bin, boro, zip,
    inspection_date, disposition_date, disposition_code, comments, details, disp_num)


#okay now to merge the smaller complaint file with the larger one
#order on BIN, by date

complaint_clean_final <- rbind(complaint_big_clean, complaint_disp_clean)

#arranges by inspection date to get the small complaint disp in the right order
complaint_clean_final_ordered <- complaint_clean_final %>% 
  group_by(bin, complaint_num) %>% 
  arrange(inspection_date, .by_group = T) %>% 
  ungroup()

#then orders by disp num
complaint_clean_final_ordered <- complaint_clean_final_ordered %>% 
  group_by(bin, complaint_num) %>% 
  arrange(desc(disp_num), .by_group = T)

  
  


