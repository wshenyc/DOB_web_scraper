library(tidyverse)
require(rvest)
require(httr)
library(RSelenium)
library(stringr)
library(lubridate)


##----helper functions----
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

##----initializing dataframes & variables----
complaint_disposition <- data.frame()
complaint_disp_small <- data.frame()

bis_url <- ""
webElem <- NULL 

##----test data----
test_data <- read_csv("~/Downloads/dob_vacates_20220411.csv") 
test_bins <- test_data[151:195,]$bin

##----remote screen set up----
rd <- rsDriver(browser = c("firefox"), port = 4565L)
remote_driver <- rd[["client"]]

##----actual for loop scraping----

for (i in 1:length(test_bins)) {
  #navigate to the respective BIN's DOB page 
  bis_url <- paste("https://a810-bisweb.nyc.gov/bisweb/PropertyProfileOverviewServlet?bin=",
                   test_bins[i],"&go4=+GO+&requestid=0", sep="")
  
  remote_driver$navigate(bis_url)
  
  #loop until the property page table loads
  while(is.null(webElem)){
    webElem <- tryCatch({remote_driver$findElement(using = 'link text',
                                                   'Complaints')},
                        error = function(e){NULL})
  }
  
  #find and click on the complaints button
  complaints <- remote_driver$findElement(
    using = "link text", 
    "Complaints")
  
  complaints$clickElement() #this will make it click on the complaints button
  
  #loop until the complaints table has loaded hopefully 
  while(is.null(webElem)){
    webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                   'body > center:nth-child(1) > table:nth-child(2)')},
                        error = function(e){NULL})
  }

  
##-----looping through the complaints----

  #if there is a next button OR a previous button 
  if(length(remote_driver$findElements(using="name",
                                       "next")) !=0 |
     length(remote_driver$findElements(using = "name",
                                       "previous")) != 0 ) {
    
    #click next while there's a next button 
    while(length(remote_driver$findElements(using='name', 'next')) !=0) {
      
      comp_table<-remote_driver$getPageSource()[[1]]%>% 
        read_html() %>% 
        html_nodes("table") %>% 
        html_table()
      
      #gets all the complaint numbers 
      each_complaint <- as.data.frame(comp_table[[4]])
      
      comp_num <- numextract(each_complaint$X1)
      
      comp_num <- comp_num[!is.na(comp_num)]
      
      #loops through the complaint table, clicking on each complaint number
      for (i in 1:length(comp_num)) {
        
        #add complaint number to final df
        
        
        webElems <- remote_driver$findElement("link text", comp_num[i])
        webElems$clickElement()
        
        #loop until complaint disposition table loads?
        while(is.null(webElem)){
          webElem <- tryCatch({remote_driver$findElement(using =  'css selector', 
                                                         'body > center:nth-child(1) > table:nth-child(1)')},
                              error = function(e){NULL})
        }
      
        #data scraping
        #gotta check if the table is blank
        if (length(remote_driver$findElements(using = "css selector", 
                                              "body > center:nth-child(1) > table:nth-child(13) > tbody:nth-child(1) > tr:nth-child(5)"))
            != 0) {
          
          disp_table <-remote_driver$getPageSource()[[1]]%>% 
            read_html() %>% 
            html_nodes("table") %>% 
            html_table()
          
          disp_history <- as.data.frame(disp_table[[11]]) 
          
          #adding the complaint number
          disp_history$complaint_num <- comp_num[i]
          
          #separate table called to get building info 
          bldg_details <- as.data.frame(disp_table[[3]]) 
          bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
          bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
          bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
          bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
          
          #joined with the disp history
          disp_history$address <- bldg_address
          disp_history$bin <- bldg_bin
          disp_history$boro <- bldg_boro
          disp_history$zip <- bldg_zip
          
          complaint_disposition <- rbind(complaint_disposition, disp_history)
          
        } else {
          #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
          disp_table <-remote_driver$getPageSource()[[1]]%>% 
            read_html() %>% 
            html_nodes("table") %>% 
            html_table()
          
          disp_comments <- as.data.frame(disp_table[[8]]) 
          
          #adding the complaint number
          disp_comments$complaint_num <- comp_num[i]
          
          #separate table called to get building info 
          bldg_details <- as.data.frame(disp_table[[3]]) 
          bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
          bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
          bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
          bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
          
          #joined with the disp history
          disp_comments$address <- bldg_address
          disp_comments$bin <- bldg_bin
          disp_comments$boro <- bldg_boro
          disp_comments$zip <- bldg_zip
          
          complaint_disp_small <- rbind(complaint_disp_small, disp_comments)
        }
        
        
        #after it's done scraping it goes back to the complaint table 
        remote_driver$goBack()
        
        #loop until complaint table loads
        while(is.null(webElem)){
          webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                         'body > center:nth-child(1) > table:nth-child(2)')},
                              error = function(e){NULL})
        }
      }#for loop closure
      
      #after it scrapes all the complaints available, it'll click the next button
      next_button <- remote_driver$findElement(using = 'name',
                                               'next')
      
      next_button$clickElement() #click next button 
      
      
    }#while there's a next button on the page, it'll re-run the above code 
    
    #if there's no more next button and just a previous button 
    if (length(remote_driver$findElements(using="name",
                                          "next")) ==0 &
        length(remote_driver$findElements(using = "name",
                                          "previous")) != 0 ) {
      
      comp_table<-remote_driver$getPageSource()[[1]]%>% 
        read_html() %>% 
        html_nodes("table") %>% 
        html_table()
      
      #gets all the complaint numbers 
      each_complaint <- as.data.frame(comp_table[[4]])
      
      comp_num <- numextract(each_complaint$X1)
      
      comp_num <- comp_num[!is.na(comp_num)]
      
      #loops through the complaint table, clicking on each complaint number
      for (i in 1:length(comp_num)) {
        
        webElems <- remote_driver$findElement("link text", comp_num[i])
        webElems$clickElement()
        
        #loop until complaint disposition table loads?
        while(is.null(webElem)){
          webElem <- tryCatch({remote_driver$findElement(using =  'css selector', 
                                                         'body > center:nth-child(1) > table:nth-child(1)')},
                              error = function(e){NULL})
        }
        
        #data scraping
        #gotta check if the table is blank
        if (length(remote_driver$findElements(using = "css selector", 
                                              "body > center:nth-child(1) > table:nth-child(13) > tbody:nth-child(1) > tr:nth-child(5)"))
            != 0) {
          
          disp_table <-remote_driver$getPageSource()[[1]]%>% 
            read_html() %>% 
            html_nodes("table") %>% 
            html_table()
          
          disp_history <- as.data.frame(disp_table[[11]]) 
          
          #adding the complaint number
          disp_history$complaint_num <- comp_num[i]
          
          #separate table called to get building info 
          bldg_details <- as.data.frame(disp_table[[3]]) 
          bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
          bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
          bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
          bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
          
          #joined with the disp history
          disp_history$address <- bldg_address
          disp_history$bin <- bldg_bin
          disp_history$boro <- bldg_boro
          disp_history$zip <- bldg_zip
          
          complaint_disposition <- rbind(complaint_disposition, disp_history)
          
        } else {
          #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
          disp_table <-remote_driver$getPageSource()[[1]]%>% 
            read_html() %>% 
            html_nodes("table") %>% 
            html_table()
          
          disp_comments <- as.data.frame(disp_table[[8]]) 
          
          #adding the complaint number
          disp_comments$complaint_num <- comp_num[i]
          
          #separate table called to get building info 
          bldg_details <- as.data.frame(disp_table[[3]]) 
          bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
          bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
          bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
          bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
          
          #joined with the disp history
          disp_comments$address <- bldg_address
          disp_comments$bin <- bldg_bin
          disp_comments$boro <- bldg_boro
          disp_comments$zip <- bldg_zip
          
          complaint_disp_small <- rbind(complaint_disp_small, disp_comments)
        }
        
        
        #after it's done scraping it goes back to the complaint table 
        remote_driver$goBack()
        
        #loop until complaint table loads
        while(is.null(webElem)){
          webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                         'body > center:nth-child(1) > table:nth-child(2)')},
                              error = function(e){NULL})
        }
      }#for loop closure
    }#if statement closure for just previous button 
    
  } #if statement closure for next and previous button
  #if there isn't a next button, just run normally
  else {
    comp_table<-remote_driver$getPageSource()[[1]]%>% 
      read_html() %>% 
      html_nodes("table") %>% 
      html_table()
    
    #gets all the complaint numbers 
    each_complaint <- as.data.frame(comp_table[[4]])
    
    comp_num <- numextract(each_complaint$X1)
    
    comp_num <- comp_num[!is.na(comp_num)]
    
    #loops through the complaint table, clicking on each complaint number
    for (i in 1:length(comp_num)) {
      
      #add complaint number to final df
      
      
      webElems <- remote_driver$findElement("link text", comp_num[i])
      webElems$clickElement()
      
      #loop until complaint disposition table loads?
      while(is.null(webElem)){
        webElem <- tryCatch({remote_driver$findElement(using =  'css selector', 
                                                       'body > center:nth-child(1) > table:nth-child(1)')},
                            error = function(e){NULL})
      }
      
     #data scraping
      #gotta check if the table is blank
      if (length(remote_driver$findElements(using = "css selector", 
                                            "body > center:nth-child(1) > table:nth-child(13) > tbody:nth-child(1) > tr:nth-child(5)"))
          != 0) {
        
        disp_table <-remote_driver$getPageSource()[[1]]%>% 
          read_html() %>% 
          html_nodes("table") %>% 
          html_table()
        
        disp_history <- as.data.frame(disp_table[[11]]) 
        
        #adding the complaint number
        disp_history$complaint_num <- comp_num[i]
        
        #separate table called to get building info 
        bldg_details <- as.data.frame(disp_table[[3]]) 
        bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
        bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
        bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
        bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
        
        #joined with the disp history
        disp_history$address <- bldg_address
        disp_history$bin <- bldg_bin
        disp_history$boro <- bldg_boro
        disp_history$zip <- bldg_zip
        
        complaint_disposition <- rbind(complaint_disposition, disp_history)
        
      } else {
        #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
        disp_table <-remote_driver$getPageSource()[[1]]%>% 
          read_html() %>% 
          html_nodes("table") %>% 
          html_table()
        
        disp_comments <- as.data.frame(disp_table[[8]]) 
        
        #adding the complaint number
        disp_comments$complaint_num <- comp_num[i]
        
        #separate table called to get building info 
        bldg_details <- as.data.frame(disp_table[[3]]) 
        bldg_address <- str_squish(sub('Complaint at:', '',bldg_details$X1[1]))
        bldg_bin <- str_squish(sub('BIN:', '',bldg_details$X3[1]))
        bldg_boro <- str_squish(sub('Borough:', '',bldg_details$X5[1]))
        bldg_zip <- str_squish(sub('ZIP:', '',bldg_details$X7[1]))
        
        #joined with the disp history
        disp_comments$address <- bldg_address
        disp_comments$bin <- bldg_bin
        disp_comments$boro <- bldg_boro
        disp_comments$zip <- bldg_zip
        
        complaint_disp_small <- rbind(complaint_disp_small, disp_comments)
      }
      
      
      #after it's done scraping it goes back to the complaint table 
      remote_driver$goBack()
      
      #loop until complaint table loads
      while(is.null(webElem)){
        webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                       'body > center:nth-child(1) > table:nth-child(2)')},
                            error = function(e){NULL})
      }
    }#else statement closure 
    
  }#clicking on all the complaints and scraping loop closure

}#master for loop closure


#saving the results for the first 1:46 buildings
complaint_disp_small_one <- complaint_disp_small
complaint_disposition_one <- complaint_disposition

# complaint_disp_small_one %>% write_csv("complaint_disp_small_one.csv")
# complaint_disposition_one %>% write_csv("complaint_disposition_one.csv")

#saving the results for 47:71 buildings
complaint_disp_small_two <- complaint_disp_small
complaint_disposition_two <- complaint_disposition

# complaint_disp_small_two %>% write_csv("complaint_disp_small_two.csv")
# complaint_disposition_two %>% write_csv("complaint_disposition_two.csv")

#saving the results for 71:90 buildings
complaint_disp_small_three <- complaint_disp_small
complaint_disposition_three <- complaint_disposition

# complaint_disp_small_three %>% write_csv("complaint_disp_small_three.csv")
# complaint_disposition_three %>% write_csv("complaint_disposition_three.csv")

#saving the results for 91:110 buildings
complaint_disp_small_four <- complaint_disp_small
complaint_disposition_four <- complaint_disposition

# complaint_disp_small_four %>% write_csv("complaint_disp_small_four.csv")
# complaint_disposition_four %>% write_csv("complaint_disposition_four.csv")

#saving the results for 111:150 buildings
complaint_disp_small_five <- complaint_disp_small
complaint_disposition_five <- complaint_disposition

# complaint_disp_small_five %>% write_csv("complaint_disp_small_five.csv")
# complaint_disposition_five %>% write_csv("complaint_disposition_five.csv")

#saving the results for 151:195 buildings
complaint_disp_small_six <- complaint_disp_small
complaint_disposition_six <- complaint_disposition

complaint_disp_small_six %>% write_csv("complaint_disp_small_six.csv")
complaint_disposition_six %>% write_csv("complaint_disposition_six.csv")

complaint_disposition <- rbind(complaint_disposition_one,
                               complaint_disposition_two,
                               complaint_disposition_three,
                               complaint_disposition_four,
                               complaint_disposition_five,
                               complaint_disposition_six)

complaint_disp_small <- rbind(complaint_disp_small_one,
                              complaint_disp_small_two,
                              complaint_disp_small_three,
                              complaint_disp_small_four,
                              complaint_disp_small_five,
                              complaint_disp_small_six)

complaint_disposition %>% write_csv("conh_complaint_disposition.csv")
complaint_disp_small %>% write_csv("conh_complaint_disposition_small.csv")
