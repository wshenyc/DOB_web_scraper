library(tidyverse)
require(rvest)
require(httr)
library(RSelenium)
library(stringr)


##----helper functions----
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

##----test data----
test_data <- read_csv("DOB_Complaints_Received.csv")
sample_bins <- test_data$BIN
sample_small <- sample_bins[1:10] #let's cut down the size for now 

##----remote screen set up----
rd <- rsDriver(browser = c("firefox"))
remote_driver <- rd[["client"]]

#looping through the page
#https://a810-bisweb.nyc.gov/bisweb/PropertyProfileOverviewServlet?bin=1060482&go4=+GO+&requestid=0

# complaint_disposition <- data.frame(bin = integer(),
#                                     address = character(),
#                                     complaint_num = integer(),
#                                     disp_date = character(),
#                                     disp_code = character(),
#                                     comments = character())

complaint_disposition <- data.frame()
complaint_disp_small <- data.frame()

bis_url <- ""
webElem <- NULL 

for (i in 1:length(sample_small)) {
  #navigate to the respective BIN's DOB page 
  bis_url <- paste("https://a810-bisweb.nyc.gov/bisweb/PropertyProfileOverviewServlet?bin=",
                   sample_small[i],"&go4=+GO+&requestid=0", sep="")
  
  remote_driver$navigate(bis_url)
  
  #loop until the property page table loads
  while(is.null(webElem)){
    webElem <- tryCatch({remote_driver$findElement(using = 'css selector',
                                                   'body > center:nth-child(1) > table:nth-child(2)')},
                        error = function(e){NULL})
  }

  
  #
  
  #find and click on the complaints button
  complaints <- remote_driver$findElement(
    using = "css selector", 
    "body > center:nth-child(1) > table:nth-child(8) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > b:nth-child(1) > a:nth-child(1)")
  
  complaints$clickElement() #this will make it click on the complaints button
  

  
  #loop until the complaints table has loaded hopefully 
  while(is.null(webElem)){
    webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                   'body > center:nth-child(1) > table:nth-child(2)')},
                        error = function(e){NULL})
  }
  
  
##-----looping through the complaints----
  comp_table<-remote_driver$getPageSource()[[1]]%>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table()
  
  #gets all the complaint numbers 
  each_complaint <- as.data.frame(comp_table[[4]])
  
  comp_num <- numextract(each_complaint$X1)
  
  comp_num <- comp_num[!is.na(comp_num)]
  
  #if there is a next button 
  if(length(remote_driver$findElements(using="name",
                                       "next")) !=0) {
    
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
        
        ###insert scraping the actual data
        #gotta check if the table is blank
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
          complaint_disposition <- rbind(complaint_disposition, disp_history)
          
        } else {
          #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
          disp_table <-remote_driver$getPageSource()[[1]]%>% 
            read_html() %>% 
            html_nodes("table") %>% 
            html_table()
          
          disp_comments <- as.data.frame(disp_table[[8]]) 
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
      
      #this will click the next button 
      
      next_button <- remote_driver$findElement(using = 'name',
                                               'next')
      
      next_button$clickElement() #click next button 
      
      
    }#while loop run this true 
    
    
    #if there isn't a next button, just run normally
  } else {
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
        complaint_disposition <- rbind(complaint_disposition, disp_history)
        
      } else {
        #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
        disp_table <-remote_driver$getPageSource()[[1]]%>% 
          read_html() %>% 
          html_nodes("table") %>% 
          html_table()
        
        disp_comments <- as.data.frame(disp_table[[8]]) 
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
  
  

  # #loops through the complaint table, clicking on each complaint number
  # for (i in 1:length(comp_num)) {
  #   
  #   #add complaint number to final df
  #   
  #   
  #   webElems <- remote_driver$findElement("link text", comp_num[i])
  #   webElems$clickElement()
  #   
  #   #loop until complaint disposition table loads?
  #   while(is.null(webElem)){
  #     webElem <- tryCatch({remote_driver$findElement(using =  'css selector', 
  #                                                    'body > center:nth-child(1) > table:nth-child(1)')},
  #                         error = function(e){NULL})
  #   }
  #   
  #   ###insert scraping the actual data
  #   #gotta check if the table is blank
  #   #if (remote_driver$findElement())
  #   
  #   #after it's done scraping it goes back to the complaint table 
  #   remote_driver$goBack()
  #   
  #   #loop until complaint table loads
  #   while(is.null(webElem)){
  #     webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
  #                                                    'body > center:nth-child(1) > table:nth-child(2)')},
  #                         error = function(e){NULL})
  #   }
  #   
  # }#clicking on all the complaints and scraping loop closure
  # 
  
}#master for loop closure

########################################################
##----let's test some stuff----
# /html/body/center/table[8]/tbody/tr/td[1]/table/tbody/tr[2]/td[1]/b/a
# body > center > table:nth-child(8) > 
# tbody > tr > td:nth-child(1) > table > tbody > tr:nth-child(2) > td:nth-child(1) > b > a
complaints <- remote_driver$findElement(
  using = "css selector", "tbody > tr > td:nth-child(1) > table > tbody > tr:nth-child(2) > td:nth-child(1) > b > a")

complaints$clickElement() #this will make it click on the complaints button

#specific complaints
# body > center:nth-child(1) > table:nth-child(4) > 
# tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > a:nth-child(1)

# body > center:nth-child(1) > table:nth-child(4) > 
# tbody:nth-child(1) > tr:nth-child(5) > td:nth-child(1) > a:nth-child(1)

# each_complaint <- remote_driver$findElements(
#   using = "css selector", "body > center:nth-child(1) > table:nth-child(4) > tbody:nth-child(1)"
# )
# 



test<-remote_driver$getPageSource()[[1]]%>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table()

each_complaint <- as.data.frame(test[[4]])

comp_num <- numextract(each_complaint$X1)

comp_num <- comp_num[!is.na(comp_num)]

#okay so this gives me all the complaint numbers
#can i somehow make the computer know to go through all these links?

for (i in 1:length(comp_num)) {
  webElems <- remote_driver$findElement("link text", comp_num[i])
  webElems$clickElement()
  
  #loop until page loads
  while(is.null(webElem)){
    webElem <- tryCatch({remote_driver$findElement(using =   'css selector', 
                                                   'body > center:nth-child(1) > table:nth-child(1)')},
                        error = function(e){NULL})
  }
  
  remote_driver$goBack()
  
}

webElems <- remote_driver$findElement("link text", "2310044") 

webElems$clickElement() #holy shit that actually worked!!!!!

#body > center:nth-child(1) > table:nth-child(13) > tbody:nth-child(1) > tr:nth-child(4)
##testing

#checks to see if there are comments in the disposition history table 
if (length(remote_driver$findElements(using = "css selector", 
                              "body > center:nth-child(1) > table:nth-child(13) > tbody:nth-child(1) > tr:nth-child(5)"))
    != 0) {
  
  disp_table <-remote_driver$getPageSource()[[1]]%>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table()
  
  disp_history <- as.data.frame(disp_table[[11]]) 
  
} else {
  #body > center:nth-child(1) > table:nth-child(2) > tbody:nth-child(1)
  disp_table <-remote_driver$getPageSource()[[1]]%>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table()
  
  disp_comments <- as.data.frame(disp_table[[8]]) 
}

#this table gives me: address, BIN, Borough, zip
test_bin <- as.data.frame(disp_table[[3]])

#I can get the complaint number from the first page an just save it 

#testing
test_val <- 2302031 

complaint_disposition <- complaint_disposition %>% 
  cbind(test_val)

##need to work out how to deal with the next button for complaints
##-----looping through the complaints----

#if there is a next button 
if(length(remote_driver$findElements(using="name",
                              "next")) !=0) {
  
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
    
    ###insert scraping the actual data
    #gotta check if the table is blank
    #if (remote_driver$findElement())
    
    #after it's done scraping it goes back to the complaint table 
    remote_driver$goBack()
    
    #loop until complaint table loads
    while(is.null(webElem)){
      webElem <- tryCatch({remote_driver$findElement(using = 'css selector', 
                                                     'body > center:nth-child(1) > table:nth-child(2)')},
                          error = function(e){NULL})
    }
  }#for loop closure
  
  #this will click the next button 
 
  next_button <- remote_driver$findElement(using = 'name',
                                           'next')
  
  next_button$clickElement() #click next button 
  
  
  }#while loop run this true 
  
 
#if there isn't a next button, just run normally
} else {
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
  
  ###insert scraping the actual data
  #gotta check if the table is blank
  #if (remote_driver$findElement())
  
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

