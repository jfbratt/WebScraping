library(rvest)
library(magrittr)
library(httr)
library(stringr)
library(XML)
library(selectr)
library(RCurl)
library(jsonlite)

#Reading in a CSV
url <- read.csv(file="EC2014.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
url <- as.data.frame(url)

#Calculate length of variable url
length <- nrow(url)

#Instantiate both check and error lists
checkList <- c()
errorList <- c()

#Loop through to identify the div class containing the post
for (i in 1:length)
  { tryCatch({

    ID <- paste(i, ".txt", sep="") # Create ID file name
      urlTest <- url_success(url[i,5]) #test URL for success
    
      html <- html(httr::GET(url[i,5])) #convert URL into SourceCode
      GET(handle=handle(url[i,5]))
      
      wp <- html_node(html, ".entry-content")
      bl <- html_node(html, ".post-body")
      ptp <- html_node(html, ".post")
      bnow <- html_node(html,".content")
      med <- html_node(html,".graf--p") #Not Working..
      rwhit <- html_node(html,".panel-body")
      ejohn <- html_node(html, ".postentry")
      cbro <- html_node(html, ".post-content")
      cflib <- html_node(html,".entry")
      tmun <- html_node(html, ".entry-wrapper")
      asimp <- html_node(html, ".note") # app.simplenote.com
      dmlc <- html_node(html, ".primary") #digital media + learning Competition
      npr <- html_node(html, ".storytext") #NPR

      if (!is.null(wp))
        {
          #WordPress Blogs
          temptext <- wp %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE) 
          cat("Printing ID:", ID, "\n")
          
        } else if(!is.null(bl))
        {
          #Blogger Blogs
          temptext <- bl %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(ptp))
        {
          #PlayThePast Blog
          temptext <- ptp %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)  
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(bnow))
        {
          #Beth Nowviskie Blog
          temptext <- bnow %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(med))
        {
          #Medium Blog
          temptext <- med %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if(!is.null(rwhit))
        {
          #Roger Whitson Blog
          temptext <- rwhit %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(ejohn))
        {
          #John Resig Blog
          temptext <- ejohn %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(cbro))
        {
          # Blog
          temptext <- cbro %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        }  else if (!is.null(cflib))
        {
          #Code4Library Blog
          temptext <- cflib %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(tmun))
        {
          #Trevor Munoz Blog
          temptext <- tmun %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(asimp))
        {
          #App.Simplenote
          temptext <- asimp %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else if (!is.null(dmlc))
        {
          #Digital Media and Learning
          temptext <- dmlc %>% html_text()
          write(temptext, file = ID, ncolumns=1, append=FALSE)   
          cat("Printing ID:", ID, "\n")
          
        } else
        {
          checkList<- append(checkList, url[i,5])
          cat("CheckList - Adding ", ID, "\n")
        } 
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
}