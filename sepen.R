# libraries to load
library(RSelenium)
library(httr)
library(XML)

# general inputs
# working directories
wd <- 'c:/gergo/sepen'
repo <- "c:/gergo/sepen/repo/"
dl <- 'c:/Users/meresz/Downloads'

setwd(wd)

out <- c('TS=(“health workforce”) AND (TS=(planning) OR TS=(forecasting) OR TS=(data) OR TS=(skills) OR TS=(methodology) OR TS=(demand) OR TS=(supply) OR TS=(doctors) OR TS=(dentists) OR TS=(pharmacists) OR TS=(midwives) OR TS=(nurses) OR TS=(allied health professions))', 
         'TS=(“human resources for health”) AND (TS=(planning) OR TS=(forecasting) OR TS=(data) OR TS=(skills) OR TS=(methodology) OR TS=(demand) OR TS=(supply) OR TS=(doctors) OR TS=(dentists) OR TS=(pharmacists) OR TS=(midwives) OR TS=(nurses) OR TS=(allied health professions))')

#################################
### Download citations module ###
#################################

rD <- rsDriver(browser="chrome")
remDr <- rD$client
url<-"https://apps.webofknowledge.com/WOS_AdvancedSearch_input.do?SID=T1tYqoCJRHpSHjAu9am&product=WOS&search_mode=AdvancedSearch"
remDr$navigate(url)
Sys.sleep(2)
url2 <- gsub("search_mode=GeneralSearch", "search_mode=AdvancedSearch", remDr$getCurrentUrl()[1][[1]])
remDr$navigate(url2)     

for (i in 1:length(out))
{
  #if(i > 1)
  #{
  #  webElem <- remDr$findElement("class", "bselsets")
  #  webElem$clickElement()
  #  webElem <- remDr$findElement("xpath", "//input[@src='https://images.webofknowledge.com/WOKRS5251R3/images/delsets.gif']")
  #  webElem$clickElement()
  #}
  
  Sys.sleep(1)
  webElem <- remDr$findElement("class", "Adv_formBoxesSearch")
  webElem$clearElement()
  webElem$sendKeysToElement(list(out[i]))
  
  webElem2 <- remDr$findElement("id", "periodRange")
  webElem2$clickElement()
  
  webElem3 <- remDr$findElement("name", "startYear")
  
  option <- remDr$findElement(using = 'xpath', "//*/option[@value = '1997']")
  option$clickElement()
  
  webElem4 <- remDr$findElement("class", "searchButton")
  webElem4$clickElement()
  
  webElem5 <- remDr$findElement("id", "set_1_div")
  webElem5$clickElement()
  
  # check if no results
  nohits <- NULL
  nohits <- try(unlist(remDr$findElement(using = 'xpath', "//*/div[@class= 'noHitsMessage']")), silent = T)
  if(class(nohits) != "try-error") {next}
  
  option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'other']")
  option$clickElement()
  
  webElem7 <- remDr$findElement("id", "numberOfRecordsRange")
  webElem7$clickElement()
  
  webElem8 <- remDr$findElement("id", "markFrom")
  webElem8$sendKeysToElement(list("1"))
  
  webElem9 <- remDr$findElement("id", "markTo")
  webElem9$sendKeysToElement(list("500"))
  
  option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HIGHLY_CITED HOT_PAPER OPEN_ACCESS PMID USAGEIND AUTHORSIDENTIFIERS ACCESSION_NUM FUNDING SUBJECT_CATEGORY JCR_CATEGORY LANG IDS PAGEC SABBR CITREFC ISSN PUBINFO KEYWORDS CITTIMES ADDRS CONFERENCE_SPONSORS DOCTYPE ABSTRACT CONFERENCE_INFO SOURCE TITLE AUTHORS  ']")
  option$clickElement()
  
  option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'fieldtagged']")
  option$clickElement()
  
  webElem10 <- remDr$findElement("class", "quickoutput-action")
  webElem10$clickElement()
  
  Sys.sleep(6)
  
  webElem11 <- remDr$findElement("class", "quickoutput-cancel-action")
  webElem11$clickElement()
  
  pg_source     <- GET(remDr$getCurrentUrl()[[1]])
  page_html     <- content(pg_source, as='text', encoding = 'UTF-8')
  page_html     <- htmlTreeParse(page_html, useInternalNodes = T)
  hit_value     <- xpathSApply(page_html, "//span[@id = 'trueFinalResultCount']", function(x) xmlValue(x))
  pg_count      <- ceiling(as.numeric(hit_value)/500)-1
  
  for(j in 1:pg_count)
  {
    option <- remDr$findElement('class', 'saveToButton')
    option$clickElement()
    
    webElem7 <- remDr$findElement("id", "numberOfRecordsRange")
    webElem7$clickElement()
    
    webElem8 <- remDr$findElement("id", "markFrom")
    
    rec_start <- as.character(j*500+1)
    rec_stop <- as.character(as.numeric(rec_start)+499)
    
    if(as.numeric(rec_stop) > as.numeric(hit_value)) {
      rec_stop <- hit_value
    }
    
    webElem8$sendKeysToElement(list(rec_start))
    
    webElem9 <- remDr$findElement("id", "markTo")
    webElem9$sendKeysToElement(list(rec_stop))
    
    option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HIGHLY_CITED HOT_PAPER OPEN_ACCESS PMID USAGEIND AUTHORSIDENTIFIERS ACCESSION_NUM FUNDING SUBJECT_CATEGORY JCR_CATEGORY LANG IDS PAGEC SABBR CITREFC ISSN PUBINFO KEYWORDS CITTIMES ADDRS CONFERENCE_SPONSORS DOCTYPE ABSTRACT CONFERENCE_INFO SOURCE TITLE AUTHORS  ']")
    option$clickElement()
    
    option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'fieldtagged']")
    option$clickElement()
    
    webElem10 <- remDr$findElement("class", "quickoutput-action")
    webElem10$clickElement()
    
    Sys.sleep(5+runif(1))
    
    webElem11 <- remDr$findElement("class", "quickoutput-cancel-action")
    webElem11$clickElement()
    Sys.sleep(2+runif(1))
    
    # check if files had to be cleaned out
    savedrecs_files <- list.files(path = dl, pattern="savedrecs")
    
    if(length(savedrecs_files) == 100) 
    {
      dir_name <- max(as.numeric(list.files(repo, recursive=F, full.names=F, include.dirs=F)), na.rm=T) + 1
      dir.create(file.path(repo, dir_name))
      setwd(dl)
      file.copy(savedrecs_files, paste0(repo, dir_name))
      write(out[i], paste0(repo, dir_name, "/search_term.txt"), append=T)
      file.remove(savedrecs_files)
    }
  }
  
  dir_name <- max(as.numeric(list.files(repo, recursive=F, full.names=F, include.dirs=F)), na.rm=T) + 1
  dir.create(file.path(repo, dir_name))
  setwd(dl)
  file.copy(savedrecs_files, paste0(repo, dir_name))
  file.remove(savedrecs_files)
  
  rD[["server"]]$stop()
  rm(rD)
  rm(remDr)
  rD <- rsDriver(browser="chrome")
  remDr <- rD$client
  url<-"https://apps.webofknowledge.com/WOS_AdvancedSearch_input.do?SID=T1tYqoCJRHpSHjAu9am&product=WOS&search_mode=AdvancedSearch"
  remDr$navigate(url)
  Sys.sleep(2)
  url2 <- gsub("search_mode=GeneralSearch", "search_mode=AdvancedSearch", remDr$getCurrentUrl()[1][[1]])
  remDr$navigate(url2)
  
  write(out[i], paste0(repo, dir_name, "/search_term.txt"))
}


library(bibliometrix)
library(dplyr)

setwd(paste0('c:/gergo/sepen/repo'))
txt_file_list <- list.files(pattern = "savedrecs", recursive=T)

out <- NULL

for (i in txt_file_list)
{
  current <- convert2df(readFiles(i), dbsource = "isi", format = "plaintext")
  current$search_term <- readLines(paste0(strsplit(i, "/")[[1]][1], "/", "search_term.txt"))
  out <- bind_rows(out, current)
}

write.table(out, file = "wos_data_20180302.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "double",
            fileEncoding = "")

out <- out[,-which(colnames(out) %in% "search_term")]

write.table(unique(out), file = "unique_wos_data_20180302.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "double",
            fileEncoding = "")

