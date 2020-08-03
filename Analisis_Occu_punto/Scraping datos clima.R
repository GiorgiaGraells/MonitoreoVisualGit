#SCRAPING PARA OBTENER DATOS DE CLIMA EN METEORED.CL


################################
#Ejemplo tutorial

#Scraping con phantomjs //web con javascript

library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)

# Let phantomJS scrape techstars, output is written to techstars.html

system("./phantomjs scrape_techstars.js")

batches <- html("techstars.html") %>%   html_nodes(".batch")

class(batches) #crea una lista con la información de la web


batch_titles <- batches %>%   html_nodes(".batch_class") %>%   html_text()

batch_season <- str_extract(batch_titles, "(Fall|Spring|Winter|Summer)")
batch_year <- str_extract(batch_titles, "([[:digit:]]{4})")


# location info is everything in the batch title that is not year info or season info
batch_location <- sub("\\s+$", "",
                      sub("([[:digit:]]{4})", "",
                          sub("(Fall|Spring|Winter|Summer)","",batch_titles)))


# create data frame with batch info.
batch_info <- data.frame(location = batch_location,
                         year = batch_year,
                         season = batch_season)

breakdown <- lapply(batches, function(x){
  company_info <- x %>% html_nodes(".parent")
  companies_single_batch <- lapply(company_info, function(y){
    as.list(gsub("\\[\\+\\]\\[\\-\\]\\s", "", y %>%
                   html_nodes("td") %>%
                   html_text()))
  })
  
  df <- data.frame(matrix(unlist(companies_single_batch),
                          nrow=length(companies_single_batch),
                          byrow=T,
                          dimnames = list(NULL, c("company","funding","status","hq"))))
  return(df)
})


# Add batch info to breakdown
batch_info_extended <- batch_info[rep(seq_len(nrow(batch_info)),
                                      sapply(breakdown, nrow)),]
breakdown_merged <- rbind.fill(breakdown)

# Merge all information
techstars <- tbl_df(cbind(breakdown_merged, batch_info_extended)) %>%
  mutate(funding = as.numeric(gsub(",","",gsub("\\$","",funding))))

##########################################################################

###### scraping por base meteorológica para los tres aeropuertos


library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)

# Let phantomJS scrape techstars, output is written to techstars.html

system("./phantomjs scrape_techstars.js")

batches <- html("https://www.meteored.cl/tiempo-en_Valparaiso-America+Sur-Chile-Valparaiso--sactual-18577.html") %>%   html_nodes(".batch")

class(batches) #crea una lista con la información de la web


batch_titles <- batches %>%   html_nodes(".batch_class") %>%   html_text()

batch_airport <- str_extract(batch_titles, "(Aeropuerto RODELILLO (SCRD)|Aeropuerto RODELILLO (SCRD)|Aeropuerto QUINTERO (SCER))")
batch_year <- str_extract(batch_titles, "([[:digit:]]{4})")


# location info is everything in the batch title that is not year info or season info
batch_location <- sub("\\s+$", "",
                      sub("([[:digit:]]{4})", "",
                          sub("(Fall|Spring|Winter|Summer)","",batch_titles)))


# create data frame with batch info.
batch_info <- data.frame(location = batch_location,
                         year = batch_year,
                         season = batch_season)

breakdown <- lapply(batches, function(x){
  company_info <- x %>% html_nodes(".parent")
  companies_single_batch <- lapply(company_info, function(y){
    as.list(gsub("\\[\\+\\]\\[\\-\\]\\s", "", y %>%
                   html_nodes("td") %>%
                   html_text()))
  })
  
  df <- data.frame(matrix(unlist(companies_single_batch),
                          nrow=length(companies_single_batch),
                          byrow=T,
                          dimnames = list(NULL, c("company","funding","status","hq"))))
  return(df)
})


# Add batch info to breakdown
batch_info_extended <- batch_info[rep(seq_len(nrow(batch_info)),
                                      sapply(breakdown, nrow)),]
breakdown_merged <- rbind.fill(breakdown)

# Merge all information
techstars <- tbl_df(cbind(breakdown_merged, batch_info_extended)) %>%
  mutate(funding = as.numeric(gsub(",","",gsub("\\$","",funding))))

