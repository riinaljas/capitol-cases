library(tidyverse)
library(rvest)
library(utilities)
library(pdftools)
library(tidytext)
library(unpivotr)
library(foreach)


myurl <- "https://www.justice.gov/usao-dc/capitol-breach-cases"

data <- read_html(myurl) %>%
  html_nodes('a')

emptydf <- data.frame(link = as.character())

for (x in 1:length(data)) {
  temp <- as.character(data[x]) %>%
    as.data.frame() %>%
    rename(link = 1)
  emptydf <- bind_rows(emptydf, temp)
}

data <- read_html(myurl) %>%
  html_nodes('a') %>%
  html_text() %>%
  as.data.frame() %>% 
  rename(name = 1)


links_and_names <- bind_cols(emptydf, data) %>%
  mutate(link = str_extract_all(link, '".*"'))

downloadfiles <- links_and_names %>% 
  filter(str_detect(link, "download")) %>% 
  mutate(link = paste0("https://www.justice.gov",
                       str_extract_all(link, '\\/.*download')))
downloadfiles <- downloadfiles %>% 
  mutate(id = rownames(downloadfiles))
                    
## download files---- 

folder <- 'complaints/'

# get all files in the directories, recursively
f <- list.files(folder, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)
foreach(myfile = downloadfiles$link, myid = downloadfiles$id) %do% {
  
  tempname = downloadfiles %>% 
    filter(link == myfile) %>% 
    select(name) %>%
    mutate(name = ifelse(str_detect(name, "et al|& Ashlock|Second Superceding Indictment")==T,
                         str_extract(name, "(.*?) et al|& Ashlock|Superceding"), 
                         name)
             
    ) %>% 
    mutate(name = paste0("id", myid, "_", name))
  #write id-s to filenames for later joins
                    
  download.file(myfile, 
                      paste0("complaints/", tempname, ".pdf"), "curl", quiet = FALSE, 
                      cacheOK = TRUE,
                      extra = getOption("download.file.extra"))
  print(downloadfiles %>% 
          filter(link == myfile) %>% 
          select(id))
}





## turn files into text mass-----

myfiles <- list.files("complaints", pattern = "pdf$")

myfiles <- paste0("complaints/", myfiles) %>% 
  as.data.frame() %>% 
  rename(file = 1)
#pdf_texts <- lapply(myfiles, pdf_text)
## create a large df----

## create keywords----
keywords = "fuentes|extremist|supremacy|russia|germany|russian imperial movement|imperial| rim |identitarian|great replacement|generation identity|identitarianist|accelerationism|accelerationist|europe|europeans|europe|international|three percent|patriot front|radical|ukraine|france|french|qanon|azov|military|paramilitary|ohiomilitia|extreme|extremist|nazi|identity movement|generation|atomwaffen|aryan renaissance society|awd|battalion|black pigeon speaks|blood & honour|blue awakening|boogaloo|british|combat 18|daily Stormer|defend evropa|holy land foundation|generation evropa|movement against illegal migration|national action|national socialist underground|nordic resistance movement|order of nine angles|patriotic europeans against the islamization of the west|rise above movement| r.a.m. | ram |the base |sonnenkrieg division|feuerkrieg fivision| fkd |american renaissance|countercurrents|groyper|altright corporation|stanza|awakening|polish national day|american renaissance conference|financed|funded|funding|donation|alt right|far right"
intkeywords = "abroad|foreign|russia|germany|russian imperial movement|imperial| rim |identitarian|great replacement|generation identity|identitarian|acceleration|europe|european|ukraine|france|french|azov|identity movement|generation|atomwaffen|aryan renaissance society|awd|battalion|black pigeon speaks|blood & honour|blue awakening|boogaloo|british|combat 18|daily Stormer|defend evropa|holy land foundation|generation evropa|movement against illegal migration|national action|national socialist underground|nordic resistance movement|order of nine angles|patriotic europeans against the islamization of the west|rise above movement| r.a.m. | ram |the base |sonnenkrieg|feuerkrieg| fkd |american renaissance|countercurrents|groyper|altright corporation|stanza|awakening|polish national day|american renaissance conference|financed|funded|funding|donation|international airport|investigate international and domestic terrorism|international|fundrais"
symbolkeywords <- " race |racial|nsc-131|nsc131|symbol|auschwitz|6mwe|six million|6 million|jew| race |kekistan|frog|vietnam|confederacy|vdare"
susannekeywords <- "molotov|gun|guns|zip ties|taser|pitchfork| bear spray |spray|pole|fire|extinguisher|weapon|armed|gear|stick|firearm|club|axe|handle"
race <- " race | racial |far-right|alt-right|fuentes|proud boys|oath keepers|three percent|patriot front|extreme"

textdf <- data.frame(page = as.character(), 
                     name = as.character(), 
                     has_keyword = as.character(), 
                     keyword = as.character()) 


for (myfile in myfiles$file){
  temp <- pdf_text(myfile) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(page = 1) %>% 
    mutate(page = page %>% 
             tolower() %>% 
             trimws(which = c("both")) %>% 
             str_replace_all("\n", " ") %>% 
             as.character(),
           name = myfile,
           has_keyword = ifelse(str_detect(page, keywords)==T, 
                                "YES",
                                "NO"),
           keyword = str_match_all(page, keywords) %>% 
             as.character()
    )
  print(myfile)
    
    textdf <- bind_rows(textdf, temp)
    
}

of_interest <- textdf %>% 
  filter(has_keyword == "YES") %>% 
  count(name, keyword) %>% 
  select(-n) %>% 
  mutate(person = str_extract(name, "(.*?) -") %>% 
           str_remove_all("complaints/") %>% 
           str_remove_all(" -")) %>% 
  mutate(keyword = keyword %>% 
           str_remove_all('c\\(\\"') %>% 
           str_remove_all('\\)'),
         person = person %>% 
           str_remove_all('c\\(\\"') %>% 
           str_remove_all('\\)')) %>% 
  filter(!keyword %in% c("investigate international and domestic terrorism", "international airport", "terrace"))

latest_same_file <- read_csv(
  dir(pattern = "keywords_")
)
test <- of_interest %>% 
  mutate(name = name %>% 
           str_remove_all("(id)\\d\\d\\d_|(id)\\d\\d_")
  )
is_updated <- test %>% 
  anti_join(latest_same_file, by = "name") %>% 
  mutate(update_date = Sys.Date())

is_not_updated <-test %>% 
  anti_join(is_updated, by = "name") %>% 
  mutate(update_date = str_extract_all(dir(pattern = "keywords_"),
                                       "\\d\\d\\d\\d-\\d\\d-\\d\\d") %>% 
           unlist() %>%
           as.Date())
of_ineterest <- bind_rows(is_updated, is_not_updated)

f <- dir(pattern = "keywords_")
# remove the files
file.remove(f)

f <- list.files(folder, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)


write_csv(of_interest, paste0("keywords_", Sys.Date()))

