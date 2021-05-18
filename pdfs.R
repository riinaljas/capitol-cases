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
symbolkeywords <- "racial|nsc-131|nsc131|symbol|auschwitz|6mwe|six million|6 million|jew| race |kekistan|frog|vietnam|confederacy|vdare"
susannekeywords <- "molotov|gun|guns|zip ties|taser|pitchfork| bear spray |spray|pole|fire|extinguisher|weapon|armed|gear|stick|firearm|club|axe|handle"

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


errorfiles <- myfiles %>% 
  anti_join(textdf)

race <- " race | racial |far-right|alt-right|fuentes|proud boys|oath keepers|three percent|patriot front|extreme"

of_interest2 <- textdf %>% 
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

symbolkeywords <- "racial|nsc-131|nsc131|symbol|auschwitz|6mwe|six million|6 million|jew| race |kekistan|frog|vietnam|confederacy|vdare"
write_csv(of_interest, paste0("keywords_", Sys.Date()))

of_interest2 %>% anti_join(read_csv("keywords_2021-04-06")) %>% unique() %>% view()

#test <- myfiles[8]
downloadfiles <- downloadfiles %>% 
  mutate(rank = rownames(downloadfiles))


### susanne keywords -----


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
           has_keyword = ifelse(str_detect(page, susannekeywords)==T, 
                                "YES",
                                "NO"),
           keyword = str_match_all(page, susannekeywords) %>% 
             as.character()
    )
  print(myfile)
  
  textdf <- bind_rows(textdf, temp)
  
}

of_sus_int <- textdf %>% 
  filter(has_keyword == "YES") %>% 
  count(name, keyword) %>% 
  select(-n) %>% 
  mutate(person = str_extract(name, "(.*?) -") %>% 
           str_remove_all("complaints/") %>% 
           str_remove_all(" -")) %>% 
  mutate(keyword = keyword %>% 
           str_replace_all('c\\(|\\)|\\"', ""))

of_sus_int <- of_sus_int %>% 
  mutate(keyword = keyword %>% 
           str_remove_all('"|[,]') %>% 
           trimws())

of_sus_int <- of_sus_int %>% 
  mutate(split = str_split(keyword, " ")) %>% 
  mutate(split = map(.$split, ~ unique(.x))) %>% 
  mutate(split = map_chr(.$split, ~paste(.x, collapse = " "))) %>%
  mutate(keyword = split %>% 
           str_replace_all(" ", ", ")) %>% 
  select(-split) %>% 
  unique()

cols = paste0("columname", c(1:7)) %>% as.character()

of_sus_int  <- of_sus_int %>% 
  separate(keyword, into = cols) %>% unique() %>%
  pivot_longer(c(2:8), names_repair = "unique") %>% 
  select(-2, -3) %>% 
  filter(!is.na(value)) %>% 
  unique()


of_sus_int <- of_sus_int %>% 
  rename(keyword = value, 
         link = 1) 
  
test <- of_sus_int %>% 
  pivot_wider(values_from = keyword, names_from = link) %>% 
  pivot_longer(everything()) %>% 
  rename(keyword = value) %>% 
  mutate(keyword = keyword)

#take out periods to make it easier to take out sentences
periodwords <- c("U\\.S\\." = "US", 
                 "p\\.m\\." = "pm", 
                 "a\\.m\\.", "am")
                 
x <- pdf_text("complaints/Abual-Ragheb - Affidavit.pdf") %>% 
  as.tibble() %>% 
  rename(page = 1)

x$page <- str_replace_all(x$page, c("U\\.S\\." = "US", 
                                 "p\\.m\\." = "pm", 
                                 "a\\.m\\." = "am", 
                                 "d\\.c\\." = "dc"))
  
### extract sentences with words in them with page numbers 

]
               
my_loop_function <- function(name, keyword){
  df_keyword = keyword %>% 
    unlist()
  print("managed first step")
  
  x <- pdf_text(name) %>% 
    as.tibble() %>% 
    rename(content = 1)
  x$content <- str_replace_all(x$content, c("U\\.S\\." = "US", 
                                      "p\\.m\\." = "pm", 
                                      "a\\.m\\." = "am", 
                                      "d\\.c\\." = "dc"))
  x$pagenr <- rownames(x)
  print("managed second step")
  y <- x %>% 
    unnest_sentences(sentences, content)
  print("fourth step")
  y <- y %>% 
    mutate(key_sentence = ifelse(str_detect(sentences, df_keyword)==T, 
                                 sentences,
                                 NA)) %>% 
    filter(!is.na(key_sentence)) %>% 
    select(pagenr, key_sentence) %>% 
    mutate(keyword = keyword %>% 
             as.character() %>% 
             str_remove("c") %>% 
             str_remove_all('"|\\(|\\)'), 
           name = name)
  print("fifth step")
}

library(foreach)

keysentences = data.frame(pagenr = as.character(), 
                          key_sentence = as.character(), 
                          keywords = as.character(), 
                          file_name = as.character())

foreach(name = test$name , keyword = test$keyword) %do%
  {
    df_keyword = keyword %>% 
    unlist()
  print("managed first step")
  
  x <- pdf_text(name) %>% 
    as.tibble() %>% 
    rename(content = 1)
  x$content <- str_replace_all(x$content, c("U\\.S\\." = "US", 
                                            "p\\.m\\." = "pm", 
                                            "a\\.m\\." = "am", 
                                            "d\\.c\\." = "dc"))
  x$content <- x$content %>% str_squish() %>% trimws()
  x$pagenr <- rownames(x)
  print("managed second step")
  y <- x %>% 
    unnest_sentences(sentences, content) %>% 
    mutate(key_sentence = ifelse(str_detect(sentences, df_keyword)==T, 
                                 sentences,
                                 NA) %>% 
             str_squish()) %>% 
    filter(!is.na(key_sentence)) %>% 
    select(pagenr, key_sentence) %>% 
    mutate(keywords = paste( unlist(keyword), collapse=', '),
           file_name = name)
  keysentences <- bind_rows(keysentences, y)
  print(name)
  }

missing <- anti_join(test %>% mutate(id = rownames(test)), keysentences %>% 
                       mutate(name=file_name))

foreach(name = missing$name , keyword = missing$keyword) %do%
  {
    df_keyword = keyword %>% 
      unlist()
    print("managed first step")
    
    x <- pdf_text(name) %>% 
      as.tibble() %>% 
      rename(content = 1)
    x$content <- str_replace_all(x$content, c("U\\.S\\." = "US", 
                                              "p\\.m\\." = "pm", 
                                              "a\\.m\\." = "am")) 
    x$content <- x$content %>% str_squish() %>% trimws()       
    x$pagenr <- rownames(x)
    print("managed second step")
    pattern = paste(unlist(keyword), collapse='|')
    y <- x %>% 
      unnest_sentences(sentences, content) %>% 
      mutate(key_sentence = ifelse(str_detect(sentences, pattern) == T, 
                                  sentences,
                                   NA) %>% 
               str_squish()) %>% 
      filter(!is.na(key_sentence)) %>% 
      select(pagenr, key_sentence) %>% 
      mutate(keywords = paste( unlist(keyword), collapse=', '),
             file_name = name)
    keysentences <- bind_rows(keysentences, y)
    print(name)
  }

keysentences <- keysentences %>%
  mutate(id = str_extract(file_name, "id...") %>% 
           str_remove_all('_|id')) %>% 
  left_join(downloadfiles) %>% 
  select(-file_name)

keysentences <- keysentences %>% 
  mutate(key_sentence = key_sentence %>% 
           trimws()) %>% 
  select(-id)

cols = c(1:8) %>% as.character()
keysentences <- keysentences %>% 
  separate(keywords, cols, sep = " ") %>% 
  unique() %>% 
  pivot_longer(c(3:10), names_to = "keyword") %>% 
  select(-keyword) %>% 
  unique() %>% 
  filter(!is.na(value)) %>% 
  mutate(value = value %>% 
           str_remove_all("[,]")) %>% 
  mutate(value = ifelse(str_detect(key_sentence, value)==T, 
                        value, 
                        NA)) %>% 
  filter(!is.na(value)) %>% 
  rename(keyword=value) %>% 
  pivot_wider(names_from = keyword, values_from = keyword)

mycols = names(keysentences %>% select(5:21))

keysentences$keywords <- apply( keysentences[ , mycols ] , 1 , paste , collapse = ", " ) %>% 
  str_remove_all(", NA|NA,|NA") %>% 
  trimws()


keysentences <- keysentences %>% 
  select(-c(5:21))


  
keysentences <- keysentences %>% 
  filter(!str_detect(key_sentence, "polesi|sticking to|panhandle|in the county of |milsim|being fired|was fired|getting fired|i am currently assigned|unzipped|begun|twitter handle|facebook handle|instagram handle|have investigated|as a special agent|gundersen|did knowingly, and with intent|swarmed|republican club|filed|the damage included|the capitol suffered millions|bringing andcntributing|the capitol suffered millions of dollars|fbi agent|additionally, one subject was shot and killed|violation of|§|the crowd was not lawfully authorized to enter|in light of the dangerous circumstances|entering and remaining in a restricted|engaging in physical violence in a restricted|disorderly and disruptive conducts|resisting, or impeding|civil disorder"))

cleaned_riin <- read_csv('riin.csv')
### - cleaned out manually in excel


cleaned_riin <- cleaned_riin %>% 
  select(-X7) %>% 
  rename(stands_out = X2)

write_csv(cleaned_riin, "cleaned_riin2.csv")
