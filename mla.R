library(tidyverse)
library(tidytext)
library(xml2)
library(stringr)

# functions

session_length <- function(details_xml) {
  return(details_xml %>% xml_children() %>% length() %>% unlist())
}

extract_title <- function(entry) {
  return(unlist(strsplit(gsub('^\\d. ', '', entry), '," '))[1] %>% gsub('^\"', '', .))
}

extract_institution <- function(entry) {
  return(unlist(strsplit(unlist(strsplit(entry, '," '))[2], ', '))[2])
}

extract_speaker <- function(entry) {
  return(unlist(strsplit(unlist(strsplit(entry, '," '))[2], ', '))[1])
}

extract_special_institution <- function(entry) {
  return(unlist(strsplit(gsub('Speakers{0,1}: ', '', entry), ', '))[2])
}

extract_special_speaker <- function(entry) {
  return(unlist(strsplit(gsub('Speakers{0,1}: ', '', entry), ', '))[1])
}



session_content <- function(details_xml, session_title) {
  text_as_list <- details_xml %>% xml_children() %>% xml_text() %>% unlist()
  
  if (text_as_list %>% 
        as_tibble() %>% 
        filter(grepl('^(\\d. "|")', value)) %>%
        nrow() > 0) {
    titles <- text_as_list %>% 
      as_tibble() %>% 
      filter(grepl('^(\\d. "|")', value)) %>%
      mutate(presentation_title = sapply(value, extract_title),
             speaker = sapply(value, extract_speaker),
             institution = sapply(value, extract_institution)
      ) %>%
      select(-value)
  } else if (text_as_list %>% 
             as_tibble() %>% 
             filter(grepl('^Speakers{0,1}:', value)) %>%
             nrow() > 0) {
    titles <- text_as_list %>% 
      as_tibble() %>% 
      filter(grepl('^Speakers{0,1}:', value)) %>%
      as.character() %>%
      strsplit('; ') %>%
      unlist() %>%
      as_tibble() %>%
      mutate(presentation_title = NA,
             speaker = sapply(value, extract_special_speaker),
             institution = sapply(value, extract_special_institution)) %>%
      select(-value)
  } else {
    titles <- tibble(presentation_title = NA,
                     speaker = NA,
                     institution = NA)
  }
  
  time = strsplit(text_as_list[1], ', ')[[1]][1]
  room = strsplit(text_as_list[1], ', ')[[1]][2]
  building = strsplit(text_as_list[1], ', ')[[1]][3]
  
  return(titles %>%
           mutate(session_title = session_title,
                  time = time,
                  room = room,
                  building = building))
}


parse_program <- function(program_xml, conf_year) {
  details <- program_xml %>% 
    xml_find_all('//details')
  
  session_titles <- program_xml %>% 
    xml_find_all('//session') %>% xml_find_all('//title') %>% xml_text()
  
  content <- tibble()
  
  for (i in 1:length(details)) {
    content <- rbind(content, session_content(details[i], session_titles[i]))
  }
  
  return(content %>% mutate(year = conf_year))
}

# import and merge data

content2014 <- parse_program(read_xml('mla_source/2014_program.xml'), 2014)
content2015 <- parse_program(read_xml('mla_source/2015_program.xml'), 2015)
content2016 <- parse_program(read_xml('mla_source/2016_program.xml'), 2016)


mla <- bind_rows(content2014,
                 content2015,
                 content2016) %>%
  mutate(society = 'MLA')

write_csv(mla, 'parsed_programs/mla_programs_parsed.csv')

# summary statistics

mla %>%
  filter(!is.na(institution)) %>%
  count(institution, sort = TRUE)

words <- mla %>%
  unnest_tokens(word, presentation_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word),
         !word %in% c('de', 'la', 'univ')) 

word_count <- words %>%
  group_by(year) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup() %>%
  group_by(word, year, annual_words) %>%
  summarize(count = n()) %>%
  mutate(annual_share = count/annual_words)

session_words <- mla %>%
  unnest_tokens(word, session_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word),
         !word %in% c('de', 'la', 'univ')) 

session_word_count <- words %>%
  group_by(year) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup() %>%
  group_by(word, year, annual_words) %>%
  summarize(count = n()) %>%
  mutate(annual_share = count/annual_words)

bigrams <- mla %>%
  unnest_tokens(bigram, presentation_title, token = 'ngrams', n=2) %>%
  separate(bigram, c('word', 'next_word'), sep = " ") %>%
  filter(!word %in% c(stop_words$word, 'de', 'la', 'univ'), # remove stop words
         !next_word %in% c(stop_words$word, 'de', 'la', 'univ')) %>% 
  unite(bigram, word, next_word, sep = ' ') 

bigram_count <- bigrams %>%
  group_by(year) %>%
  mutate(annual_bigrams = sum(!is.na(bigram))) %>%
  ungroup() %>%
  group_by(bigram, year, annual_bigrams) %>%
  summarize(count = n()) %>%
  mutate(annual_share = count/annual_bigrams)

session_bigrams <- mla %>%
  unnest_tokens(bigram, session_title, token = 'ngrams', n=2) %>%
  separate(bigram, c('word', 'next_word'), sep = " ") %>%
  filter(!word %in% c(stop_words$word, 'de', 'la', 'univ'), # remove stop words
         !next_word %in% c(stop_words$word, 'de', 'la', 'univ')) %>% 
  unite(bigram, word, next_word, sep = ' ') 

session_bigram_count <- bigrams %>%
  group_by(year) %>%
  mutate(annual_bigrams = sum(!is.na(bigram))) %>%
  ungroup() %>%
  group_by(bigram, year, annual_bigrams) %>%
  summarize(count = n()) %>%
  mutate(annual_share = count/annual_bigrams)


mla %>%
  filter(!is.na(speaker)) %>%
  count(speaker, sort = TRUE)



word_count %>%
  filter(word %in% c('adjunct', 'classroom', 'teaching', 'contingent',
                     'adjuncts', 'pedagogy')) %>%
  ggplot(aes(year, annual_share, color = word)) +
  geom_line() +
  xlab('Year') + 
  ylab('Word share in presentation titles') +
  ggtitle('Word count share for popular words in MLA presentation titles')
