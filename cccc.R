library(tidyverse)
library(tidytext)

# functions

parse_presenter <- function(presenter_text) {
  pres_split <- strsplit(presenter_text, ', ') %>% unlist()
  title_split <- strsplit(presenter_text, '\"') %>% unlist()
  if (length(strsplit(pres_split[1], ': ') %>% unlist()) > 1) {
    presenter_name <- strsplit(pres_split[1], ': ')[[1]][2]
  } else {
    presenter_name <- pres_split[1]
  }
  presenter_affiliation = pres_split[2]
  if (length(title_split) > 1) {
    presentation_title <- title_split[2]
  } else {
    presentation_title <- NA
  }
  return(cbind(presenter_name, presenter_affiliation, presentation_title) %>% as_tibble())
}

combine_pres <- function(pres, next_pres) {
  if (length(strsplit(pres, ', ') %>% unlist()) > 1 & length(strsplit(next_pres, ', ') %>% unlist()) == 1) {
    return(paste(pres, next_pres))
  } else if (length(strsplit(pres, ', ') %>% unlist()) == 1) {
    if (length(strsplit(next_pres, ', ') %>% unlist()) > 1) {
      return(paste(pres, next_pres, sep = ', '))
    } else {
    return(NA)
    }
  } else {
    return(pres)
  }
}

combine_content <- function(value, next_value) {
  if (grepl('continued on next page$', value)) {
    return(paste(value, next_value) %>% gsub('continued on next page', '', .))
  } else {
    return(value)
  }
}

parse_session <- function(text_line) {
  content <- text_line %>% 
    as.character() %>%
    strsplit('\n') %>%
    unlist() %>%
    gsub('^Capri \\d{2,3}.*', '\n', .) %>%
    gsub('^Royale Pavilion \\d{1,3}.*', '\n', .) %>%
    gsub('^Skybox \\d{1,3}.*', '\n', .) %>%
    gsub('^(Grand|Grande) Ballroom .*', '\n', .) %>%
    gsub('.*Monaco Tower.*', '\n', .) %>%
    paste(collapse = '\n') %>%
    strsplit('\n\n') %>%
    unlist() %>%
    as_tibble() %>%
    filter(!is.na(value),
           nchar(value) > 5,
           !grepl('^\\f', value),
           !grepl('\\d{1,2}\\/\\d{1,2}\\/\\d{1,2}', value),
           !grepl('^CCCC CONVENTION, ', value),
           !grepl('^\\d{1,3}', value),
           !grepl('indd \\d{1,3}$', value)) %>%
    mutate(next_value = lead(value),
           info = mapply(combine_content, value, next_value),
           next_info = lead(info),
           final_info = mapply(combine_content, info, next_info)) %>%
    select(value = final_info)
  session_title <- content[1,1] %>% 
    gsub('\t', '', .) %>% 
    gsub('\n', ' ', .) %>% 
    gsub('^\\s', '', .)
  if (length(content$value) == 1) {
    session_description <- NA
    pres_content <- NA
  } else if (length(content$value) == 2) {
    session_description <- NA
    pres_content <- content[2,1]
  } else {
    if (grepl('Chair$', strsplit(content[2,1] %>% as.character(), ':\\s')[[1]][1] %>% gsub('\n', '', .)) |
        grepl('Speakers$', strsplit(content[2,1] %>% as.character(), ':\\s')[[1]][1] %>% gsub('\n', '', .)) |
        grepl('Co-Chairs$', strsplit(content[2,1] %>% as.character(), ':\\s')[[1]][1] %>% gsub('\n', '', .)) |
        grepl('Leaders$', strsplit(content[2,1] %>% as.character(), ':\\s')[[1]][1] %>% gsub('\n', '', .))) {
      session_description <- NA
      pres_content <- content[2,1]
    } else {
      session_description <- content[2,1] %>%
        gsub('\t', '', .) %>%
        gsub('\n', ' ', .) %>%
        gsub('^\\s', '', .)
      if (length(content$value) > 3 &
          !grepl(':', content$value[3])) {
        pres_content <- content[4,1]
      } else {
        pres_content <- content[3,1]
      }
      
    }
  }
  if (!is.na(pres_content)) {
    session_presenters <- pres_content %>% 
      gsub('\t', '', .) %>% 
      gsub('^\\s', '', .) %>%
      strsplit('\n') %>%
      unlist() %>%
      as_tibble() %>%
      filter(!grepl('^[A-Z]\\d{1,3}', value),
             !grepl('^\\d{1,2}-[A-Z]', value)) %>%
      mutate(next_pres = lead(value),
             presenter = mapply(combine_pres, value, next_pres)) %>%
      filter(!is.na(presenter))
    session_table <- rbind(t(mapply(parse_presenter, session_presenters$presenter))) %>% 
      as_tibble() %>% 
      unnest() %>%
      mutate(session_title = session_title,
             session_description = session_description)
  } else {
    session_table <- cbind(presenter_name = NA,
                           presenter_affiliation = NA,
                           presentation_title = NA,
                           session_title = session_title,
                           session_description = session_description)
  }
  return(session_table)
}


# import data

cccc <- readLines('2013program.txt')
cccc_raw_parse <- strsplit(paste(cccc, collapse = '\n'), '[A-Z]{1,2}\\.\\d{1,2}')[[1]] # parse by enumerated session

cccc_sessions <- cccc_raw_parse %>%
  as_tibble() %>%
  filter(nchar(value) > 0) %>%
  mutate(next_value = lead(value),
         l = nchar(value)) %>%
  filter(!is.na(value),
         !is.na(next_value))


# process imported data

cccc_sessions_parsed <- as_tibble(data.frame())

for (i in 1:length(cccc_sessions$value)) {
  print(i)
  cccc_sessions_parsed <- rbind(cccc_sessions_parsed,
                               parse_session(cccc_sessions[i,1]))
}


# test

text_line <- cccc_sessions[114,1]


# combine programs

cccc2014 <- cccc_sessions_parsed

cccc_all <- cccc_all %>%
  full_join(cccc2014 %>%
              mutate(year = 2014))


write_csv(cccc2015, 'cccc2015_parsed.csv')
write_csv(cccc_all, 'cccc_programs_parsed.csv')
cccc_all <- read_csv('cccc_programs_parsed.csv')


# add summary statistics

cccc <- cccc_all %>%
  group_by(year) %>%
  mutate(total_presentations = sum(!is.na(presentation_title)),
         total_sessions = sum(!is.na(unique(session_title)))) %>%
  ungroup() 

# analyze results


cccc_title_words <- cccc %>%
  unnest_tokens(word, presentation_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word)) %>%
  group_by(year) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup() 


cccc_title_words %>%
  group_by(word) %>%
  filter(!is.na(word)) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

cccc_session_words <- cccc %>%
  select(session_title, year) %>%
  unique() %>%
  unnest_tokens(word, session_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word)) %>%
  group_by(year) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup()  

cccc_all %>%
  select(session_title, year) %>%
  unique() %>%
  filter(grepl('pedagogy', tolower(session_title)) |
           grepl('teaching', tolower(session_title))) %>%
  count(year, sort=TRUE)

cccc_words_ranked <- cccc_title_words %>%
  group_by(word) %>%
  filter(!is.na(word)) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

cccc_session_words %>%
  group_by(word, year, annual_words) %>%
  summarize(count = n()) %>%
  mutate(share = count/annual_words) %>%
  # filter(word %in% cccc_words_ranked$word[1:10]) %>%
  filter(word %in% c('adjunct', 'classroom', 'teaching', 'contingent',
                     'adjuncts', 'pedagogy')) %>%
  ggplot(aes(year, share, color = word)) +
  geom_line() +
  xlab('Year') + 
  ylab('Word count in presentation titles') +
  ggtitle('Word counts for popular words in CCCC presentation titles')

cccc_title_words %>%
  group_by(word, year, annual_words) %>%
  summarize(count = n()) %>%
  mutate(share = count/annual_words) %>%
  # filter(word %in% cccc_words_ranked$word[1:10]) %>%
  filter(word %in% c('adjunct', 'classroom', 'teaching', 'contingent',
                     'adjuncts', 'pedagogy')) %>%
  ggplot(aes(year, share, color = word)) +
  geom_line() +
  xlab('Year') + 
  ylab('Perecentage of words in presentation titles') +
  ggtitle('Word share for selected words in CCCC presentation titles')


cccc_title_bigrams <- cccc_all %>%
  filter(year == 2016) %>%
  unnest_tokens(bigram, presentation_title, token = 'ngrams', n=2) %>%
  separate(bigram, c('word', 'next_word'), sep = " ") %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word) %>% 
  unite(bigram, word, next_word, sep = ' ')

cccc_title_bigrams %>%
  group_by(bigram) %>%
  filter(!is.na(bigram)) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


cccc_all %>%
  filter(!is.na(presenter_affiliation)) %>%
  group_by(presenter_affiliation) %>%
  summarize(count = n()) %>%
  arrange(desc(count))




# deep learning
# use gensim

library(wordVectors)

write_lines(cccc_all[!is.na(cccc_all$presentation_title),]$presentation_title, 'cccc_titles.csv')
write_lines(cccc_all$session_title %>% unique(), 'cccc_session_titles.csv')

if (!file.exists("cccc_session_titles.csv")) {
  prep_word2vec(origin="./",
                destination="cccc_session_titles.csv",
                lowercase=T,
                bundle_ngrams=2)
}

if (!file.exists("cccc_session_vectors.bin")) {
  model = train_word2vec("cccc_session_titles.csv",
                         "cccc_session_vectors.bin",
                         vectors=200,
                         threads=4,
                         window=12,
                         iter=5,
                         negative_samples=0)
} else {
  model = read.vectors("cccc_session_vectors.bin")
}

model %>% closest_to('teacher')


set.seed('10')
centers = 150
clustering = kmeans(model, centers=centers,iter.max = 40)

sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

tastes = model[[c('research', 'adjunct'),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
ped = model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 20 sweet or salty.
ped = ped[
  rank(-ped[,1])<20 |
    rank(-ped[,2])<20,
  ]

plot(ped,type='n')
text(ped,labels=rownames(ped))
