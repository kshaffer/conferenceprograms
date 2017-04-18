library(tidyverse)
library(tidytext)

# functions

combine_sessions <- function(first, second) {
  if (substring(strsplit(first, '\\(')[[1]][2], 1, 3) %in% c('AMS', 'SMT', 'SEM') &
      substring(strsplit(second, '\\(')[[1]][2], 1, 3) %in% c('AMS', 'SMT', 'SEM')) {
    return(first)
  } else if (substring(strsplit(first, '\\(')[[1]][2], 1, 3) %in% c('AMS', 'SMT', 'SEM') &
             !substring(strsplit(second, '\\(')[[1]][2], 1, 3) %in% c('AMS', 'SMT', 'SEM')) {
    return(paste(first, second))
  } else {
    return(NA)
  }
}

trim.trailing <- function (x) sub("[ \\t]+$", "", x)

combine_rows <- function(first, second) {
  if (length(strsplit(first, '\\(')[[1]]) > 1 & length(strsplit(second, '\\(')[[1]]) > 1) {
    return(first)
  } else if (length(strsplit(first, '\\(')[[1]]) > 1 & length(strsplit(second, '\\(')[[1]]) == 1) {
    return(paste(first, second))
  } else if (length(strsplit(first, '\\(')[[1]]) == 1 & is.na(second)) {
    return(NA)
  } else {
    return(NA)
  }
}

parse_session_meta <- function (session_as_lines) {
  session_meta <- strsplit(session_as_lines$paper[1], '\\(')[[1]]
  session_title <- session_meta[1]
  association <- strsplit(session_meta[2], '\\)')[[1]][1]
  location <- strsplit(session_meta[3], '\\)')[[1]][1]
  return(as_tibble(cbind(session_title, association, location)))
}

parse_session_chair <- function (session_as_lines) {
  if (length(session_as_lines) > 0) {
    session_chair_name <- strsplit(session_as_lines, '\\(')[[1]][1] %>% trim.trailing()
    session_chair_affiliation <- strsplit(strsplit(session_as_lines, '\\(')[[1]][2], '\\)')[[1]][1] %>% trim.trailing()
    return(as_tibble(cbind(session_chair_name, session_chair_affiliation)))
  } else {}
  return(NA)
}

parse_presentation <- function (paper) {
  presenter_name <- strsplit(paper, '\\(')[[1]][1] %>% trim.trailing()
  presenter_affiliation <- strsplit(strsplit(paper, '\\(')[[1]][2], '\\)')[[1]][1] %>% trim.trailing()
  presentation_title <- strsplit(strsplit(paper, '\\),')[[1]][2], '\\"')[[1]][2] 
  return(as_tibble(cbind(presenter_name, presenter_affiliation, presentation_title)))
}

create_session_table <- function(parsed_session) {
  if (parsed_session %>% filter(grepl('chair', tolower(paper))) %>% nrow() > 0) {
    session_chair <- parse_session_chair(parsed_session %>%
                                           filter(grepl('chair', tolower(paper))) %>%
                                           unlist() %>%
                                           as.character())
  } else {
    session_chair <- parse_session_chair(parsed_session %>%
                                           filter(grepl('moderator', tolower(paper))) %>%
                                           unlist() %>%
                                           as.character())
    
  }
  resp <- parsed_session %>%
    filter(grepl('respondent', tolower(paper))) %>%
    unlist() %>%
    as.character()
  if (length(resp) > 0) {
    session_respondent <- parse_session_chair(resp) %>%
      select(session_respondent_name = session_chair_name,
             session_respondent_affiliation = session_chair_affiliation)
    
  } else {
    session_respondent <- as_tibble(cbind(session_respondent_name = NA, session_respondent_affiliation = NA))
  }
  session_meta <- parse_session_meta(parsed_session)
  session_papers <- t(mapply(parse_presentation, parsed_session$paper)) %>%
    as_tibble() %>%
    unnest() %>%
    anti_join(session_meta, by = c('presenter_name' = 'session_title')) %>%
    filter(!is.na(presentation_title))
  if (nrow(session_papers) == 0) {
    presenter_na <- c(NA, NA, NA) %>% t() %>% as_tibble()
    colnames(presenter_na) <- c('presenter_name', 'presenter_affiliation', 'presentation_title')
    session_papers <- rbind(session_papers,
                            presenter_na)
  }
  return(as_tibble(cbind(session_meta, session_chair, session_respondent, session_papers)))
}


# import data

smt <- readLines('smt2014_extract.txt')
smt_raw_parse <- strsplit(paste(smt, collapse = '\n'), '\n\n')[[1]]

smt_sessions_no_page_turns <- smt_raw_parse %>%
  as_tibble() %>%
  filter(nchar(value) > 70) %>%
  mutate(next_value = lead(value))

smt_sessions <- smt_sessions_no_page_turns %>%
  mutate(session_data = mapply(combine_sessions, value, next_value)) %>%
  filter(!is.na(session_data)) %>%
  select(session_data)



# process imported data

smt_sessions_parsed <- as_tibble(data.frame())

for (i in 1:length(smt_sessions$session_data)) {
  smt_sessions_parsed <- rbind(smt_sessions_parsed,
                               create_session_table(strsplit(smt_sessions$session_data[i], '\n')[[1]] %>% 
                                                      as_tibble() %>%
                                                      select(line = value) %>%
                                                      mutate(next_line = lead(line),
                                                             paper = mapply(combine_rows, line, next_line)) %>%
                                                      select(paper) %>%
                                                      unique() %>%
                                                      filter(!is.na(paper))))
}


musicon_all <- smt_sessions_parsed %>%
  mutate(year = 2014)

# smt_sessions_parsed <- read_csv('smt2016_parsed.csv')

musicon_all <- musicon_all %>%
  rbind(smt_sessions_parsed %>%
          mutate(year = 2016))

write_csv(musicon_all, 'musicon_all_parsed.csv')

smt_title_words <- musicon_all %>%
  unnest_tokens(word, presentation_title) %>%
  anti_join(stop_words)

smt_title_words %>%
  group_by(word, association) %>%
  filter(!is.na(word)) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


smt_title_bigrams <- musicon_all %>%
  unnest_tokens(bigram, presentation_title, token = 'ngrams', n=2) %>%
  separate(bigram, c('word', 'next_word'), sep = " ") %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word) %>% 
  unite(bigram, word, next_word, sep = ' ')

smt_title_bigrams %>%
  group_by(bigram) %>%
  filter(!is.na(bigram)) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


musicon_all %>%
  filter(!is.na(presenter_affiliation)) %>%
  group_by(presenter_affiliation) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


