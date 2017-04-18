library(tidytext)
library(tidyverse)

cccc_all <- read_csv('cccc_programs_parsed.csv')
musicon_all <- read_csv('musicon_programs_parsed.csv')

# add summary statistics

cccc <- cccc_all %>%
  group_by(year) %>%
  mutate(total_presentations = sum(!is.na(presentation_title)),
         total_sessions = sum(!is.na(unique(session_title)))) %>%
  ungroup() 

musicon <- musicon_all %>%
  group_by(year) %>%
  mutate(total_presentations = sum(!is.na(presentation_title)),
         total_sessions = sum(!is.na(unique(session_title)))) %>%
  ungroup() 

# combine conferences

conf <- cccc %>%
  mutate(association = 'CCCC',
         source = 'CCCC') %>%
  full_join(musicon %>%
              mutate(source = 'musicon'))

conf_title_words <- conf %>%
  unnest_tokens(word, presentation_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word)) %>%
  group_by(year, association) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup() 

conf_title_words %>%
  group_by(word, year, source, annual_words) %>%
  summarize(count = n()) %>%
  mutate(share = count/annual_words) %>%
  # filter(word %in% cccc_words_ranked$word[1:10]) %>%
  filter(word %in% c('pedagogy')) %>%
  ggplot(aes(year, share, color = source)) +
  geom_line() +
  xlab('Year') + 
  ylab('Perecentage of words in presentation titles') +
  ggtitle('Word share for selected words in CCCC presentation titles')



word_ratios <- conf_title_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(CCCC / musicon)) %>%
  arrange(desc(logratio))


word_ratios %>%
  filter(word != 'â') %>%
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (CCCC/Musicon)") +
  ggtitle('Word frequency in CCCC and SMT/AMS annual meetings.') +
  scale_fill_discrete(name = "", labels = c("CCCC", "SMT/AMS"))
