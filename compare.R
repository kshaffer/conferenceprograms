library(tidytext)
library(tidyverse)

cccc_all <- read_csv('parsed_programs/cccc_programs_parsed.csv')
musicon_all <- read_csv('parsed_programs/musicon_programs_parsed.csv')
mla_all <- read_csv('parsed_programs/mla_programs_parsed.csv') %>%
  select(presenter_name = speaker,
         presenter_affiliation = institution,
         presentation_title,
         session_title,
         year,
         source = society)

# add summary statistics

cccc <- cccc_all %>%
  group_by(year) %>%
  mutate(total_presentations = sum(!is.na(presentation_title)),
         total_sessions = sum(!is.na(unique(session_title)))) %>%
  ungroup() 

mla <- mla_all %>%
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
  mutate(source = 'CCCC') %>%
  full_join(mla) %>%
  full_join(musicon %>%
              mutate(source = 'SMT/AMS')) %>%
  select(-session_description, -association, -location,
         -session_chair_name, -session_chair_affiliation,
         -session_respondent_name, -session_respondent_affiliation)

conf_title_words <- conf %>%
  unnest_tokens(word, presentation_title) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word),
         !word %in% c('de', 'la', 'du', 'des', 'univ')) %>%
  mutate(word = gsub('studentsâ', 'students', word)) %>%
  group_by(year, source) %>%
  mutate(annual_words = sum(!is.na(word))) %>%
  ungroup() 

conf_title_words %>%
  group_by(word, year, source, annual_words) %>%
  summarize(count = n()) %>%
  mutate(share = count/annual_words) %>%
  filter(word %in% c('students')) %>%
  ggplot(aes(year, share, color = source)) +
  geom_line() +
  xlab('Year') + 
  ylab('Perecentage of words in presentation titles') +
  ggtitle('Word share for "students" in CCCC, MLA, and SMT/AMS presentation titles')



word_ratios <- conf_title_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(CCCC / MLA)) %>%
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
  ylab("Log odds ratio (CCCC/MLA)") +
  ggtitle('Word frequency in CCCC (2014-17) and MLA (2014-16) annual meetings.') +
  scale_fill_discrete(name = "", labels = c("CCCC", "MLA"))


institution_ratios <- conf %>%
  mutate(presenter_affiliation = gsub('Univ\\.', 'University', presenter_affiliation),
         presenter_affiliation = gsub('Coll\\.', 'College', presenter_affiliation),
         presenter_affiliation = gsub('The Ohio', 'Ohio', presenter_affiliation),
         presenter_affiliation = gsub('The Pennsylvania', 'Pennsylvania', presenter_affiliation),
         presenter_affiliation = gsub(' at Urbana-Champaign', '', presenter_affiliation),
         presenter_affiliation = gsub('-Madison', '', presenter_affiliation),
         presenter_affiliation = gsub('-Lincoln', '', presenter_affiliation),
         presenter_affiliation = gsub(' Amherst', '', presenter_affiliation),
         presenter_affiliation = gsub('^University of California.*', 'University of California', presenter_affiliation),
         presenter_affiliation = gsub(' at .*$', '', presenter_affiliation),
         presenter_affiliation = gsub('Graduate Center', 'Graduate Center, CUNY', presenter_affiliation),
         presenter_affiliation = gsub('CUNY Graduate Center', 'Graduate Center, CUNY', presenter_affiliation)) %>%
  count(presenter_affiliation, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -presenter_affiliation) %>%
  mutate(logratio = log(MLA / `CCCC`)) %>%
  arrange(desc(logratio))


institution_ratios %>%
  filter(!grepl('Level', presenter_affiliation),
         !grepl('Civic & Public NA', presenter_affiliation),
         !grepl('NY', presenter_affiliation)) %>%
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(presenter_affiliation = reorder(presenter_affiliation, logratio)) %>%
  ggplot(aes(presenter_affiliation, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab('Presenter affiliation') +
  ylab("Log odds ratio (MLA/CCCC)") +
  ggtitle('Most distinctive presenter affiliations in\nMLA (2014-16) and CCCC (2014-17) annual meetings.') +
  scale_fill_discrete(name = "", labels = c("MLA", "CCCC"))
