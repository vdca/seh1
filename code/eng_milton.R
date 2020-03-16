
#' input: text files with verse lines by john milton (paradise lost 9 and 10);
#'        with scansion by bruce hayes; four rows per verse line.
#' source: Bruce Hayes, Colin Wilson, Anne Shisko. 2012. Maxent grammars for the metrics of Shakespeare and Milton. (Online supplementary material)
#'         https://linguistics.ucla.edu/people/hayes/ShakespeareAndMilton/
#' output: only 10-syllable lines:
#'   (1) d = one row per syllable
#'   (2) dsum = one row per position in template, with entropy

#--------------------------------------------------------
# Global
#--------------------------------------------------------

rm(list=ls())     # remove previous objects from workspace

library(tidyverse)
theme_set(theme_bw())
library(vegan)

#--------------------------------------------------------
# Load data
#--------------------------------------------------------

# first 4 lines contain metadata; exclude
dl <- read_lines("../data/eng/raw/eng_milton1.txt")[-c(1:4)]

# each verse line is encoded in 4 separate rows of text:
# syllables, stress, juncture, comment.
# each row of text contains the following fields:
# verse-line number, encoded feature (syl, str, junct, comm),
# text of whole verse-line, features for each syllable within verse-line.
# except for comment-row, which provides comments for the whole verse-line.

# according to hayes2012, sample contains books 8 and 9
# of paradise lost, summing 2293 lines (of iambic pentametre),
# 3.2 % of which contain an 11th extrametrical syllable,
# i.e. 73 or 74 lines.

# actually, sample includes books 9 and 10 (rather than 8 and 9).

#--------------------------------------------------------
# Create IDs, split syllable fields
#--------------------------------------------------------

# compare data from Hayes et al. 2012 with raw text from:
# https://www.dartmouth.edu/~milton/reading_room/pl/book_9/text.shtml

# generate a poemID (= bookID)
d9 <- read_lines("../data/eng/raw/eng_milton_paradise9.txt")
d10 <- read_lines("../data/eng/raw/eng_milton_paradise10.txt")
poemID <- rep(c(9, 10), c(length(d9)*4, length(d10)*4))

# find stanza initial verse lines
initial1a <- grep("#", d9)
initial1b <- grep("#", d10)

# relative line ID
poemlenLines <- c(length(d9), length(d10))
lineRelID <- map(poemlenLines, seq) %>%
  unlist() %>% 
  rep(each = 4)

# create absolute lineID by combining poemID and lineRelID
lineAbsID <- paste(poemID, lineRelID, sep = "_")

# put together IDs and line data
ddf <- tibble(poemID, lineRelID, lineAbsID, dl)

# split syllables
dsyl <- ddf %>% 
  mutate(dsplt = map(dl, ~ strsplit(.x, '\t') %>% unlist %>% t %>% as_data_frame)) %>%
  unnest(dsplt) %>% 
  select(-dl, -V1, -V3)
colnames(dsyl) <- c("poemID", "lineRelID", "lineAbsID", "feature", paste0('X', 1:11))

#--------------------------------------------------------
# Summarise
#--------------------------------------------------------

# Number of syllables per line
dsyl_sum <- dsyl %>% 
  filter(feature == 'syllables') %>% 
  gather('position', 'syllable', X1:X11) %>% 
  filter(syllable != '') %>%
  group_by(poemID, lineAbsID) %>% 
  summarise(linelength = n()) %>% 
  ungroup()

# Total n of lines = 2293.
#   2220 contain 10 syl.
#   73 contain 11 syl.
dsyl_sum %>% 
  count(linelength) %>% 
  mutate(prop = n/sum(n))

# Number of books: 2 (paradise lost #9 and #10).
length(unique(ddf$poemID))
# Number of stanzas per book: 48, 44
length(initial1a)
length(initial1b)

# Number of lines per book
ddf %>% 
  group_by(poemID) %>% 
  summarise(max(lineRelID))

# Non-empty comments
realcomments <- dsyl %>% 
  filter(feature == 'comment',
         X1 != "") %>%
  rename(comment = X1) %>% 
  select(lineAbsID, comment)

#--------------------------------------------------------
# Filter data
#--------------------------------------------------------

# select only 10-syllable lines
d10 <- dsyl %>% 
  filter(lineAbsID %in% filter(dsyl_sum, linelength == 10)$lineAbsID) %>% 
  select(-X11)

#--------------------------------------------------------
# Long-format data
#--------------------------------------------------------

# convert stress levels (from 1 = unstressed, 2 secondary, to 3 and 4 primary) to binary.
# recode unstressed (less than threshhold x) as 0 (now x = 1; could also be x = 2),
# recode everything above x as 1

dlong <- d10 %>% 
  filter(feature == 'stress') %>% 
  select(-feature, -poemID, -lineRelID) %>% 
  gather('position', 'stress', X1:X10) %>% 
  mutate(stress = as.integer(stress),
         stress = if_else(stress > 1, 1, 0),
         position = str_replace(position, 'X', ''),
         position = as.integer(position)) %>% 
  rename(lineID = lineAbsID)

#--------------------------------------------------------
# standardise (unified format across language samples)
#--------------------------------------------------------

# define standard prominence template for iambic pentametre
# define sample-wide features
templates <- tibble(language = "eng",
                    sample = "milpar",
                    author = "milton",
                    template = "iam5",
                    lengthline = 10,
                    position = seq(lengthline),
                    relpos = (position-1) / (lengthline-1),
                    prominence = rep(c(0,1), 5))

# compute deviations from ideal template
# re-order columns
d <- dlong %>% 
  left_join(templates) %>% 
  mutate(value = as.integer(stress),
         stress = NULL,
         deviation = (prominence != value) %>% as.integer,
         uniqueLineID = paste(sample, lineID, sep = "_")) %>% 
  select(language, sample, template, lineID, position,
         prominence, lengthline, value, deviation, relpos, uniqueLineID)

# saveRDS(d, "../data/eng/processed/eng_milton_dev1.rds")
# write_tsv(d, "../data/eng/processed/eng_milton_dev1.tsv")

#--------------------------------------------------------
# summarise by position in template, add entropy
#--------------------------------------------------------

# d <- readRDS("../data/eng/processed/eng_milton_dev1.rds")

# summarise data by sample
dsum <- d %>% 
  group_by(language, template, sample, position, relpos, prominence) %>% 
  summarise(n = n(),
            c1 = sum(value)) %>% 
  ungroup() %>% 
  mutate(c0 = n - c1,
         f1 = c1/n,
         f0 = c0/n) %>% 
  rename(size = n) %>% 
  mutate(entropy = diversity(.[,c("c1", "c0")], base = 2))

# saveRDS(dsum, "../data/eng/processed/eng_milton_entropy.rds")
# write_tsv(dsum, "../data/eng/processed/eng_milton_entropy.tsv")
# dsum <- readRDS("../data/eng/processed/eng_milton_entropy.rds")
