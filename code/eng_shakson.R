
#' input: text files with all sonnets by shakespeare (n = 154);
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
dl <- read_lines("../data/eng/raw/eng_shakson1.txt")[-c(1:4)]

# each verse line is encoded in 4 separate rows of text:
# syllables, stress, juncture, comment.
# each row of text contains the following fields:
# verse-line number, encoded feature (syl, str, junct, comm),
# text of whole verse-line, features for each syllable within verse-line.
# except for comment-row, which provides comments for the whole verse-line.
# first verse-line of each poem is indicated in the comment field
# with "poem initial".

# according to hayes2012 paper, sample contains all 154 shakespeare's
# sonnets, except for sonnet 145 which is in iambic tetrametre,
# summing 2141 lines,
# 7.6 % of which contain 11 syllables, instead of 10,
# i.e. 163 lines.

#--------------------------------------------------------
# Create IDs, split syllable fields
#--------------------------------------------------------

# find poem initial verse lines
initial1 <- grep("poem initial", dl)

# comment field is the 4th row of each line; select the 1st row of
# each poem
initial1 <- initial1 - 3
# calculate last row of each poem (plus 1)
lastrow <- c(initial1[-1], length(dl)+1)
# length in rows of each poem
poemlenRows <- lastrow - initial1
poemlenLines <- poemlenRows / 4
table(poemlenLines)

# assign poemIDs
poemID <- rep(1:length(poemlenLines), poemlenRows)

# relative line ID
lineRelID <- unlist(sapply(poemlenLines, seq))
lineRelID <- rep(lineRelID, each = 4)

# create absolute lineID by combining poemID and lineRelID
lineAbsID <- paste(poemID, lineRelID, sep = "_")

# put together IDs and line data
ddf <- data_frame(poemID, lineRelID, lineAbsID, dl)

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

# as expected, most lines (n=1979, 92%) contain 10 syllables.
dsyl_sum %>% 
  count(linelength) %>% 
  mutate(prop = n/sum(n))

# sonnet 145 is the only deviant one where all lines are 8-syllable long
dsyl_sum %>% 
  filter(linelength == 8) %>% 
  count(poemID)

# Number of poems.
# 154 (as expected)
dsyl %>%
  distinct(poemID) %>% 
  nrow()

# Number of lines per poem.
# 152 poems have 14 lines (as canonical sonnets do).
dsyl %>%
  group_by(poemID) %>% 
  summarise(n_lines = n_distinct(lineAbsID)) %>% 
  count(n_lines)

# non-canonical sonnets:
#   sonnet 126 has 12 lines,
#   sonnet 99 has 15 lines.
dsyl %>%
  group_by(poemID) %>% 
  summarise(n_lines = n_distinct(lineAbsID)) %>% 
  filter(n_lines != 14)

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
                    sample = "shakson",
                    author = "shakespeare",
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

# saveRDS(d, "../data/eng/processed/eng_shakson_dev1.rds")
# write_tsv(d, "../data/eng/processed/eng_shakson_dev1.tsv")

#--------------------------------------------------------
# summarise by position in template, add entropy
#--------------------------------------------------------

# d <- readRDS("../data/eng/processed/eng_shakson_dev1.rds")

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

# saveRDS(dsum, "../data/eng/processed/eng_shakson_entropy.rds")
# write_tsv(dsum, "../data/eng/processed/eng_shakson_entropy.tsv")
# dsum <- readRDS("../data/eng/processed/eng_shakson_entropy.rds")

