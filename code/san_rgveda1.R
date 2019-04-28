
# input: text file with proportion of H syllables in each position of
#        samples of Ṛgveda lines with 8, 11, and 12 syllables.
#        extracted from: Gunkel & Ryan 2011. Hiatus Avoidance and Metrification in the Rigveda.
# output: (1) dsum = one row per position in template, with entropy

#--------------------------------------------------------
# Global
#--------------------------------------------------------

rm(list=ls())     # remove previous objects from workspace

library(tidyverse)
theme_set(theme_bw()) # ggplot
library(vegan)

#--------------------------------------------------------
# dsum (row = position in template)
#--------------------------------------------------------

# Data taken from Gunkel & Ryan 2011.
# The data includes proportion of heavy syllables in each position of 3 classes of metres:
# metres with 8, 11, 12 syllables.
# The proportion is calculated on a sample of verses from the Rgveda (N?).

dwide <- read_tsv("../data/san/san_rgveda1.csv")

# gather wide data
dlong <- dwide %>% 
  gather("position", "f1", -sample) %>% 
  arrange(sample) %>%
  na.omit() %>% 
  mutate(f1 = f1/100,
         f0 = 1-f1,
         position = as.integer(position),
         language = "san")

# encode idealised templates (based on knowledge of the tradition).

# usually (Arnold 1905), two main metres distinguished: tri and dimetre.
# dimetre is usually 8 syl; tri can be 12 or 11 (or even 10).
# most typical forms are:
#   anuṣṭubh: 4 lines * 8 syl
#   triṣṭubh: 4 * 11 syl
#   jagatī: 4 * 12 syl
# also:
#   gāyatrī: 3 * 8 syl
#   virạj: 3 * 11 syl.
# stanzas which combine dimetre and trimetre verses are called 'lyric'.
# feet:
#   "in almost all metres a general iambic rhythm may be noticed" (:9)
# SEH:
#   "in all metres the rhythm of the latter part of the verse is
#   much more rigidly defined than that of the earlier part." (:9)

tmplt11 <- tibble(sample = "rgveda11",
                  template = "iam6a",
                  position = seq(11),
                  prominence = c(rep(c(0, 1), 5), 0))
tmplt12 <- tibble(sample = "rgveda12",
                  template = "iam6b",
                  position = seq(12),
                  prominence = c(rep(c(0, 1), 6)))
tmplt8 <- tibble(sample = "rgveda8",
                  template = "iam4",
                  position = seq(8),
                  prominence = c(rep(c(0, 1), 4)))
templates <- bind_rows(tmplt11, tmplt12, tmplt8)

# estimate n of lines in each sample provided by gunkel&ryan2011.
#
# size of each sample. counts from arnold1905:69:
# 23669 trimet (11syl and 12syl), 15000 dimet.
# however, gunkel and ryan 2011 exclude 761 11syl and 12syl lines.
# trimet also contains 10syl, and dimet 7syl,
# so counts are approximate.
# also, arnold gives counts for trimet, but gunkel&ryan
# give frequencies for 11syl and 12syl separately.
# i will divide arnold's trimet counts by 2 to get a *rough*
# approximation to the counts of 11syl and 12syl.
size_trimet <- (23669 - 761) / 2
size_dimet <- 15000
sizes <- tibble(sample = c("rgveda11", "rgveda12", "rgveda8"),
                size = c(size_trimet, size_trimet, size_dimet))

# bind size and template,
# generate counts based on frequency and approximate sample size,
# compute entropy (base=2) based on counts
dsum <- dlong %>% 
  left_join(templates) %>% 
  left_join(sizes) %>% 
  mutate(c1 = (f1*size) %>% round(0),
         c0 = (f0*size) %>% round(0)) %>% 
  mutate(entropy = diversity(.[,c("c1", "c0")], base = 2))

# saveRDS(dsum, "../data/san/san_rgveda_entropy1.rds")
# write_tsv(dsum, "../data/san/san_rgveda_entropy1.tsv")
# dsum <- readRDS("../data/san/san_rgveda_entropy1.rds")

