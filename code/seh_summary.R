
# Input:
# Output:

#--------------------------------------------------------
# Global
#--------------------------------------------------------

rm(list=ls())     # remove previous objects from workspace

library(tidyverse)
theme_set(theme_bw())
library(xtable)

# figure properties
figscale <- 1
fw <- 5 # figure width
fh <- 5 # figure height
figdir <- "../plotak/seh1"

source("rmetrics-functions1.R")

savetbl <- function(x, y, xcapt) {
  xname <- paste(figdir, "/tbl_", y, ".txt", sep = "")
  x %>% 
    xtable(label = y, caption = xcapt) %>% 
    print(include.rownames = F, booktabs = T,
          sanitize.text.function = identity, table.placement = "tb") %>% 
    writeLines(xname)
  return()
}
  
#--------------------------------------------------------
# Entropy data
#--------------------------------------------------------

dsan <- read_tsv("../data/san/san_rgveda_entropy1.tsv")
dest <- read_tsv("../data/est/est_tetra_entropy1.tsv")
dnld <- read_tsv("../data/nld/nl-spoken/tsv/nld_sonnets_entropy1.tsv")
deng <- read_tsv("../data/eng/processed/eng_shakson_entropy.tsv")
deng2 <- read_tsv("../data/eng/processed/eng_milton_entropy.tsv")
dshi <- read_tsv("../data/shi/shi_dell1_entropy1.tsv")

dsum <- bind_rows(dsan, dest, dnld, deng, deng2, dshi)

# extra variables
dsum <- dsum %>% 
  group_by(template) %>% 
  mutate(prominence = as.character(prominence),
         position = as.integer(position),
         lengthline = max(position),
         relpos = (position-1) / (lengthline-1)) %>%
  ungroup()

#--------------------------------------------------------
# Plots: entropy
#--------------------------------------------------------

# plot entropy
dsum %>%
  ggplot() +
  aes(x = relpos, y = entropy, group = prominence, colour = prominence) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ language) +
  scale_color_brewer(palette = "Paired",
                     name = "Prominence of position\nin template") +
  labs(x = "Relative position of syllable within line",
       y = "Entropy") +
  theme(legend.justification=c(0,0), legend.position=c(2/3,1/4))

ggsave.alt(custom.name = "entropy1", width = fw*1.5)

# proportion of heavy
dsum %>% 
  filter(language == "eng") %>% 
  ggplot() +
  aes(x = relpos, y = f1, group = template) +
  geom_point() +
  geom_line() +
  facet_wrap(~ template, ncol = 1)

dsum %>% 
  mutate(deviation = if_else(prominence == "0", f1, f0)) %>% 
  summarySE("deviation",
            c("language", "template", "position", "relpos"))

#--------------------------------------------------------
# MM: entropy, main model (new version)
#--------------------------------------------------------

library(lmerTest)
library(sjPlot)
library(broom)

# main model
mm1 <- lmer(entropy ~ relpos + (1+relpos+prominence|language), dsum)
summary(mm1)
# corresponding null model
mm1_null <- lmer(entropy ~ (1+relpos+prominence|language), dsum)
anova(mm1, mm1_null)

#--------------------------------------------------------
# Tables

mm1tbl <- mm1 %>% 
  summary() %>% 
  coefficients() %>% 
  tidy() %>% 
  mutate_each(funs(prettyNum(., digits = 3)), -.rownames) %>% 
  select(Term = .rownames,
         Estimate,
         "Std. Error" = Std..Error,
         "$t$ value" = t.value,
         "Pr ($>t$)" = Pr...t..)

xcapt <- c("Results of the full mixed model, 
           with relative syllable position as the fixed predictor, 
           and random slope and intercept for the effect of prominence and 
           syllable position in each language.")
savetbl(mm1tbl, "sehfull", xcapt)

#--------------------------------------------------------
# plot random effects.
sjp.lmer(mm1, axis.title = c("Language", "Random effects"),
         sort = "(Intercept)", show.values = T, geom.size = 1)

ggsave.alt(custom.name = "ranef1", width = fw*1.5)

# plot fixed effects.
# not very informative
sjp.lmer(mm1, type = "fe", show.intercept = T)

#--------------------------------------------------------
# LM: entropy + prominence
#--------------------------------------------------------

# run linear model for each language
lang.lm <- dsum %>% 
  group_by(language) %>% 
  do(lm(entropy ~ relpos + prominence, data = .) %>% tidy()) %>% 
  mutate_each(funs(prettyNum(., digits = 3)), -language, -term) %>% 
  select(Language = language,
         Term = term,
         Estimate = estimate,
         "Std. Error" = std.error,
         "$t$ value" = statistic,
         "Pr ($>t$)" = p.value)

savetbl2 <- function(x, y, xcapt) {
  xname <- paste(figdir, "/tbl_", y, ".txt", sep = "")
  x %>% 
    xtable(label = y, caption = xcapt) %>% 
    print(include.rownames = T, booktabs = T,
          hline.after = c(-1,0,nrow(x),3,6,9,12),
          sanitize.text.function = identity,
          table.placement = "tb") %>% 
    writeLines(xname)
  return()
}

xcapt <- c("Results of the linear model applied to each language corpus, 
           with position prominence and relative syllable position as predictors.")
savetbl2(lang.lm, "sehbylang", xcapt)

#--------------------------------------------------------
# MM: entropy, main model (old version)
#--------------------------------------------------------

library(lmerTest)
library(sjPlot)
library(broom)

# main model
mm1 <- lmer(entropy ~ relpos + prominence + (1+relpos+prominence|language), dsum)
summary(mm1)
# corresponding null model
mm1_null <- lmer(entropy ~ prominence + (1+relpos+prominence|language), dsum)
anova(mm1, mm1_null)

#--------------------------------------------------------
# Tables

mm1tbl <- mm1 %>% 
  summary() %>% 
  coefficients() %>% 
  tidy() %>% 
  mutate_each(funs(prettyNum(., digits = 3)), -.rownames) %>% 
  select(Term = .rownames,
         Estimate,
         "Std. Error" = Std..Error,
         "$t$ value" = t.value,
         "Pr ($>t$)" = Pr...t..)

xcapt <- c("Results of the full mixed model, 
           with relative syllable position and position prominence 
           as fixed predictors, and language as random effect.")
savetbl(mm1tbl, "sehfull", xcapt)

#--------------------------------------------------------
# plot random effects.
sjp.lmer(mm1, axis.title = c("Language", "Random effects"),
         sort = "(Intercept)", show.values = T, geom.size = 1)

ggsave(file.path(figdir, "ranef1.pdf"),
       scale = figscale*1, width = fw*1.5, height = fh,
       device = cairo_pdf())

# plot fixed effects.
# not very informative
sjp.lmer(mm1, type = "fe", show.intercept = T)

#--------------------------------------------------------
# MM: entropy, other models
#--------------------------------------------------------

mm1 <- lmer(entropy ~ relpos + prominence + (1+relpos+prominence|language), dsum)
mm2 <- lmer(entropy ~ relpos + (1+relpos+prominence|language), dsum)
mm3 <- lmer(entropy ~ (1+relpos+prominence|language), dsum)

anova(mm3, mm2)
anova(mm2, mm1)

sjp.lmer(mm2, axis.title = c("Language", "Random effects"),
         sort = "(Intercept)", show.values = T, geom.size = 1)

