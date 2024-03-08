library(tidyverse)
library(corpora)
library(broom)
library(ggfortify)
library(ggeffects)
library(caret)
library(car)
library(tidymodels)

# import df_livetexts from textverarbeitung.R
load("./weltfussball_liveticker/df_livetexts.RData")

# import corpus, corpus_old & livetexts_tidy from textverarbeitung.R
load("./weltfussball_liveticker/corpus.RData")

