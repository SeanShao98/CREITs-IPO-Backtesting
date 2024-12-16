# Backtesting of C-REITs IPO strategy returns
# Created: 2024/8/21
# Modified: 2024/11/29
# Author: yifu.shao@CICC
#
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# The author holds the copyright for this project.
# This project is part of the Fixed Income Research Group at CICC RS Department.
# The code in this project represents non-confidential technical information
# developed and published by Yifu Shao as part of related topics within the
# Fixed Income Research Group at CICC.
# The purpose of open-sourcing this code is to share algorithms for
# strategy return calculations and data processing workflows.
# Copyright for the code belongs to the author,
# and usage is subject to the MIT open-source license.
# 
# Special Disclaimer: The reliability and applicability of the publicly
# disclosed content of this code are for reference only.
# The publication of this code does not constitute any liability on the part of
# CICC for any consequences, nor does it serve as any form of investment advice.
# For data beyond the publicly available information **in the repository**,
# please contact the relevant personnel at CICC.
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# 1. load package ------------------------------------------------
## basic package
##  - depreciated: reshape2
library(stringr)
library(openxlsx); library(readxl)
library(dplyr); library(tidyr)
library(ggplot2); library(showtext)
library(stargazer); library(modelsummary); library(kableExtra)
library(matrixcalc) ## for matrix cal
library(stringdist) ## for fuzzy vlookup
library(parallel)   ## for parApply

rm(list = ls())
options(scipen = 10)
# 2. common function ------------------------------------------------
## 2.1. level-0 functions ------------------------------------------------
##      - functions for basic data process

### seq for double instead of float
rseq <- function(from = 1, to = 1, by = ((to - from)/(length.out - 1)), digits) {
  ### rounded seq, the last argument is digit
  return(round(seq(from, to, by), digits = digits))
}


### Excel-like vlookup, vec-in-vec-out
vlookup <- function(dt_index, dt_lookup_area, pointer = NULL, no.match = NA) {
  ### {vec, df} :-> vector
  dt_lookup_area %<>% unique()
  if (is.null(pointer)) {
    area <- dt_lookup_area
  } else {
    area <- subset(dt_lookup_area, select = c(1, pointer))
  }
  area %<>% as.data.table() %>% `names<-`(c("index", "value"))
  index <- dt_index %>% as.data.table() %>% `names<-`("index")
  area[index, on = "index"][[2]] %>%
    replace(is.na(.), no.match) %>%
    return()
}


### find the closet chr for col_fuz in col_dic, or reurn value
fuzzy_vlookup <- function(col_fuz, col_dic, value = NULL, return_val = FALSE) {
  ### same as vlookup, but col_fuz, col_dic and value are all VECTORS
  ### return_val = FALSE: return closest string in col_dic
  ### return_val = TRUE:  return value AT SAME LOCATION in value
  sapply(col_fuz, function(ii) {
    stringdist::stringdist(ii, col_dic, method = "jw") %>% which.min()
  }, simplify = TRUE) -> col_return
  if (return_val) {
    value[col_return] %>% `names<-`(col_fuz) %>% return()
  } else {
    col_dic[col_return] %>% `names<-`(col_fuz) %>% return()
  }
}


### count unique element
unicnt <- function(vec) {
  unique(vec) %>% length() %>% return()
}


### replace value by mutiple index (multi-column vlookup)
multi_index_replace <- function(dt_0, dt_new, index_col, val_col) {
  ### replace values in dt_0[, val_col] with values in dt_new[, val_col],
  ### according to index_col.
  ### it functions as 'replace', so:
  ###   1. val_col must exist in dt_0
  ###   2. new index_col in dt_new will be IGNORED
  ###   3. it is essentially multi-column vlookup
  ###   4. dt_new should be unique by id
  
  ### run for example
  ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ###   dt_0 = data.table(id1 = c("A", "A", "B", "B"),
  ###                     id2 = c("a", "d", "d", "d"),
  ###                     value1 = 1:4, value2 = 11:14)
  ###   dt_new = data.table(id1 = c("A", "A", "C"), id2 = c("d", "d", "a"),
  ###                      value1 = c(999,888,777), value2 = c(999,888,777))
  ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  dt_new %<>% unique(by = index_col)
  left_join(dt_0, dt_new, by = index_col, suffix = c("", "_new")) %>%
    mutate(across(all_of(val_col),
                  ~ coalesce(get(paste0(cur_column(), "_new")), .))) %>%
    select(-ends_with("_new")) %>%
    return()
}


### find the position (in logi-vec) of the closet value
near_select <- function(vec, x) {
  ### vec = c(1, 2, 3), x = 2.5 => c(F, T, F)
  ### returns LOGIC VEC
  return(vec == vec[which.min(abs(vec - x))])
}


### flexible select() function based on stringr
select_name <- function(tbl, v_reg) {
  ### select multiple columns using regex from tbl
  ### NOTED: one regex can match multiple columns
  ### E.g., [, .(col_1, var_1, col2)]
  ###       1. v_reg = "col.*" will return col_1 and col2
  ###       2. v_reg = c("^c.*1$", "var") will return col_1 and var_1
  ###       3. regex based on flexible stringr
  lapply(v_reg, function(reg) {
    col <- select_if(tbl, str_detect(names(tbl), reg))
    if (dim(col)[2] == 0) {
      tibble(rep(NA, dim(col)[1])) %>%
        `names<-`(reg) %>%
        return()
    } else {
      return(col)
    }
  }) %>% {do.call(cbind, .)} %>%
    as.data.table() %>%
    return()
}

## 2.2. level-1 functions ------------------------------------------------
##      - for certain purpose or poject

### discrete gradient color vector
color_scale <- function(n, dark = "#0D2357", light = "#C6DBEF", name = NULL) {
  ### returns evenly divided color, from light to dark
  scales::seq_gradient_pal(light, dark, "Lab")(seq(0,1,length.out=n)) %>%
    `names<-`(name) %>%
    return()
}

### p-value stars
get_stars <- function(pval) {
  cut(pval, breaks = c(-0.1, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", " ")) %>%
    return()
}


### factorize a vector according to value, with a value-label
factor_with_valuelabel <- function(variable, levels, value) {
  ### for chr vector variable, and given levels from variable
  ###    get factorize the vector and labeled with value
  ### e.g. c("A", "B"), Corr = c(0.5, 0.3) => "A Corr = 0.5", "B Corr = 0.3"
  ###      with levels = c("A", "B")
  dt_fac <- data.table(variable = factor(variable, levels = levels), value) %>%
    unique() %>% arrange(variable)
  factor(
    variable,
    levels = levels,
    labels = paste0(dt_fac$variable, "\n", dt_fac$value)
  ) %>%
    return()
}


### manually output table content as latex
outTex_tbl <- function(tbl,
                       var.names = TRUE,
                       top.lines = "\\\\[-1.8ex]\\hline \\hline\\\\[-1.8ex]",
                       title.lines = "\\hline \\\\[-1.8ex]",
                       bottom.lines = "[1ex]\\hline \\\\[-2.5ex]",
                       column.add = NULL,
                       file.name) {
  ### Tex tabular output, without environment, only inner content
  ### including lines, by default
  ### var.names: if TRUE, use names(tbl) as title. Or no title.
  n.pos <- 0
  for (pos in column.add) {
    tbl <- add_column(tbl, new = rep("", dim(tbl)[1]), .after = pos + n.pos)
    n.pos <- n.pos + 1
  }
  tbl_tex <- apply(tbl, 1, function(row) {
    paste0(paste(row, collapse = " & "), " \\\\")
  })
  if (var.names == TRUE) {
    tbl_tex <- c(paste0(paste(names(tbl), collapse = " & "), " \\\\"),
                 title.lines, tbl_tex)
  }
  tbl_tex <- c(top.lines, tbl_tex, bottom.lines)
  cat(tbl_tex, sep = "\n", file = file.name)
}

### separate a long table into pieces by row, and cbind them
cut_tbl <- function(tbl, nrow, N, fill.with = "") {
  ### if tbl is 17*2, cut it into N pieces, each piece has nrow rows
  ### if out-bounded, fill with "" by default
  new_tbl <- tbl[1:nrow, ]
  for (n in c(2:N)) {
    new_tbl <- cbind(new_tbl, tbl[((n - 1) * nrow + 1):(n * nrow), ])
  }
  names_new_tbl <- names(new_tbl)
  new_tbl %>%
    `names<-`(paste0("V", c(1:dim(new_tbl)[2]))) %>%
    mutate_all(~replace(., is.na(.), fill.with)) %>%
    `names<-`(names_new_tbl) %>%
    return()
}

### full-return ifelse
ifelse_full <- function(condition, yes, no) {
  ### E.g., x <- c(1, 2, -1, 0, -3, 4)
  ###       ifelse(x > 0, x, 9999)
  ### base::ifelse is scalar-width. This fun is vector-width
  if (condition) {
    return(yes)
  } else {
    return(no)
  }
}

### normalize
normalize <- function(x) {return((x - mean(x, na.rm = T))/sd(x, na.rm = T))}

### add date
date_add <- function(dates, days, date_dic = dt_fcost$date) {
  date_dic[match(dates, date_dic) + days] %>% return()
}
