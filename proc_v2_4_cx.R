# Backtesting of C-REITs IPO strategy returns
# Created: 2024/8/21
# Modified: 2025/1/10
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

# 0. initializing ------------------------------------------------
## 项目文件夹位置
setwd("~/***")

## personal head file
source("ini.R")

## required package:
## library(data.table); library(magrittr)
require(see)
library(showtext)
library(ks)
library(cowplot)
library(patchwork)
library(ggnewscale)

## 中文字体文件
font_add(family = "Hei", regular = "STHeiti Light.ttc")
showtext_auto()

## 读取proc_1_data.R所清洗的所有数据
load("MAIN_V2.RData")

# 1. cx data ------------------------------------------------
## 数据准备
dt_return_D60 <- readxl::read_xlsx("REITs基金收益率-v2.xlsx", sheet = "D60-Value") %>%
  as.data.table() %>%
  rename(fcode = 证券代码, fname = 证券名称, Listdate = 上市日期,
         po_d0 = 首日开盘价, plist = 发行价)

apply(dt_return_D60, MARGIN = 1, function(f.j) {
  ## 检查上市日期: 指从0开始多少条数据是有意义的(包含首日).
  ## d_range = 4 表明 0:4 共有5条行情数据, 但打新只统计 0:3 共4个交易窗口.
  d_range <- min(
    which(sort(dt_fcost$date, decreasing = TRUE) == f.j["Listdate"])[1] - 1,
    60
  )
  ## 当日买入-卖出不考虑(都是收盘价), 因此买入0-59, 卖出1-60
  vec_pc <- as.numeric(f.j[-c(1:5)])
  
  ## 连阳次数, (最大)连阳天数, 翻阳天数
  rev_flag_rl <- rle(diff(vec_pc, lag = 1) > 0)
  dt_rl <- data.table(
    fcode = f.j["fcode"],
    rle.n = sum(rev_flag_rl$values & rev_flag_rl$lengths > 1),
    rle.max = max(rev_flag_rl$lengths[rev_flag_rl$values]),
    rle.sum = sum(rev_flag_rl$lengths[rev_flag_rl$values]),
    d_range = d_range
  )
  
  ## 对 d in 0:59 买入, 可以在后续任意一天卖出。for all d, 可以计算指标:
  ##    (长度皆为60) 在 d 日买入,
  ##    1. 最短盈利时间: 第一个正收益的日期, (1-60, NA)
  ##    2. 盈利窗口宽度: 有正收益的天数, (1-60, NA)
  ##    3. 平均盈利幅度: 剩余日期内平均盈利情况(等权重)
  ##    4. 平均盈利幅度-去负值: 去除亏钱部分的平均盈利幅度
  ##    5. 最大盈利幅度: 剩余日期内最多能赚多少
  ## *仅保留有效交易窗口
  dt_rev <- sapply(0:(d_range - 1), function(d.buy) {
    p.d.buy <- vec_pc[1 + d.buy]
    vec_pc_rest <- vec_pc[1:(d_range + 1)][-(1 + 0:d.buy)]
    ## gc01 at d.buy
    i.d.buy <- dt_fcost %>%
      filter(date == date_add(f.j["Listdate"], d.buy)) %>%
      pull(cost)
    ## gc01 after d.buy
    vec_gc01.d <- r_gc01(
      date_add(f.j["Listdate"], d.buy),
      date_add(f.j["Listdate"], d.buy + length(vec_pc_rest)),
      dt_fcost
    )
    exrev_rm <- vec_pc_rest / p.d.buy * 100 -
      cumprod(vec_gc01.d)[-length(vec_gc01.d)] * 100
    ### 组装数据
    data.table(
      fcode = f.j["fcode"],
      d_range = d_range,
      d_buy = d.buy,
      ### 盈利窗口
      min_rev_d = which(vec_pc_rest > p.d.buy)[1],
      sum_rev_d = sum(vec_pc_rest > p.d.buy),
      ### 收益 (绝对值, 元)
      avg_rev_cny = mean(vec_pc_rest - p.d.buy),
      max_rev_cny = max(vec_pc_rest - p.d.buy),
      avg_rev_rm_cny = mean(c(vec_pc_rest - p.d.buy)[vec_pc_rest - p.d.buy > 0]),
      avg_los_rm_cny = mean(c(vec_pc_rest - p.d.buy)[vec_pc_rest - p.d.buy < 0])
    ) %>%
      mutate(
        ### 注意0亏损或者0盈利时, *_rm_cny 会有NaN
        across(c(avg_rev_rm_cny, avg_los_rm_cny), ~ ifelse(is.nan(.), 0, .)),
        ### 收益率 %
        avg_rev_rm = avg_rev_rm_cny / p.d.buy * 100,
        avg_los_rm = avg_los_rm_cny / p.d.buy * 100,
        ### 第 d.buy 天的超额收益率%
        avg_exrev_rm = mean(exrev_rm[exrev_rm > 0]),
        avg_exlos_rm = mean(exrev_rm[exrev_rm < 0]),
        across(c(avg_exrev_rm, avg_exlos_rm), ~ ifelse(is.nan(.), 0, .)),
      ) %>% return()
  }, simplify = F) %>% rbindlist()
  return(list("rl" = dt_rl, "rev" = dt_rev))
}, simplify = FALSE) %>%
  `names<-`(dt_return_D60$fcode) -> ls_cxr

dt_cxr_rev <- do.call(rbind, lapply(ls_cxr, function(x) x[[2]])) %>%
  ### !!! 使用收益率还是超额收益率
  mutate(avg_rev_rm = avg_exrev_rm, avg_los_rm = avg_exlos_rm) %>%
  left_join(dt_ofline_j[, .(fcode, fname, ftype, fclass)], by = "fcode") %>%
  mutate(
    ### 标准化盈亏per day: [0, 1], 每一个"bar"的总长是1
    unif_rev_rm = avg_rev_rm / (avg_rev_rm - avg_los_rm),
    unif_los_rm = avg_los_rm / (avg_rev_rm - avg_los_rm),
    ### 简单盈亏比per day: 会出现Inf
    prof_los = - avg_rev_rm / avg_los_rm,
    prof_los = ifelse(is.nan(prof_los), NA, prof_los),
    prof_los = ifelse(is.infinite(prof_los), NA, prof_los)
  ) %>%
  mutate(
    .by = fcode,
    ### 平均标准化盈亏比, 注意unif_rev_rm本身已有比例概念, 加总时无需再除以loss
    unif_rev_los_avg = mean(unif_rev_rm, na.rm = T),
    ### 整体盈亏比
    prof_los_avg = sum(avg_rev_rm, na.rm = T) / sum(-avg_los_rm, na.rm = T),
    ### 平均简单盈亏比标签
    mean_prof_los = mean(prof_los, na.rm = T),
    ### 整体胜率
    sum_sum_rev_d = sum(sum_rev_d),
    TWSR = round(100 * sum_sum_rev_d / sum(d_buy + 1), 2)
  ) %>%
  mutate(
    ## 标准化盈亏比标签
    fname_rev_los_unf = paste0(fname, "\n", round(unif_rev_los_avg, 3)),
    fname_rev_los_unf = forcats::fct_reorder(fname_rev_los_unf,
                                             unif_rev_los_avg,
                                             .desc = TRUE),
    ## 整体盈亏比标签
    fname_prof_los_avg = paste0(fname, "\n", round(prof_los_avg, 1)),
    fname_prof_los_avg = forcats::fct_reorder(fname_prof_los_avg,
                                         prof_los_avg,
                                         .desc = TRUE),
    ## 简单盈亏比标签
    fname_prof_los = paste0(fname, "\n", round(mean_prof_los, 1)),
    fname_prof_los = forcats::fct_reorder(fname_prof_los,
                                          mean_prof_los,
                                          .desc = TRUE),
    ### 胜率标签
    fname_win = paste0(fname, "\n", TWSR, "%"),
    fname_win = forcats::fct_reorder(fname_win, TWSR, .desc = TRUE)
    ### Note: 若有的项目上市时间太短按照胜率排序没意义, 改用sum_sum_rev_d排序
  )

# 2. revenue and loss ------------------------------------------------
dt_cxr_rev %<>% filter(!is.na(ftype))

## 胜率图
for (type.i in unique(dt_cxr_rev$ftype)) {
  ## 45度线坐标
  dt_range <- dt_cxr_rev[ftype == type.i, .(fname_win, d_range)] %>% unique()
  ## 主图
  ggplot(data = dt_cxr_rev[ftype == type.i, ], mapping = aes(x = d_buy)) +
    geom_bar(mapping = aes(y = sum_rev_d, fill = fclass),
             stat = "identity") +
    scale_fill_manual(values = c("产权类" = "#5b0d07", "经营权类" = "#b68e55")) +
    geom_segment(data = dt_range,
                 mapping = aes(x = 0, y = d_range, xend = d_range, yend = 0),
                 linetype = "dotted", alpha = 1) +
    guides(fill = "none") +
    labs(x = paste0(type.i, "类 (胜率), REITs上市后天数"),
         y = "盈利窗口宽度") +
    facet_wrap(~ fname_win, scales = "free") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      strip.text = element_text(size = 10)
    ) -> p_type
  ## 锁定facet大小来绘图
  p_layout <- ggplot_build(p_type)$layout$layout
  total_w <- max(p_layout$COL) * 2.33 + 0.5
  total_h <- max(p_layout$ROW) * 2.2 + 0.5
  ggsave(filename = paste0("output/p_cxr_wins_", type.i, ".pdf"),
         plot = p_type,
         width = total_w, height = total_h)
}

## 盈亏图, raw：每一天的盈亏率，按照整体盈亏比倒序排布
for (type.i in unique(dt_cxr_rev$ftype)) {
  ## 主图
  ggplot(data = dt_cxr_rev[ftype == type.i, ], mapping = aes(x = d_buy)) +
    geom_bar(mapping = aes(y = avg_rev_rm), fill = "#5b0d07",
             stat = "identity") +
    geom_bar(mapping = aes(y = avg_los_rm), fill = "#b68e55",
             stat = "identity") +
    guides(fill = "none") +
    labs(x = paste0(type.i, "类 (盈亏比), REITs上市后天数"),
         y = "盈利率与亏损率 %") +
    facet_wrap(~ fname_prof_los_avg, scales = "free_y") +
    coord_cartesian(xlim = c(0, 60)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      strip.text = element_text(size = 10)
    ) -> p_type
  ## 锁定facet大小来绘图
  p_layout <- ggplot_build(p_type)$layout$layout
  total_w <- max(p_layout$COL) * 2.33 + 0.5
  total_h <- max(p_layout$ROW) * 2.2 + 0.5
  ggsave(filename = paste0("output/p_cxr_rl_org_", type.i, ".pdf"),
         plot = p_type,
         width = total_w, height = total_h)
}

## 盈亏图, unified：盈利/(盈利+亏损)，将值标准化到[0,1]区间
##                  按照平均标准化盈亏比倒序排布
for (type.i in unique(dt_cxr_rev$ftype)) {
  ## 主图
  ggplot(data = dt_cxr_rev[ftype == type.i, ], mapping = aes(x = d_buy)) +
    geom_bar(mapping = aes(y = unif_rev_rm), fill = "#5b0d07",
             stat = "identity") +
    geom_bar(mapping = aes(y = unif_los_rm), fill = "#b68e55",
             stat = "identity") +
    guides(fill = "none") +
    labs(x = paste0(type.i, "类 (标准化盈亏比), REITs上市后天数"),
         y = "盈利与亏损规模 (标准化)") +
    facet_wrap(~ fname_rev_los_unf) +
    coord_cartesian(xlim = c(0, 60), ylim = c(-1, 1)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      strip.text = element_text(size = 10)
    ) -> p_type
  ## 锁定facet大小来绘图
  p_layout <- ggplot_build(p_type)$layout$layout
  total_w <- max(p_layout$COL) * 2.33 + 0.5
  total_h <- max(p_layout$ROW) * 2.2 + 0.5
  ggsave(filename = paste0("output/p_cxr_rl_unf_", type.i, ".pdf"),
         plot = p_type,
         width = total_w, height = total_h)
}

## 盈亏比：每日的盈亏比（弃用），按照平均盈亏比倒序排布
for (type.i in unique(dt_cxr_rev$ftype)) {
  ## 主图
  ggplot(data = dt_cxr_rev[ftype == type.i, ], mapping = aes(x = d_buy)) +
    geom_bar(data = dt_cxr_rev[ftype == type.i & prof_los > 1, ],
             mapping = aes(y = prof_los), fill = "#5b0d07",
             stat = "identity") +
    geom_bar(data = dt_cxr_rev[ftype == type.i & prof_los < 1, ],
             mapping = aes(y = prof_los), fill = "#b68e55",
             stat = "identity") +
    geom_hline(mapping = aes(yintercept = 1), linetype = "dashed") +
    guides(fill = "none") +
    labs(x = paste0(type.i, "类 (平均盈亏比), REITs上市后天数"),
         y = "盈亏比") +
    facet_wrap(~ fname_prof_los, scales = "free_y") +
    coord_cartesian(xlim = c(0, 60)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      strip.text = element_text(size = 10)
    ) -> p_type
  ## 锁定facet大小来绘图
  p_layout <- ggplot_build(p_type)$layout$layout
  total_w <- max(p_layout$COL) * 2.33 + 0.5
  total_h <- max(p_layout$ROW) * 2.2 + 0.5
  ggsave(filename = paste0("output/cxr_rev/p_cxr_plratio_", type.i, ".pdf"),
         plot = p_type,
         width = total_w, height = total_h)
}

# 3. description ------------------------------------------------
## 基本表现（连阳等）
dt_cxr_rl <- do.call(rbind, lapply(ls_cxr, function(x) x[[1]])) %>%
  left_join(dt_ofline_j[, .(fcode, fname, ftype, fclass)], by = "fcode") %>%
  mutate(rle.max = if_else(is.infinite(rle.max), 0, rle.max)) %>%
  mutate(rle.max = if_else(is.na(rle.max), 0, rle.max))

## 连阳次数
dt_cxr_rl %>%
  mutate(fname = forcats::fct_reorder(fname, rle.n, .desc = T)) %>% {
    ggplot(data = ., mapping = aes(x = fname)) +
      geom_bar(mapping = aes(y = rle.n, fill = fclass),
               stat = "identity", alpha = 0.9) +
      geom_line(mapping = aes(y = 50 * rle.n/d_range, group = 1),
                color = "lightblue", linewidth = 1.5) +
      scale_y_continuous(
        name = "连阳次数",
        sec.axis = sec_axis(~ . / 50, name = "连阳次数/上市天数 (前60交易日)")
      ) +
      theme_minimal() +
      labs(x = NULL) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_fill_manual(name = NULL,
                        values = c("经营权类" = "#b68e55",
                                   "产权类" = "#5b0d07")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } -> p_cx_rle_n

## 最大连阳天数
dt_cxr_rl %>%
  mutate(fname = forcats::fct_reorder(fname, rle.max, .desc = T)) %>% {
    ggplot(data = ., mapping = aes(x = fname)) +
      geom_bar(mapping = aes(y = rle.max, fill = fclass),
               stat = "identity", alpha = 0.9) +
      geom_line(mapping = aes(y = 25 * rle.max/d_range, group = 1),
                color = "lightblue", linewidth = 1.5) +
      scale_y_continuous(
        name = "最大连阳天数",
        sec.axis = sec_axis(~ . / 25, name = "最大连阳天数/上市天数 (前60交易日)")
      ) +
      theme_minimal() +
      labs(x = NULL) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_fill_manual(name = NULL,
                        values = c("经营权类" = "#b68e55",
                                   "产权类" = "#5b0d07")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } -> p_cx_rle_max

## 翻阳次数
dt_cxr_rl %>%
  mutate(fname = forcats::fct_reorder(fname, rle.sum, .desc = T)) %>% {
    ggplot(data = ., mapping = aes(x = fname)) +
      geom_bar(mapping = aes(y = rle.sum, fill = fclass),
               stat = "identity", alpha = 0.9) +
      geom_line(mapping = aes(y = 30 * rle.sum/d_range, group = 1),
                color = "lightblue", linewidth = 1.5) +
      scale_y_continuous(
        name = "总翻阳天数",
        sec.axis = sec_axis(~ . / 30, name = "总翻阳天数/上市天数 (前60交易日)")
      ) +
      theme_minimal() +
      labs(x = NULL) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_fill_manual(name = NULL,
                        values = c("经营权类" = "#b68e55",
                                   "产权类" = "#5b0d07")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } -> p_cx_rle_sum

# 4. overview ------------------------------------------------
dt_cxr_rev %>%
  mutate(
    .by = fcode,
    mean_return = mean(avg_rev_rm),
    mean_loss   = mean(avg_los_rm)
  ) %>%
  select(fcode, fname, fclass,
         mean_return,      # 平均盈利率
         mean_loss,        # 平均亏损率
         # mean_prof_los,    # 平均简单盈亏比
         # unif_rev_los_avg, # 平均标准化盈亏比
         prof_los_avg,     # 整体盈亏比
         TWSR) %>%         # 时序加权胜率
  unique() %>%
  mutate(fname = factor(fname, levels = fname[order(TWSR)])) %>%
  pivot_longer(
    cols = -c("fcode", "fname", "fclass"),
    names_to = c("variable"),
    values_to = c("value")
  ) %>%
  arrange(fcode, variable) %>%
  mutate(unit = rep(c("%", "%", "%", ""), unicnt(fcode))) %>%
  mutate(
    .by = variable,
    mean_value = paste0(round(mean(value), 2), unit)
  ) %>%
  mutate(
    sign.fill = case_when(
      variable == "TWSR" & value  >= 50 ~ "1",
      variable == "mean_loss"   ~ "-1",
      variable == "mean_return" ~ "1",
      variable == "prof_los_avg" & value  >= 1 ~ "1",
      .default = "-1"
    ),
    variable_tab = factor(
      variable,
      levels = c("mean_loss", "mean_return", "prof_los_avg", "TWSR"),
      labels = paste0(c("平均亏损率%\n均值=", "平均盈利率%\n均值=",
                        "整体盈亏比\n均值=", "时序加权胜率%\n均值="),
                      mean_value[c(2, 3, 4, 1)])
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = fname, y = value)) +
      facet_wrap(~ variable_tab, nrow = 1, scales = "free_x") +
      coord_flip() +
      geom_bar(mapping = aes(fill = sign.fill),
               stat = "identity", position = "dodge") +
      scale_fill_manual(
        name = NULL, values = c("1" = "#5b0d07", "-1" = "#b68e55")
      ) +
      guides(fill = "none") +
      theme_bw() +
      labs(x = NULL, y = "次新收益率%") +
      theme(strip.text = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 12))
  } -> fig_overall_cxr

# 5. I/O ------------------------------------------------
ggsave(filename = paste0("output/", "fig_overall_cxr", ".pdf"),
       plot = fig_overall_cxr,
       width = 12, height = 10)
ggsave(filename = paste0("output/", "p_cx_rle_max", ".pdf"),
       plot = p_cx_rle_max,
       width = 12, height = 8)
ggsave(filename = paste0("output/", "p_cx_rle_sum", ".pdf"),
       plot = p_cx_rle_sum,
       width = 12, height = 8)
ggsave(filename = paste0("output/", "p_cx_rle_n", ".pdf"),
       plot = p_cx_rle_n,
       width = 12, height = 8)