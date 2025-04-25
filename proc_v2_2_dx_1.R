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
library(see)
library(showtext)
library(cowplot)

## 中文字体文件
font_add(family = "Hei", regular = "STHeiti Light.ttc")
showtext_auto()

## 读取proc_1_data.R所清洗的所有数据
load("MAIN_V2.RData")

## 选择一个收益率
dt_ofline_j %<>%
  mutate(
    dxr_ofl = ifelse(restrict == "FALSE", dxr_ofl_norst, dxr_ofl_rst),
    dxr_pub = dxr_pub
  )

## brief
dt_ofline_j %>%
  `[`(as.Date(Listdate) <= as.Date(Listdate[fname == "华夏首创奥莱REIT"]),
      .(ofl = mean(dxr_ofl), pub = mean(dxr_pub)))

dt_ofline_j %>%
  `[`(as.Date(Listdate) <= as.Date(Listdate[fname == "华夏首创奥莱REIT"]) &
        as.Date(Listdate) >= as.Date("2024-01-01"),
      .(ofl = mean(dxr_ofl), pub = mean(dxr_pub)))

# 0.5. overall description ------------------------------------------------
dt_ofline_j %>%
  mutate(
    fname = factor(fname, levels = fname[order(Listdate)]),
    dxr_spread = dxr_ofl - dxr_pub
  ) %>%
  select(fcode, fname, fclass, dxr_ofl, dxr_pub, dxr_spread) %>%
  pivot_longer(
    cols = starts_with("dxr_"),
    values_to = "dxr",
    names_to = "variable"
  ) %>%
  mutate(sign.r = as.character(sign(dxr))) %>%
  mutate(
    .by = variable,
    win = round(sum(dxr > 0, na.rm = T) / sum(!is.na(dxr)) * 100, 1),
    dxr.mean = round(mean(dxr), 3),
    win.dxr.mean = round(mean(dxr[dxr > 0], na.rm = T), 3),
    loss.dxr.mean = mean(dxr[dxr < 0], na.rm = T),
    win.loss.ratio = round(-win.dxr.mean / loss.dxr.mean, 3)
  ) %>%
  arrange(fcode, variable) %>%
  mutate(
    v_lab = factor(
      variable,
      levels = c("dxr_ofl", "dxr_pub", "dxr_spread"),
      labels = c(
        paste0("网下打新收益率%\n胜率 = ", win[1],
               "%, 平均收益率 = ", dxr.mean[1],
               "%, 胜者平均收益率 = ", win.dxr.mean[1],
               "%, 盈亏比 = ", win.loss.ratio[1]),
        paste0("公众打新收益率%\n胜率 = ", win[2],
               "%, 平均收益率 = ", dxr.mean[2],
               "%, 胜者平均收益率 = ", win.dxr.mean[2],
               "%, 盈亏比 = ", win.loss.ratio[2]),
        paste0("收益率利差% (网下 - 公众)\n网下胜率 = ", win[3],
               "%, 平均利差 = ", dxr.mean[3], "%")
      )
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = fname, y = dxr)) +
      facet_wrap(~ v_lab, ncol = 1, scales = "free_y") +
      geom_bar(mapping = aes(fill = sign.r),
               stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("-1" = "#b68e55", "1" = "#5b0d07")) +
      guides(fill = "none") +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(strip.text = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 12))
  } -> fig_overall_dscrp

dt_dxr_D60 %>%
  select(fcode, tt, pc) %>%
  filter(tt %in% c(0, 3)) %>%
  pivot_wider(
    id_cols = fcode, names_from = tt, values_from = pc, names_prefix = "pc_"
  ) %>%
  left_join(
    select(dt_ofline_j, fcode, fname, dxr = dxr_ofl, Listdate, plist),
    by = "fcode"
  ) %>%
  mutate(
    fname = factor(fname, levels = fname[order(Listdate)]),
    pd_1 = (pc_0 / plist - 1) * 100,
    pd_4 = (pc_3 / plist - 1) * 100
  ) %>%
  pivot_longer(
    cols = c(dxr, pd_1, pd_4),
    values_to = "value",
    names_to = "variable"
  ) %>%
  mutate(sign.r = as.character(sign(value))) %>%
  mutate(
    .by = variable,
    win = round(sum(value > 0, na.rm = T) / sum(!is.na(value)) * 100, 1),
    v.mean = round(mean(value), 3),
    win.v.mean = round(mean(value[value > 0], na.rm = T), 3),
    loss.v.mean = mean(value[value < 0], na.rm = T),
    win.v.ratio = round(-win.v.mean/loss.v.mean, 3)
  ) %>%
  arrange(fcode, variable) %>%
  mutate(
    v_lab = factor(
      variable,
      levels = c("dxr", "pd_1", "pd_4"),
      labels = c(
        paste0("网下打新收益率%\n胜率 = ", win[1],
               "%, 平均收益率 = ", v.mean[1],
               "%, 胜者平均收益率 = ", win.v.mean[1],
               "%, 盈亏比 = ", win.v.ratio[1]),
        paste0("上市第1日价格涨幅%\n胜率 = ", win[2],
               "%, 平均涨幅 = ", v.mean[2],
               "%, 胜者平均涨幅 = ", win.v.mean[2],
               "%, 盈亏比 = ", win.v.ratio[2]),
        paste0("上市第4日价格涨幅%\n胜率 = ", win[3],
               "%, 平均涨幅 = ", v.mean[3],
               "%, 胜者平均涨幅 = ", win.v.mean[3],
               "%, 盈亏比 = ", win.v.ratio[3])
      )
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = fname, y = value)) +
      facet_wrap(~ v_lab, ncol = 1, scales = "free_y") +
      geom_bar(mapping = aes(fill = sign.r),
               stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("-1" = "#b68e55", "1" = "#5b0d07")) +
      guides(fill = "none") +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(strip.text = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 12))
  } -> fig_overall_dscrp_pc

# 1. i-level, renewed ------------------------------------------------
## 账户层面认购金额和账户数量分布（分类型）

label_function_left <- function(breaks) {
  ifelse(breaks < 0, "", format(breaks, big.mark = ","))
}
label_function_right <- function(breaks) {
  ifelse(breaks > 0, "", format(-breaks, big.mark = ","))
}

dt_ofline_type <- dt_ofline_i %>%
  mutate(plist = vlookup(fcode, dt_ofline_j[, .(fcode, plist)])) %>%
  mutate(N_i = unicnt(icode), V_i = sum(qty * plist)/10000, .by = itype) %>%
  select(itype, N_i, V_i) %>% unique() %>%
  arrange(V_i) %>%
  mutate(itype = factor(itype, levels = itype))

ggplot(data = dt_ofline_type, mapping = aes(x = itype)) +
  geom_bar(aes(y = -V_i), stat = "identity",
           fill = "#5b0d07", alpha = 1) +
  geom_bar(aes(y = N_i * (max(dt_ofline_type$V_i)/max(dt_ofline_type$N_i))),
           stat = "identity", fill = "#b68e55", alpha = 1) +
  coord_flip() +
  scale_y_continuous(
    name = NULL,
    breaks = c(-0, -1e3, -2e3, -3e3, -4e3),
    labels = label_function_right,
    sec.axis = sec_axis(~ . * (max(dt_ofline_type$N_i)/max(dt_ofline_type$V_i)),
                        name = NULL,
                        breaks = c(0, 150, 300, 450, 550),
                        labels = label_function_left)
  ) +
  geom_text(data = data.table(x = c(2.5, 2.5), y = c(2000, -2000),
                              label = c("网下投资账户数量 (个)",
                                        "网下拟认购金额 (亿元)")),
            size = 4.5,
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = F) +
  labs(x = NULL, y = NULL) + #labs(x = "网下投资者类型") +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) -> fig_idist

dt_lm_str <- dt_ofline_i %>%
  mutate(plist = vlookup(fcode, dt_ofline_j[, .(fcode, plist)])) %>%
  mutate(N_i = unicnt(icode), V_i = sum(qty * plist)/10000, .by = c("fcode", "itype")) %>%
  select(fcode, fname, itype, N_i, V_i) %>% unique() %>%
  mutate(
    .by = "fcode",
    N_i_pct = N_i / sum(N_i),
    V_i_pct = V_i / sum(V_i),
  ) %>%
  filter(itype %in% c("保险机构", "券商自营", "公募基金专户", "券商资管计划", "集合信托计划", "私募基金")) %>%
  left_join(dt_ofline_j[, .(fcode, fclass, dxr_ofl, dxr_pub)], by = join_by(fcode)) %>%
  mutate(across(-c(fcode, fname, itype, fclass), ~ normalize(.)))

sapply(unique(dt_lm_str$itype), function(itype_i) {
  sapply(c("N_i", "N_i_pct", "V_i", "V_i_pct"), function(var_i) {
    sapply(c("ofl", "pub"), function(dxr_type) {
      ls_lm_prop <- lm(
        data = dt_lm_str[itype == itype_i & fclass == "产权类"],
        formula = as.formula(paste0("dxr_", dxr_type, "~", var_i))
      ) %>% summary()
      ls_lm_oprt <- lm(
        data = dt_lm_str[itype == itype_i & fclass == "经营权类"],
        formula = as.formula(paste0("dxr_", dxr_type, "~", var_i))
      ) %>% summary()
      ls_lm_pool <- lm(
        data = dt_lm_str[itype == itype_i],
        formula = as.formula(paste0("dxr_", dxr_type, "~", var_i))
      ) %>% summary()
      sapply(list(ls_lm_prop, ls_lm_oprt, ls_lm_pool), function(ls) {
        data.table(
          dxr_type = dxr_type, itype = itype_i, predictor = var_i,
          coef = ls$coefficients[var_i, "Estimate"],
          pvalue = ls$coefficients[var_i, "Pr(>|t|)"] %>% get_stars,
          R2 = ls$r.squared
        ) %>% return()
      }, simplify = FALSE) %>% rbindlist() %>%
        cbind(fclass = c("产权类", "经营权类", "全样本")) %>% return()
    }, simplify = FALSE) %>% rbindlist() %>% return()
  }, simplify = FALSE) %>% rbindlist() %>% return()
}, simplify = FALSE) %>% rbindlist() %>% filter(pvalue != " " & R2 >= 0.1) %>%
  arrange(-coef) %>%
  mutate(
    dxr_type = case_match(dxr_type, "ofl" ~ "网下", "pub" ~ "公众"),
    predictor = case_match(
      predictor,
      "N_i" ~ "账户数量", "V_i" ~ "认购金额",
      "N_i_pct" ~ "账户数量占比", "V_i_pct" ~ "认购金额占比"
    ),
    x_lab = paste0(dxr_type, "\n", itype, "\n", predictor, "\nR2 = ", round(R2, 3)),
    x_lab = factor(x_lab, levels = x_lab),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类"))
  ) %>% {
    ggplot(data = ., mapping = aes(x = x_lab, y = coef)) +
      geom_bar(aes(fill = predictor), stat = "identity", alpha = 1) +
      facet_wrap(~ fclass, scales = "free", ncol = 1) +
      scale_fill_manual(
        name = "",
        values = c("认购金额占比" = "#b68e55", "认购金额" = "#b68e55",
                   "账户数量占比" = "#5b0d07", "账户数量" = "#5b0d07")
      ) +
      labs(x = NULL, y = "回归系数 (标准化)") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14)
      ) +
      geom_text(
        aes(label = round(coef, 3), vjust = if_else(coef > 0, -.2, 1.2)),
        size = 4, position = position_dodge(width = 0.9)
      )
  } -> fig_lm_str

# 2. descriptive ------------------------------------------------
## 2.1. 持有时间 ------------------------------------------------
dt_dxr_D60 %>%
  filter(tt != 61) %>%
  select(fcode, fname, tt, starts_with("dxr_")) %>%
  left_join(dt_ofline_j[, .(fcode, restrict)], by = "fcode") %>%
  bind_rows(
    mutate(., restrict = "POOL")
  ) %>%
  pivot_longer(cols = starts_with("dxr_"),
               names_to = "rtype", values_to = "dxr") %>%
  mutate(
    dxr.mean = mean(dxr, na.rm = T),
    dxr.med  = median(dxr, na.rm = T),
    dxr_q1 = quantile(dxr, .25, na.rm = T),
    dxr_q3 = quantile(dxr, .75, na.rm = T),
    .by = c("restrict", "tt", "rtype")
  ) %>%
  mutate(
    restrict_tab = factor(
      restrict,
      levels = c("POOL", "FALSE", " TRUE"),
      labels = c("全样本", "无限售条款", "有限售条款")
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = tt, group = rtype, linetype = rtype)) +
      facet_wrap(~ restrict_tab, ncol = 1, scales = "free_y") +
      geom_line(mapping = aes(y = dxr.mean, color = "mean"), alpha = 1) +
      geom_line(mapping = aes(y = dxr.med, color = "med"), alpha = 1) +
      geom_hline(
        data = filter(., restrict != "FALSE"),
        mapping = aes(yintercept = 0), linetype = "dotted"
      ) +
      geom_vline(
        data = filter(., restrict == " TRUE"),
        mapping = aes(xintercept = 26), linetype = "dotted"
      ) +
      labs(x = "上市后天数", y = "(REITs) 打新收益率%") +
      scale_color_manual(name = "",
                         values = c("mean" = "#5b0d07", "med" = "#b68e55"),
                         labels = c("mean" = "均值", "med" = "中位数")) +
      scale_linetype_manual(name = "",
                            values = c("dxr_fc" = "solid", "dxr_wofc" = "dashed"),
                            labels = c("dxr_fc" = "含资金成本",
                                       "dxr_wofc" = "不含资金成本")) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_time_d60

dt_dxr_D60 %>%
  arrange(fcode, tt) %>%
  mutate(
    .by = c("fcode"),
    dxr_fc_dt0 = dxr_fc - first(dxr_fc),
    turn_positive = 1 * (dxr_fc > 0),
    turn_positive = turn_positive * ifelse(first(dxr_fc) > 0, NA, 1)
  ) %>%
  mutate(
    .by = c("tt"),
    win_vs_t0 = sum(dxr_fc_dt0 > 0) / n() * 100,
    win_buffer = mean(dxr_fc_dt0[dxr_fc_dt0 > 0]),
    turn_vs_t0 = sum(turn_positive > 0, na.rm = TRUE) / dt_dxr_D60[tt == 0, sum(dxr_fc < 0)] * 100
  ) %>%
  filter(tt != 0) %>%
  select(
    fname, tt, win_vs_t0, win_buffer, turn_vs_t0
  ) %>%
  mutate(
    tt = ifelse(tt == 61, 75, tt)
  ) %>%
  pivot_longer(
    cols = -c(fname, tt), names_to = "variable", values_to = "value"
  ) %>%
  mutate(
    v_tab = factor(
      variable,
      levels = c("win_vs_t0", "win_buffer", "turn_vs_t0"),
      labels = c("收益率超过首日概率%", "收益率增厚%", "负收益率转正概率%")
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = tt, y = value)) +
      facet_wrap(~ v_tab, ncol = 1, scales = "free_y") +
      geom_line(
        data = filter(., tt != 75),
        mapping = aes(color = variable), alpha = 1
      ) +
      geom_line(
        data = filter(., tt %in% c(60, 75)),
        linetype = "dashed",
        mapping = aes(color = variable), alpha = 1
      ) +
      geom_point(
        data = filter(., tt %in% c(75)),
        shape = 1, size = 1.5,
        mapping = aes(color = variable), alpha = 1
      ) +
      labs(x = "上市后天数", y = NULL) +
      scale_color_manual(
        values = c("win_vs_t0" = "#5b0d07", "win_buffer" = "#b68e55", "turn_vs_t0" = "#5b0d07")
      ) +
      scale_x_continuous(
        breaks = c(1, 15, 30, 45, 60, 75),
        labels = c(1, 15, 30, 45, 60, "2024-11-29")
      ) +
      guides(color = "none") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_time_d60_vst0

## 2.2. 分时间, 滚动均值 ------------------------------------------------
dt_ofline_j %>%
  arrange(Listdate, Xdate, fcode) %>%
  select(fcode, fname, Listdate, dxr_ofl, dxr_pub) %>%
  mutate(
    n_fund = row_number(),
    rollmean_ofl_15 = zoo::rollapply(dxr_ofl, width = 15, align = "right", FUN = mean, fill = NA),
    rollmean_pub_15 = zoo::rollapply(dxr_pub, width = 15, align = "right", FUN = mean, fill = NA),

    rollmean_ofl_20 = zoo::rollapply(dxr_ofl, width = 20, align = "right", FUN = mean, fill = NA),
    rollmean_pub_20 = zoo::rollapply(dxr_pub, width = 20, align = "right", FUN = mean, fill = NA),

    rollmean_ofl_25 = zoo::rollapply(dxr_ofl, width = 25, align = "right", FUN = mean, fill = NA),
    rollmean_pub_25 = zoo::rollapply(dxr_pub, width = 25, align = "right", FUN = mean, fill = NA),
  ) %>%
  pivot_longer(
    cols = starts_with("rollmean"), values_to = "rollmean", names_to = "variable"
  ) %>%
  filter(!is.na(rollmean)) %>%
  mutate(
    window_flag = str_sub(variable, -2, -1),
    window_flag = paste0("窗口宽度 = ", window_flag),
    type_flag = as.character(str_detect(variable, "ofl") * 1)
  ) %>% {
    ggplot(data = ., mapping = aes(x = n_fund, y = rollmean)) +
      geom_line(mapping = aes(group = variable, color = type_flag, linetype = type_flag)) +
      facet_wrap(~ window_flag, scales = "free_x") +
      scale_color_manual(
        name = NULL,
        values = c("1" = "#b68e55", "0" = "#5b0d07"), labels = c("1" = "网下投资者", "0" = "公众投资者")
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("1" = "solid", "0" = "dashed"), labels = c("1" = "网下投资者", "0" = "公众投资者")
      ) +
      labs(x = "REITs累计上市个数", y = "滚动平均打新收益率 %") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_dxr_rollmean

dt_ofline_j %>%
  arrange(Listdate, Xdate, fcode) %>%
  select(fcode, fname, Listdate, dxr_ofl, dxr_pub, dxr_ofl_norst) %>%
  mutate(
    n_fund = row_number(),
    rollmean_ofl = zoo::rollapply(dxr_ofl, width = 20, align = "right", FUN = mean, fill = NA),
    rollmean_pub = zoo::rollapply(dxr_pub, width = 20, align = "right", FUN = mean, fill = NA),
    rollmean_ofl_norst = zoo::rollapply(dxr_ofl_norst, width = 20, align = "right", FUN = mean, fill = NA),
  ) %>%
  pivot_longer(
    cols = starts_with("rollmean"), values_to = "rollmean", names_to = "variable"
  ) %>%
  filter(!is.na(rollmean)) %>% {
    ggplot(data = ., mapping = aes(x = n_fund, y = rollmean)) +
      geom_line(mapping = aes(group = variable, color = variable, linetype = variable)) +
      scale_color_manual(
        name = NULL,
        values = c("rollmean_ofl" = "#5b0d07", "rollmean_ofl_norst" = "#5b0d07",
                   "rollmean_pub" = "#b68e55"),
        labels = c("rollmean_ofl" = "网下打新收益率 - 实际", "rollmean_ofl_norst" = "网下打新收益率 - 无限售",
                   "rollmean_pub" = "公众打新收益率 - 实际")
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("rollmean_ofl" = 1, "rollmean_ofl_norst" = 2,
                   "rollmean_pub" = 1),
        labels = c("rollmean_ofl" = "网下打新收益率 - 实际", "rollmean_ofl_norst" = "网下打新收益率 - 无限售",
                   "rollmean_pub" = "公众打新收益率 - 实际")
      ) +
      labs(x = "REITs累计上市个数", y = "滚动平均打新收益率 %") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "inside",
        legend.position.inside = c(.21, .17),
        legend.background = element_rect(
          fill = "white", color = "gray", linewidth = 0.5, linetype = "solid"
        )
      )
  } -> fig_dxr_rollmean_v2

## 2.3. 分业态 ------------------------------------------------
dt_industry <- dt_ofline_j %>%
  mutate(
    dpc_1 = vlookup(fcode, dt_dxr_D60[tt == 0, .(fcode, pc)]),
    dpc_4 = vlookup(fcode, dt_dxr_D60[tt == 3, .(fcode, pc)]),
    across(starts_with("dpc_"), ~ (. / plist - 1) * 100)
  )

vec_y_lab <- c(
  "dxr_ofl" = "网下投资者打新收益率%",
  "dxr_pub" = "公众投资者打新收益率%",
  "dpc_1" = "上市首日价格涨幅%",
  "dpc_4" = "上市第4个交易日价格涨幅%"
)

ls_strip_title <- list(
  c("业态整体胜率%", "业态平均打新收益率%",
    "胜者组平均打新收益率%",
    "业态整体标准化盈亏比:\n平均盈利/(平均盈利 + 平均亏损)"),
  c("业态整体胜率%", "业态平均涨幅%",
    "胜者组平均涨幅%",
    "业态整体标准化盈亏比:\n平均涨幅/(平均涨幅 + 平均跌幅)")
) %>% rep(each = 2) %>%
  `names<-`(c("dxr_ofl", "dxr_pub", "dpc_1", "dpc_4"))

ls_fig_industry <- lapply(
  c("dxr_ofl", "dxr_pub", "dpc_1", "dpc_4"),
  function(var_y) {
    dt_industry %>%
      select(fname, ftype, dxr = all_of(var_y)) %>%
      mutate(
        ftype = recode(ftype,
                       "商业" = "消费", "物流园" = "仓储物流",
                       "保障性租赁住房" = "保租房"),
        ftype = factor(
          ftype,
          levels = rev(c("生态环保", "能源", "高速公路",
                         "消费", "产业园", "仓储物流", "保租房"))
        )
      ) %>%
      mutate(
        .by = c("ftype"),
        mean_dxr = mean(dxr, na.rm = T),
        wins.rate = round(sum(dxr > 0, na.rm = T) / sum(!is.na(dxr)) * 100, 2),
        win.dxr.mean = mean(dxr[dxr > 0], na.rm = T),
        loss.dxr.mean = mean(dxr[dxr < 0], na.rm = T),
        unf_rl = win.dxr.mean /
          (win.dxr.mean - ifelse(is.nan(loss.dxr.mean), 0, loss.dxr.mean))
      ) %>%
      select(ftype, mean_dxr, wins.rate, win.dxr.mean, unf_rl) %>%
      unique() %>%
      pivot_longer(
        cols = c(mean_dxr, wins.rate, win.dxr.mean, unf_rl),
        names_to = "variables",
        values_to = "values"
      ) %>%
      mutate(
        sign.fill = case_when(
          variables == "mean_dxr" & values > 0 ~ "H",
          variables == "wins.rate" & values > 50 ~ "H",
          variables == "win.dxr.mean" & values > 0 ~ "H",
          variables == "unf_rl" & values > 0.5 ~ "H",
          .default = "L"
        ),
        intercept = case_when(
          variables == "mean_dxr" ~ 0,
          variables == "wins.rate" ~ 50,
          variables == "unf_rl" ~ 0.5,
          .default = NA
        ),
        variables = factor(
          variables,
          levels = c("wins.rate", "mean_dxr", "win.dxr.mean", "unf_rl"),
          labels = ls_strip_title[[var_y]]
        )
      ) %>% {
        ggplot(data = ., mapping = aes(x = ftype)) +
          facet_wrap(~ variables, ncol = 4, scales = "free_y") +
          geom_bar(
            mapping = aes(y = values, fill = sign.fill),
            stat = "identity", position = "dodge"
          ) +
          geom_hline(
            mapping = aes(yintercept = intercept),
            linetype = "dashed"
          ) +
          scale_fill_manual(values = c("L" = "#b68e55", "H" = "#5b0d07")) +
          guides(fill = "none") +
          labs(x = NULL, y = vec_y_lab[var_y]) +
          theme_bw() +
          theme(strip.text = element_text(size = 12),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                axis.title.y = element_text(size = 14),
                plot.margin = margin(t = 5, r = 5, b = 21, l = 5))
      } -> fig_industry_i
    return(fig_industry_i)
  }
)

fig_industry_v2 <- plot_grid(
  ls_fig_industry[[1]], ls_fig_industry[[2]],
  ls_fig_industry[[3]], ls_fig_industry[[4]],
  ncol = 1, align = "v", axis = "t"
)

# 3. factor corr plot ------------------------------------------------
## 3.1. 参与热情 ------------------------------------------------
## 基金层面战略投资者类型（穿透）份额
as.data.table(dt_str)[type_pen %in% c("保险", "原始权益人或其关联方", "信托",
                                      "产业资本", "券商自营", "银行理财"),
                      .(fcode, type_pen, pct_type)] %>%
  mutate(type_pen = recode(type_pen,
                           "保险" = "bx_pct",
                           "原始权益人或其关联方" = "og_pct",
                           "信托" = "xt_pct",
                           "产业资本" = "cy_pct",
                           "券商自营" = "qs_pct",
                           "银行理财" = "yh_pct")) %>%
  unique() %>%
  pivot_wider(id_cols = fcode, names_from = type_pen, values_from = pct_type) %>%
  mutate(across(ends_with("pct"), ~ ifelse(is.na(.), 0, .))) -> dt_str_share

## 参与热情，即网下账户数量和网下认购金额与因子的corr
dt_participation <- dt_ofline_j %>%
  select(fcode, ofl_qty, ofl_num, plist, famt, public_amt, ends_with("place")) %>%
  mutate(public_pct = public_amt/famt * 100,
         str_pct    = 100 * str_place / (str_place + ofl_place + pub_place)) %>%
  select(!matches("(amt$|place$)")) %>%
  left_join(dt_str_share[, c("fcode", "bx_pct", "og_pct", "xt_pct",
                             "cy_pct", "qs_pct", "yh_pct")],
            by = "fcode") %>%
  pivot_longer(cols = ends_with("pct"),
               names_to = c("variable"), values_to = "value") %>%
  mutate(
    variable = recode(variable,
                      "public_pct" = "流通盘占比%",
                      "str_pct"    = "战配占比%",
                      "bx_pct"     = "保险战配占比%",
                      "og_pct"     = "原始权益人战配占比%",
                      "cy_pct"     = "产业资本战配占比%",
                      "yh_pct"     = "银行理财战配占比%",
                      "qs_pct"     = "券商自营战配占比%",
                      "xt_pct"     = "信托战配占比%")
  ) %>%
  mutate(corr = round(cor(value, ofl_num), 3), .by = variable) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("流通盘占比%",
               "原始权益人战配占比%",
               "保险战配占比%",
               "战配占比%",
               "券商自营战配占比%",
               "银行理财战配占比%",
               "产业资本战配占比%",
               "信托战配占比%"),
    value = corr
  ))

## 绘图：网下热情与相关因子
##       注意ofl_qty在 <1. j-level, renewed> 中已经乘以发行价, 是金额而非份额
ggplot(data = filter(dt_participation,
                     str_detect(variable, "^(流通盘占比|战配占比).*")),
       mapping = aes(x = value)) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_point(mapping = aes(y = ofl_qty/1e4), shape = 1) +
  geom_smooth(mapping = aes(y = ofl_qty/1e4), se = T, linewidth = .5,
              method = "loess", alpha = 0.2) +
  # labs(x = "配售比例", y = "网下（入围）投资者账户数量") +
  labs(x = "配售比例", y = "网下（入围）拟认购金额 (亿元)") +
  guides(fill = "none") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 14)) +
  theme_minimal() -> fig_participation

ggplot(data = filter(dt_participation,
                     str_detect(variable, "^(流通盘占比|战配占比).*", negate = T)),
       mapping = aes(x = value)) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_point(mapping = aes(y = ofl_qty/1e4), shape = 1) +
  geom_smooth(mapping = aes(y = ofl_qty/1e4), se = T, linewidth = .5,
              method = "loess", alpha = 0.2) +
  # labs(x = "配售比例", y = "网下（入围）投资者账户数量") +
  labs(x = "配售比例", y = "网下（入围）拟认购金额 (亿元)") +
  guides(fill = "none") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 14)) +
  theme_minimal() -> fig_participation_str

## 3.2. 战配结构指标 ------------------------------------------------
## 绘图：打新收益率与战略投资者
dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl,
         famt, public_amt, famt_persh) %>%
  mutate(public_pct = public_amt/famt * 100) %>%
  left_join(
    select(dt_str_share, fcode, bx_pct, og_pct, yh_pct, qs_pct, cy_pct),
    by = "fcode"
  ) %>%
  mutate(mkt_pct = 100 - cy_pct - og_pct) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "famt" = "拟募集金额 (万元)",
                      "public_amt" = "流通盘金额 (万元)",
                      "mkt_pct" = "市场化战配占比%",
                      "og_pct" = "原始权益人战配占比%",
                      "famt_persh" = "拟募集金额 per share",
                      "public_pct" = "流通盘金额占比%",
                      "bx_pct" = "保险战配占比%",
                      "qs_pct" = "券商自营战配占比%",
                      "yh_pct" = "银行理财战配占比%")
  ) %>%
  filter(!(variable %in% c("拟募集金额 (万元)", "拟募集金额 per share",
                           "流通盘金额 (万元)", "cy_pct"))) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("流通盘金额占比%",
               "市场化战配占比%",
               "原始权益人战配占比%",
               "券商自营战配占比%",
               "保险战配占比%",
               "银行理财战配占比%"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7,
                  method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_str

dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, dxr = dxr_pub,
         famt, public_amt, famt_persh) %>%
  mutate(public_pct = public_amt/famt * 100) %>%
  left_join(
    select(dt_str_share, fcode, bx_pct, og_pct, yh_pct, qs_pct, cy_pct),
    by = "fcode"
  ) %>%
  mutate(mkt_pct = 100 - cy_pct - og_pct) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "famt" = "拟募集金额 (万元)",
                      "public_amt" = "流通盘金额 (万元)",
                      "mkt_pct" = "市场化战配占比%",
                      "og_pct" = "原始权益人战配占比%",
                      "famt_persh" = "拟募集金额 per share",
                      "public_pct" = "流通盘金额占比%",
                      "bx_pct" = "保险战配占比%",
                      "qs_pct" = "券商自营战配占比%",
                      "yh_pct" = "银行理财战配占比%")
  ) %>%
  filter(!(variable %in% c("拟募集金额 (万元)", "拟募集金额 per share",
                           "流通盘金额 (万元)", "cy_pct"))) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("流通盘金额占比%",
               "市场化战配占比%",
               "原始权益人战配占比%",
               "券商自营战配占比%",
               "保险战配占比%",
               "银行理财战配占比%"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7,
                  method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_str

## 3.3. 网下指标 ------------------------------------------------
## 绘图：打新收益率与网下指标（网下倍数等）
dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl,
         famt_persh, ofl_multi, ofrate, p_up, p_low, ofl_num) %>%
  mutate(p_med = (p_up + p_low) / 2,
         ofrate = 100 * as.numeric(ofrate)) %>%
  mutate(across(c(p_up, p_med, p_low), ~ . / famt_persh)) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  filter(!(variable == "famt_persh")) %>%
  filter(!(variable == "ofl_multi" & value > 200)) %>%
  filter(!(variable == "ofl_num" & value > 400)) %>%
  mutate(
    variable = recode(variable,
                      "ofl_multi" = "网下倍数",
                      "ofl_num" = "网下账户数量",
                      "ofrate" = "网下认购确认比例%",
                      "p_up" = "询价区间上限 / 拟募集金额 per share",
                      "p_med" = "询价中位数 / 拟募集金额 per share",
                      "p_low" = "询价区间下限 / 拟募集金额 per share")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("网下倍数",
               "网下账户数量",
               "网下认购确认比例%",
               "询价区间上限 / 拟募集金额 per share",
               "询价中位数 / 拟募集金额 per share",
               "询价区间下限 / 拟募集金额 per share"),
    value = corr2
  )) %>%
  filter(str_detect(variable, "询价", negate = T)) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_offline

dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, dxr = dxr_pub,
         famt_persh, ofl_multi, ofrate, p_up, p_low, ofl_num) %>%
  mutate(p_med = (p_up + p_low) / 2,
         ofrate = 100 * as.numeric(ofrate)) %>%
  mutate(across(c(p_up, p_med, p_low), ~ . / famt_persh)) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  filter(!(variable == "famt_persh")) %>%
  filter(!(variable == "ofl_multi" & value > 200)) %>%
  filter(!(variable == "ofl_num" & value > 400)) %>%
  mutate(
    variable = recode(variable,
                      "ofl_multi" = "网下倍数",
                      "ofl_num" = "网下账户数量",
                      "ofrate" = "网下认购确认比例%",
                      "p_up" = "询价区间上限 / 拟募集金额 per share",
                      "p_med" = "询价中位数 / 拟募集金额 per share",
                      "p_low" = "询价区间下限 / 拟募集金额 per share")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("网下倍数",
               "网下账户数量",
               "网下认购确认比例%",
               "询价区间上限 / 拟募集金额 per share",
               "询价中位数 / 拟募集金额 per share",
               "询价区间下限 / 拟募集金额 per share"),
    value = corr2
  )) %>%
  filter(str_detect(variable, "询价", negate = T)) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_offline

## 3.4. 询价指标 ------------------------------------------------
## 绘图：打新收益率与询价指标（询价区间等）
dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl,
         famt_persh, plist, p_up, p_low) %>%
  mutate(p_list_quantile = 100 * (plist - p_low) / (p_up - p_low),
         p_med = (p_up + p_low) / 2) %>%
  mutate(across(c(p_up, p_med, p_low), ~ . / famt_persh)) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  filter(variable %in% c("p_up", "p_med", "p_low", "p_list_quantile")) %>%
  filter(!(variable == "p_up" & value < 1)) %>%
  filter(!(variable == "p_low" & value < 0.9)) %>%
  filter(!(variable == "p_med" & value < 0.92)) %>%
  filter(variable != "p_list_quantile") %>%
  mutate(
    variable = recode(variable,
                      # "p_list_quantile" = "发行价占询价区间分位数%",
                      "p_up" = "询价区间上限 / 拟募集金额 per share",
                      "p_med" = "询价区间中位数 / 拟募集金额 per share",
                      "p_low" = "询价区间下限 / 拟募集金额 per share")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c(
      "询价区间上限 / 拟募集金额 per share",
      "询价区间中位数 / 拟募集金额 per share",
      "询价区间下限 / 拟募集金额 per share"
      #"发行价占询价区间分位数%"
    ),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", nrow = 1) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_query

dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, dxr = dxr_pub,
         famt_persh, plist, p_up, p_low) %>%
  mutate(p_list_quantile = 100 * (plist - p_low) / (p_up - p_low),
         p_med = (p_up + p_low) / 2) %>%
  mutate(across(c(p_up, p_med, p_low), ~ . / famt_persh)) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  filter(variable %in% c("p_up", "p_med", "p_low", "p_list_quantile")) %>%
  filter(!(variable == "p_up" & value < 1)) %>%
  filter(!(variable == "p_low" & value < 0.9)) %>%
  filter(!(variable == "p_med" & value < 0.92)) %>%
  mutate(
    variable = recode(variable,
                      "p_list_quantile" = "发行价占询价区间分位数%",
                      "p_up" = "询价区间上限 / 拟募集金额 per share",
                      "p_med" = "询价区间中位数 / 拟募集金额 per share",
                      "p_low" = "询价区间下限 / 拟募集金额 per share")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c(
      "询价区间上限 / 拟募集金额 per share",
      "询价区间中位数 / 拟募集金额 per share",
      "询价区间下限 / 拟募集金额 per share",
      "发行价占询价区间分位数%"
    ),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", nrow = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_query

## 3.5. 估值指标 ------------------------------------------------
## 绘图：打新收益率与分派率/期限/IRR
dt_ofline_j %>%
  select(fcode, fname, fclass, dxr = dxr_ofl,
         dividend, avg_smdivd, term, avg_term,
         irr, avg_irr_sm,
         p_up, p_low, famt_persh) %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_term = term - avg_term,
         dif_irr  = irr - avg_irr_sm) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "dif_divd" = "预测分派率% - 板块二级市场均值",
                      "dif_term" = "剩余期限(年) - 板块均值",
                      "dif_irr"  = "0增速IRR% - 板块二级市场均值")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("预测分派率% - 板块二级市场均值",
               "剩余期限(年) - 板块均值",
               "0增速IRR% - 板块二级市场均值"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_value

dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fname, fclass, dxr = dxr_pub, term, avg_term) %>%
  left_join(
    select(dt_public_j, fcode, dividend, avg_smdivd, irr, avg_irr_sm),
    by = "fcode"
  ) %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_term = term - avg_term,
         dif_irr  = irr - avg_irr_sm) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "dif_divd" = "预测分派率% - 板块二级市场均值",
                      "dif_term" = "剩余期限(年) - 板块均值",
                      "dif_irr"  = "0增速IRR% - 板块二级市场均值")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("预测分派率% - 板块二级市场均值",
               "剩余期限(年) - 板块均值",
               "0增速IRR% - 板块二级市场均值"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_value

## 3.6. 板块指标 ------------------------------------------------
## 绘图：打新收益率与板块指数
dt_ofline_j %>%
  select(fcode, fclass, ftype, dxr = dxr_ofl, Xdate) %>%
  mutate(
    ftype = recode(ftype,
                   "商业" = "消费", "物流园" = "仓储物流",
                   "保障性租赁住房" = "保租房"),
  ) %>%
  left_join(
    select(dt_sector, date, ftype,
           secind_20d, secind_60d,
           index_20d, index_60d),
    by = join_by(Xdate == date, ftype == ftype)
  ) %>%
  select(-ends_with("date")) %>%
  pivot_longer(cols = -c(fcode, fclass, ftype, dxr),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "secind_20d" = "板块指数询价日前20日涨跌幅",
                      "secind_60d" = "板块指数询价日前60日涨跌幅",
                      "index_20d" = "中金C-REITs指数询价日前20日涨跌幅",
                      "index_60d" = "中金C-REITs指数询价日前60日涨跌幅")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("板块指数询价日前20日涨跌幅",
               "板块指数询价日前60日涨跌幅",
               "中金C-REITs指数询价日前20日涨跌幅",
               "中金C-REITs指数询价日前60日涨跌幅"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_sector

dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, ftype, dxr = dxr_pub, Tdate) %>%
  mutate(
    ftype = recode(ftype,
                   "商业" = "消费", "物流园" = "仓储物流",
                   "保障性租赁住房" = "保租房"),
  ) %>%
  left_join(
    select(dt_sector, date, ftype,
           secind_20d, secind_60d,
           index_20d, index_60d),
    by = join_by(Tdate == date, ftype == ftype)
  ) %>%
  select(-ends_with("date")) %>%
  pivot_longer(cols = -c(fcode, fclass, ftype, dxr),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "secind_20d" = "板块指数T日前20日涨跌幅",
                      "secind_60d" = "板块指数T日前60日涨跌幅",
                      "index_20d" = "中金C-REITs指数T日前20日涨跌幅",
                      "index_60d" = "中金C-REITs指数T日前60日涨跌幅")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("板块指数T日前20日涨跌幅",
               "板块指数T日前60日涨跌幅",
               "中金C-REITs指数T日前20日涨跌幅",
               "中金C-REITs指数T日前60日涨跌幅"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_sector

## 3.7. 规模指标 ------------------------------------------------
dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl) %>%
  left_join(
    dt_famt, by = join_by(fcode)
  ) %>%
  pivot_longer(cols = c(famt, pubfamt),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable, "famt" = "拟募集资金 (亿元)", "pubfamt" = "拟流通盘规模 (亿元)")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("拟募集资金 (亿元)", "拟流通盘规模 (亿元)"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr, group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(网下投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_ofl_amt

dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_pub) %>%
  left_join(
    dt_famt, by = join_by(fcode)
  ) %>%
  pivot_longer(cols = c(famt_act, pubfamt_act),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable, "famt_act" = "实际募集资金 (亿元)", "pubfamt_act" = "实际流通盘规模 (亿元)")
  ) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(corr = round(cor(value, dxr), 3), .by = c("variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = variable
  ) %>%
  mutate(variable = factor_with_valuelabel(
    variable,
    levels = c("实际募集资金 (亿元)", "实际流通盘规模 (亿元)"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr, group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "(公众投资者) 打新收益率 %") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pub_amt

# 4. factor reg ------------------------------------------------
## 4.1. one-factor ------------------------------------------------
## 单变量依次加入做因子

dt_lm_ofl <- dt_ofline_j %>%
  select(fcode, fclass, ftype, dxr = dxr_ofl, Xdate, ofl_multi,
         plist, term, avg_term, dividend, avg_smdivd, irr, avg_irr_sm,
         famt, public_amt, p_up, p_low, famt_persh) %>%
  left_join(
    select(
      dt_sector, date, ftype,
      secind_20d, index_20d, secind_60d, index_60d,
      index_60d_fst, index_60dm, secind_60d_fst, secind_60dm
    ),
    by = join_by(Xdate == date, ftype == ftype)
  ) %>%
  left_join(
    select(dt_famt, fcode, pubfamt), by = "fcode"
  ) %>%
  mutate(
    dif_divd = dividend - avg_smdivd,
    dif_term = term - avg_term,
    dif_irr = irr - avg_irr_sm,
    public_pct = public_amt / famt * 100,
    p_list_quantile = 100 * (plist - p_low) / (p_up - p_low),
    p_up = p_up / famt_persh,
    p_low = p_low / famt_persh,
    p_med = (p_up + p_low) / 2,
    og_pct = vlookup(fcode, dt_str_share[, c("fcode", "og_pct")])
  ) %>%
  select(
    fcode, fclass, dxr,
    secind_60d, index_60d,
    p_up, p_med, public_pct,
    dif_divd, dif_term, dif_irr, og_pct,
    famt, pubfamt
    #, ofl_multi, p_list_quantile
  ) %>% mutate(across(-c(fcode, fclass), ~ normalize(.)))

dt_lm_pub <- dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, ftype, dxr = dxr_pub, Tdate,
         plist, term, avg_term,
         famt, public_amt, p_up, p_low, ofl_multi, ofl_num) %>%
  left_join(
    select(dt_public_j, fcode, dividend, avg_smdivd, irr, avg_irr_sm),
    by = "fcode"
  ) %>%
  left_join(
    select(dt_sector, date, ftype, secind_60d, index_60d),
    by = join_by(Tdate == date, ftype == ftype)
  ) %>%
  left_join(
    select(dt_famt, fcode, famt_act, pubfamt_act), by = "fcode"
  ) %>%
  mutate(
    dif_divd = dividend - avg_smdivd,
    dif_term = term - avg_term,
    dif_irr = irr - avg_irr_sm,
    public_pct = public_amt / famt * 100,
    p_list_quantile = 100 * (plist - p_low) / (p_up - p_low),
    ofl_multi = ofl_multi,
    og_pct = vlookup(fcode, dt_str_share[, c("fcode", "og_pct")]),
    cy_pct = vlookup(fcode, dt_str_share[, c("fcode", "cy_pct")]),
    mkt_pct = 100 - cy_pct - og_pct
  ) %>%
  select(
    fcode, fclass, dxr,
    secind_60d, index_60d,
    p_list_quantile, public_pct, ofl_multi,
    dif_divd, dif_term, dif_irr, og_pct, mkt_pct, ofl_num,
    famt_act, pubfamt_act
  ) %>% mutate(across(-c(fcode, fclass), ~ normalize(.)))

vec_predictors_chn <- c(
  "dif_divd" = "预测分派率%-\n板块二级市场均值",
  "dif_term" = "剩余期限(年)-\n板块均值",
  "dif_irr" = "0增速IRR%-\n板块二级市场均值",
  "public_pct" = "流通盘占比%",
  "p_list_quantile" = "发行价\n询价区间分位数%",
  "p_up" = "询价区间上限/\n拟聚集金额 per share",
  "p_med" = "询价中位数/\n拟募集金额 per share",
  #"p_low" = "询价区间下限/\n拟聚集金额 per share",
  "ofl_multi" = "网下有效认购倍数",
  "ofl_num" = "网下有效认购账户数",
  "secind_20d" = "板块指数\n询价日前20日涨跌幅",
  "secind_60d" = "板块指数\n询价日前60日涨跌幅",
  "index_20d" = "中金C-REITs指数\n询价日前20日涨跌幅",
  "index_60d" = "中金C-REITs指数\n询价日前60日涨跌幅",
  "index_60d_fst" = "中金C-REITs指数\n询价日前60日(取短)涨跌幅",
  "index_60dm" = "中金C-REITs指数\n询价日前60日涨跌幅均值",
  "secind_60d_fst" = "板块指数\n询价日前60日(取短)涨跌幅",
  "secind_60dm" = "板块指数\n询价日前60日涨跌幅均值",
  "og_pct" = "原始权益人战配占比%",
  "mkt_pct" = "市场化战配占比%",
  "ofrate" = "网下确认比例%",
  "pbrate" = "公众确认比例%",
  "dpc_1" = "上市首日收盘价涨跌幅%",
  "dpc_4" = "上市第4收盘价涨跌幅%",
  "famt" = "拟募集资金 (亿元)",
  "pubfamt" = "拟流通盘规模 (亿元)",
  "famt_act" = "实际募集资金 (亿元)",
  "pubfamt_act" = "实际流通盘规模 (亿元)"
)

### 4.1.1. one-factor, linear ------------------------------------------------
### 仅一次项的回归
### 网下投资收益率

vec_predictors_chn %<>%
  str_replace_all("T日", "询价日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_linear_ofl <- lapply(
  names(dt_lm_ofl)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste("dxr ~", predictor))
    reg_uni_prop <- lm(formula, data = dt_lm_ofl[fclass == "产权类", ]) %>% summary()
    reg_uni_oprt <- lm(formula, data = dt_lm_ofl[fclass == "经营权类", ]) %>% summary()
    reg_uni_pool <- lm(formula, data = dt_lm_ofl) %>% summary()
    
    lapply(list(reg_uni_prop, reg_uni_oprt, reg_uni_pool), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>% rbindlist() %>%
      mutate(
        fclass = c("产权类", "经营权类", "全样本")
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(!(predictor %in% c("p_low"))) %>%
  pivot_longer(cols = c(coef),
               names_to = "variable",
               values_to = "value") %>%
  mutate(stars = ifelse(variable == "coef",
                        as.character(pvalue), as.character(pvalue_2))) %>%
  mutate(variable = ifelse(variable == "coef", "Linear", "Quadratic")) %>%
  select(!starts_with("pval")) %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  arrange(-R2) %>%
  mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
  mutate(value = ifelse(stars == " ", 0, value))

ls_fig_lm_linear_ofl <- lapply(c("产权类", "经营权类", "全样本"), function(ft) {
  ggplot(filter(dt_lm_linear_ofl, fclass == ft),
         mapping = aes(x = predictor, y = value, fill = fclass)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_fill_manual(name = "",
                      values = c("全样本" = "#5b0d07",
                                 "产权类" = "#b68e55",
                                 "经营权类" = "#b68e55")) +
    guides(fill = "none") +
    labs(
      # x = expression("(标准化后) 因子, 单变量" ~ R^2),
      x = NULL,
      y = paste0("回归系数", " - ", ft)
    ) +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 14),
      plot.margin = margin(10, 5, 5, 5)
    ) +
    geom_text(aes(label = round(value, 3),
                  vjust = if_else(value > 0, -.2, 1.2)),
              size = 3,
              position = position_dodge(width = 0.9)) %>% return()
})

comb_fig_lm_linear_ofl <- plot_grid(
  ls_fig_lm_linear_ofl[[3]], ls_fig_lm_linear_ofl[[1]], ls_fig_lm_linear_ofl[[2]],
  ncol = 1, align = "v", axis = "t"
)

comb_fig_lm_linear_ofl <- ggdraw() +
  draw_plot(comb_fig_lm_linear_ofl, x = 0, y = 0.05, width = 1, height = 0.95) +
  draw_label(expression("网下投资者 (标准化后) 因子, 单变量" ~ R^2),
             size = 14, x = 0.5, y = 0.02)

### 公众投资收益率
vec_predictors_chn %<>%
  str_replace_all("询价日", "T日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_linear_pub <- lapply(
  names(dt_lm_pub)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste("dxr ~", predictor))
    reg_uni_prop <- lm(formula, data = dt_lm_pub[fclass == "产权类", ]) %>% summary()
    reg_uni_oprt <- lm(formula, data = dt_lm_pub[fclass == "经营权类", ]) %>% summary()
    reg_uni_pool <- lm(formula, data = dt_lm_pub) %>% summary()
    
    lapply(list(reg_uni_prop, reg_uni_oprt, reg_uni_pool), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>% rbindlist() %>%
      mutate(
        fclass = c("产权类", "经营权类", "全样本")
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(!(predictor %in% c("p_low"))) %>%
  pivot_longer(cols = c(coef),
               names_to = "variable",
               values_to = "value") %>%
  mutate(stars = ifelse(variable == "coef",
                        as.character(pvalue), as.character(pvalue_2))) %>%
  mutate(variable = ifelse(variable == "coef", "Linear", "Quadratic")) %>%
  select(!starts_with("pval")) %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  arrange(-R2) %>%
  mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
  mutate(value = ifelse(stars == " ", 0, value))

ls_fig_lm_linear_pub <- lapply(c("产权类", "经营权类", "全样本"), function(ft) {
  ggplot(filter(dt_lm_linear_pub, fclass == ft),
         mapping = aes(x = predictor, y = value, fill = fclass)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_fill_manual(name = "",
                      values = c("全样本" = "#5b0d07",
                                 "产权类" = "#b68e55",
                                 "经营权类" = "#b68e55")) +
    guides(fill = "none") +
    labs(
      # x = expression("(标准化后) 因子, 单变量" ~ R^2),
      x = NULL,
      y = paste0("回归系数", " - ", ft)
    ) +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 14),
      plot.margin = margin(10, 5, 5, 5)
    ) +
    geom_text(aes(label = round(value, 3),
                  vjust = if_else(value > 0, -.2, 1.2)),
              size = 3,
              position = position_dodge(width = 0.9)) %>% return()
})

comb_fig_lm_linear_pub <- plot_grid(
  ls_fig_lm_linear_pub[[3]], ls_fig_lm_linear_pub[[1]], ls_fig_lm_linear_pub[[2]],
  ncol = 1, align = "v", axis = "t"
)

comb_fig_lm_linear_pub <- ggdraw() +
  draw_plot(comb_fig_lm_linear_pub, x = 0, y = 0.05, width = 1, height = 0.95) +
  draw_label(expression("公众投资者 (标准化后) 因子, 单变量" ~ R^2),
             size = 14, x = 0.5, y = 0.02)

### 4.1.2. one-factor, quadratic ------------------------------------------------
### 包含二次项的回归
### 网下投资者
vec_predictors_chn %<>%
  str_replace_all("T日", "询价日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_quadratic_ofl <- lapply(
  names(dt_lm_ofl)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste("dxr ~", predictor, "+I(", predictor, "^2)"))
    reg_uni_prop <- lm(formula, data = dt_lm_ofl[fclass == "产权类", ]) %>% summary()
    reg_uni_oprt <- lm(formula, data = dt_lm_ofl[fclass == "经营权类", ]) %>% summary()
    reg_uni_pool <- lm(formula, data = dt_lm_ofl) %>% summary()
    
    lapply(list(reg_uni_prop, reg_uni_oprt, reg_uni_pool), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        coef_2 = lmdt$coefficients[paste0("I(", predictor, "^2)"), "Estimate"],
        pvalue_2 = lmdt$coefficients[paste0("I(", predictor, "^2)"),
                                     "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>% rbindlist() %>%
      mutate(
        fclass = c("产权类", "经营权类", "全样本")
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(!(predictor %in% c("p_low"))) %>%
  pivot_longer(cols = c(coef, coef_2),
               names_to = "variable",
               values_to = "value") %>%
  mutate(stars = ifelse(variable == "coef",
                        as.character(pvalue), as.character(pvalue_2))) %>%
  mutate(variable = ifelse(variable == "coef", "Linear", "Quadratic")) %>%
  select(!starts_with("pval")) %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  arrange(-R2) %>%
  mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
  mutate(value = ifelse(stars == " ", 0, value))

ls_fig_lm_quadratic_ofl <- lapply(c("产权类", "经营权类", "全样本"), function(ft) {
  ggplot(filter(dt_lm_quadratic_ofl, fclass == ft),
         mapping = aes(x = predictor, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_fill_manual(name = "",
                      values = c("Linear" = "#5b0d07", "Quadratic" = "#b68e55")) +
    labs(
      # x = expression("(标准化后) 因子, 单变量" ~ R^2),
      x = NULL,
      y = paste0("回归系数", " - ", ft)
    ) +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(10, 5, 5, 5)
    ) +
    geom_text(aes(label = round(value, 3),
                  vjust = if_else(value > 0, -.2, 1.2)),
              size = 3,
              position = position_dodge(width = 0.9)) %>% return()
})

comb_fig_lm_quadratic_ofl <- plot_grid(
  ls_fig_lm_quadratic_ofl[[3]], ls_fig_lm_quadratic_ofl[[1]], ls_fig_lm_quadratic_ofl[[2]],
  ncol = 1, align = "v", axis = "t"
)

comb_fig_lm_quadratic_ofl <- ggdraw() +
  draw_plot(comb_fig_lm_quadratic_ofl, x = 0, y = 0.05, width = 1, height = 0.95) +
  draw_label(expression("网下投资者 (标准化后) 因子, 单变量" ~ R^2),
             size = 14, x = 0.5, y = 0.02)

### 公众投资者
vec_predictors_chn %<>%
  str_replace_all("询价日", "T日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_quadratic_pub <- lapply(
  names(dt_lm_pub)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste("dxr ~", predictor, "+I(", predictor, "^2)"))
    reg_uni_prop <- lm(formula, data = dt_lm_pub[fclass == "产权类", ]) %>% summary()
    reg_uni_oprt <- lm(formula, data = dt_lm_pub[fclass == "经营权类", ]) %>% summary()
    reg_uni_pool <- lm(formula, data = dt_lm_pub) %>% summary()
    
    lapply(list(reg_uni_prop, reg_uni_oprt, reg_uni_pool), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        coef_2 = lmdt$coefficients[paste0("I(", predictor, "^2)"), "Estimate"],
        pvalue_2 = lmdt$coefficients[paste0("I(", predictor, "^2)"),
                                     "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>% rbindlist() %>%
      mutate(
        fclass = c("产权类", "经营权类", "全样本")
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(!(predictor %in% c("p_low"))) %>%
  pivot_longer(cols = c(coef, coef_2),
               names_to = "variable",
               values_to = "value") %>%
  mutate(stars = ifelse(variable == "coef",
                        as.character(pvalue), as.character(pvalue_2))) %>%
  mutate(variable = ifelse(variable == "coef", "Linear", "Quadratic")) %>%
  select(!starts_with("pval")) %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  arrange(-R2) %>%
  mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
  mutate(value = ifelse(stars == " ", 0, value))

ls_fig_lm_quadratic_pub <- lapply(c("产权类", "经营权类", "全样本"), function(ft) {
  ggplot(filter(dt_lm_quadratic_pub, fclass == ft),
         mapping = aes(x = predictor, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_fill_manual(name = "",
                      values = c("Linear" = "#5b0d07", "Quadratic" = "#b68e55")) +
    labs(
      # x = expression("(标准化后) 因子, 单变量" ~ R^2),
      x = NULL,
      y = paste0("回归系数", " - ", ft)
    ) +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(10, 5, 5, 5)
    ) +
    geom_text(aes(label = round(value, 3),
                  vjust = if_else(value > 0, -.2, 1.2)),
              size = 3,
              position = position_dodge(width = 0.9)) %>% return()
})

comb_fig_lm_quadratic_pub <- plot_grid(
  ls_fig_lm_quadratic_pub[[3]], ls_fig_lm_quadratic_pub[[1]], ls_fig_lm_quadratic_pub[[2]],
  ncol = 1, align = "v", axis = "t"
)

comb_fig_lm_quadratic_pub <- ggdraw() +
  draw_plot(comb_fig_lm_quadratic_pub, x = 0, y = 0.05, width = 1, height = 0.95) +
  draw_label(expression("公众投资者 (标准化后) 因子, 单变量" ~ R^2),
             size = 14, x = 0.5, y = 0.02)

## 4.2. term and divd ------------------------------------------------
## 回归1：分派率和年限做因子
dt_ofline_lm_sm <- dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl,
         dividend, avg_smdivd, term, avg_term) %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_term = term - avg_term) %>% setDT()

### 按产权/经营权分样本
lm(data = dt_ofline_lm_sm[fclass == "经营权类", ],
   dxr ~ dif_divd + I(dif_divd ^ 2) + dif_term + I(dif_term ^ 2) +
     I(dif_divd * dif_term)) %>%
  summary() -> lmdivd_oprt
lm(data = dt_ofline_lm_sm[fclass == "产权类", ],
   dxr ~ dif_divd + I(dif_divd ^ 2) + dif_term + I(dif_term ^ 2) +
     I(dif_divd * dif_term)) %>%
  summary() -> lmdivd_prop
lm(data = dt_ofline_lm_sm,
   dxr ~ dif_divd + I(dif_divd ^ 2) + dif_term + I(dif_term ^ 2) +
     I(dif_divd * dif_term)) %>%
  summary() -> lmdivd_pool

vec_lm_var <- factor(
  c("(Intercept)", "dividend", "I(dividend^2)",
    "term", "I(term^2)", "I(term * dividend)"),
  levels = c("(Intercept)", "dividend", "I(dividend^2)",
             "term", "I(term^2)", "I(term * dividend)")
)

### 提取参数
dt_lmdivd <- data.table(
  model = rep(c("经营权类", "产权类"), each = 6),
  variable = rep(vec_lm_var, 2),
  value = c(lmdivd_oprt$coefficients[1:6, 1], lmdivd_prop$coefficients[1:6, 1]),
  R2 = rep(c(lmdivd_oprt$r.squared, lmdivd_prop$r.squared), each = 6)
) %>%
  `[`(, model_2 := paste0(model, "\nR2=", round(R2, 3)))

ggplot(dt_lmdivd,
       aes(x = variable, y = value, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ model_2) +
  theme_minimal() +
  scale_fill_manual(name = NULL,
                    values = c("产权类" = "#5b0d07",
                               "经营权类" = "#b68e55")) +
  scale_x_discrete(labels = c(
    "(Intercept)" = "截距项",
    "dividend" = "首年预测分派率差",
    "I(dividend^2)" = expression(`首年预测分派率差`^2),
    "term" = "剩余年限差",
    "I(term^2)" = expression(`剩余年限差`^2),
    "I(term * dividend)" = expression(`首年预测分派率差` %*% `剩余年限差`)
  )) +
  labs(y = "线性回归系数", x = NULL) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(value, 2),
                vjust = if_else(value > 0, -.5, 1.2)),
            size = 3,
            position = position_dodge(width = 0.9)) -> fig_lm_divd

# 5. posterior ------------------------------------------------
## 5.1. 网下posterior ------------------------------------------------
### posterior 数据
dt_posterior <- dt_ofline_j %>%
  select(
    fcode, fclass, ftype, Tdate,
    ofl_multi, ofl_num, ofrate, plist, p_up, p_low,
    term, avg_term, famt, public_amt,
    pbrate
  ) %>%
  left_join(
    select(dt_public_j, fcode, dividend, avg_smdivd, irr, avg_irr_sm),
    by = "fcode"
  ) %>%
  left_join(
    select(dt_sector, date, ftype, secind_60d, index_60d),
    by = join_by(Tdate == date, ftype == ftype)
  ) %>%
  mutate(
    dpc_1 = vlookup(fcode, dt_dxr_D60[tt == 0, .(fcode, pc)]),
    dpc_4 = vlookup(fcode, dt_dxr_D60[tt == 3, .(fcode, pc)]),
    dpc_20 = vlookup(fcode, dt_dxr_D60[tt == 19, .(fcode, pc)]),
    dpc_60 = vlookup(fcode, dt_dxr_D60[tt == 59, .(fcode, pc)]),
  ) %>%
  mutate(
    dif_divd = dividend - avg_smdivd,
    dif_term = term - avg_term,
    dif_irr = irr - avg_irr_sm,
    public_pct = public_amt / famt * 100,
    p_list_quantile = 100 * (plist - p_low) / (p_up - p_low),
    ofl_multi = ofl_multi,
    across(c(ofrate, pbrate), ~ as.numeric(.) * 100),
    og_pct = vlookup(fcode, dt_str_share[, c("fcode", "og_pct")]),
    cy_pct = vlookup(fcode, dt_str_share[, c("fcode", "cy_pct")]),
    mkt_pct = 100 - cy_pct - og_pct,
    across(starts_with("dpc_"), ~ (. / plist - 1) * 100)
  ) %>%
  select(
    -c(Tdate, p_low, p_up, plist, famt, public_amt, cy_pct,
       term, avg_term, dividend, avg_smdivd, irr, avg_irr_sm)
  ) %>%
  left_join(
    select(dt_famt, fcode, famt_act, pubfamt_act), by = "fcode"
  )

vec_predictors_chn %<>%
  str_replace_all("询价日", "T日") %>%
  `names<-`(names(vec_predictors_chn))

### 单因子回归 (仅线性), t = 1, 4
dt_lm_posterior <- lapply(
  names(dt_posterior)[-c(1:3)] %>% `[`(!grepl("dpc_", .)),
  function(predictor) {
    dt_inner <- dt_posterior %>%
      mutate(across(-c(fcode, fclass, ftype), ~ normalize(.)))
    
    fl1 <- as.formula(paste("dpc_1 ~", predictor))
    dp1_prop <- lm(fl1, data = dt_inner[fclass == "产权类", ]) %>% summary()
    dp1_oprt <- lm(fl1, data = dt_inner[fclass == "经营权类", ]) %>% summary()
    dp1_pool <- lm(fl1, data = dt_inner) %>% summary()
    
    fl4 <- as.formula(paste("dpc_4 ~", predictor))
    dp4_prop <- lm(fl4, data = dt_inner[fclass == "产权类", ]) %>% summary()
    dp4_oprt <- lm(fl4, data = dt_inner[fclass == "经营权类", ]) %>% summary()
    dp4_pool <- lm(fl4, data = dt_inner) %>% summary()
    
    lapply(list(
      dp1_prop, dp1_oprt, dp1_pool, dp4_prop, dp4_oprt, dp4_pool
    ), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>%
      rbindlist() %>%
      mutate(
        dpc = rep(c("t1", "t4"), each = 3),
        fclass = rep(c("产权类", "经营权类", "全样本"), 2)
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(pvalue != " ") %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  # mutate(predictor = paste0("(", round(R2, 3), ") ", predictor)) %>%
  arrange(-R2) %>%
  mutate(
    predictor = factor(predictor, levels = unique(predictor)),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类")),
    dpc = factor(dpc, levels = c("t1", "t4"), labels = c("上市首日", "上市第4日"))
  ) %>%
  filter(!(fclass == "产权类" & R2 < 0.2))

### 单因子绘图, t = 1, 4
ggplot(dt_lm_posterior,
       mapping = aes(x = predictor, y = coef, fill = dpc)) +
  facet_wrap(fclass ~ dpc, ncol = 2, scales = "free") +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  scale_fill_manual(
    name = "", values = c("上市首日" = "#5b0d07", "上市第4日" = "#b68e55")
  ) +
  guides(fill = "none") +
  labs(
    #x = expression("(标准化后) 因子, 单变量" ~ R^2),
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14),
    plot.margin = margin(10, 5, 5, 5)
  ) +
  geom_text(
    aes(label = round(coef, 3),
        vjust = if_else(coef > 0, -.2, 1.2)),
    size = 3,
    position = position_dodge(width = 0.9)
  ) -> fig_lm_pc

### 单因子回归 (仅线性), t = 20, 60
dt_lm_posterior_20 <- lapply(
  names(dt_posterior)[-c(1:3)] %>% `[`(!grepl("dpc_", .)),
  function(predictor) {
    dt_inner <- dt_posterior %>%
      mutate(across(-c(fcode, fclass, ftype), ~ normalize(.)))
    
    fl1 <- as.formula(paste("dpc_20 ~", predictor))
    dp1_prop <- lm(fl1, data = dt_inner[fclass == "产权类", ]) %>% summary()
    dp1_oprt <- lm(fl1, data = dt_inner[fclass == "经营权类", ]) %>% summary()
    dp1_pool <- lm(fl1, data = dt_inner) %>% summary()
    
    fl4 <- as.formula(paste("dpc_60 ~", predictor))
    dp4_prop <- lm(fl4, data = dt_inner[fclass == "产权类", ]) %>% summary()
    dp4_oprt <- lm(fl4, data = dt_inner[fclass == "经营权类", ]) %>% summary()
    dp4_pool <- lm(fl4, data = dt_inner) %>% summary()
    
    lapply(list(
      dp1_prop, dp1_oprt, dp1_pool, dp4_prop, dp4_oprt, dp4_pool
    ), function(lmdt) {
      data.table(
        predictor = predictor,
        coef = lmdt$coefficients[predictor, "Estimate"],
        pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
        R2 = lmdt$r.squared
      )
    }) %>%
      rbindlist() %>%
      mutate(
        dpc = rep(c("t1", "t4"), each = 3),
        fclass = rep(c("产权类", "经营权类", "全样本"), 2)
      ) %>% return()
  }
) %>% rbindlist() %>%
  filter(pvalue != " ") %>%
  mutate(predictor = vec_predictors_chn[predictor]) %>%
  mutate(predictor = paste0(predictor, "\n", round(R2, 3), "")) %>%
  # mutate(predictor = paste0("(", round(R2, 3), ") ", predictor)) %>%
  arrange(-R2) %>%
  mutate(
    predictor = factor(predictor, levels = unique(predictor)),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类")),
    dpc = factor(dpc, levels = c("t1", "t4"), labels = c("上市第20日", "上市第60日"))
  ) %>%
  filter(!(fclass == "产权类" & R2 < 0.2))

### 单因子绘图, t = 20, 60
ggplot(dt_lm_posterior_20,
       mapping = aes(x = predictor, y = coef, fill = dpc)) +
  facet_wrap(fclass ~ dpc, ncol = 2, scales = "free") +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  scale_fill_manual(
    name = "", values = c("上市第20日" = "#5b0d07", "上市第60日" = "#b68e55")
  ) +
  guides(fill = "none") +
  labs(
    #x = expression("(标准化后) 因子, 单变量" ~ R^2),
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14),
    plot.margin = margin(10, 5, 5, 5)
  ) +
  geom_text(
    aes(label = round(coef, 3),
        vjust = if_else(coef > 0, -.2, 1.2)),
    size = 3,
    position = position_dodge(width = 0.9)
  ) -> fig_lm_pc_20

## 5.2. posterior核心因子临界点 ------------------------------------------------
## 核心因子: ofl_multi, p_list_quantile, index_60d
##           ofl_num, ofrate, secind_60d
dt_posterior %>%
  select(
    fcode, fclass, dpc_1, dpc_4, ofrate, p_list_quantile, secind_60d, famt_act #,secind_60d
  ) %>%
  pivot_longer(
    cols = c(dpc_1, dpc_4),
    names_to = "dpc",
    values_to = "y_dpc"
  ) %>%
  pivot_longer(
    cols = c(ofrate, p_list_quantile, secind_60d, famt_act),
    names_to = "variable",
    values_to = "x_var"
  ) %>%
  filter(!is.na(x_var)) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(
    point_filter = case_when(
      .default = NA,
      fclass != "全样本" & variable == "ofrate" & dpc == "dpc_1" &
        fcode %in% c("180602.SZ", "180203.SZ") ~ "1",
      fclass != "全样本" & variable == "ofrate" & dpc == "dpc_4" &
        fcode %in% c("508086.SH", "508002.SH") ~ "1",

      fclass != "全样本" & variable == "p_list_quantile" & dpc == "dpc_1" &
        fcode %in% c("508022.SH", "508069.SH") ~ "1",
      fclass != "全样本" & variable == "p_list_quantile" & dpc == "dpc_4" &
        fcode %in% c("180603.SZ", "508069.SH") ~ "1",

      fclass != "全样本" & variable == "secind_60d" & dpc == "dpc_1" &
        fcode %in% c("180103.SZ", "508008.SH") ~ "1",
      fclass != "全样本" & variable == "secind_60d" & dpc == "dpc_4" &
        fcode %in% c("180103.SZ", "508086.SH") ~ "1",
      
      fclass != "全样本" & variable == "famt_act" & dpc == "dpc_1" &
        fcode %in% c("508056.SH", "180201.SZ") ~ "1",
      fclass != "全样本" & variable == "famt_act" & dpc == "dpc_4" &
        fcode %in% c("508056.SH", "508028.SH") ~ "1"
    )
  ) %>%
  mutate(corr = round(cor(y_dpc, x_var), 3), .by = c("dpc", "variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = c("dpc", "variable")
  ) %>%
  mutate(
    dpc = factor(
      dpc, levels = c("dpc_1", "dpc_4"),
      labels = c("上市首日", "上市第4日")
    ),
    variable = factor(
      variable,
      levels = c("ofrate", "p_list_quantile", "secind_60d", "famt_act"),
      labels = c("网下确认比例%", "发行价询价区间分位数%",
                 "板块指数T日前60日涨跌幅", "实际募集资金 (亿元)")
    )
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("网下确认比例%",
               "发行价询价区间分位数%",
               "板块指数T日前60日涨跌幅",
               "实际募集资金 (亿元)"),
    value = corr2
  ), .by = "dpc") %>% {
    ggplot(data = ., mapping = aes(
      x = x_var, y = y_dpc,
      group = fclass, color = fclass
    )) +
      facet_wrap(dpc ~ v_tab, scales = "free_x", nrow = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "lm") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.2) +
      geom_point(
        data = filter(., point_filter == "1"),
        alpha = 1, size = 2
      ) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "收盘价格较发行价涨幅%") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pc_posterior

### 核心因子图, t = 20, 60
dt_posterior %>%
  select(
    fcode, fclass, dpc_20, dpc_60, ofrate, p_list_quantile, secind_60d, famt_act #,secind_60d
  ) %>%
  pivot_longer(
    cols = c(dpc_20, dpc_60),
    names_to = "dpc",
    values_to = "y_dpc"
  ) %>%
  pivot_longer(
    cols = c(ofrate, p_list_quantile, secind_60d, famt_act),
    names_to = "variable",
    values_to = "x_var"
  ) %>%
  filter(!is.na(x_var)) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(
    point_filter = case_when(
      .default = NA,
      fclass != "全样本" & variable == "ofrate" & dpc == "dpc_20" &
        fcode %in% c("180302.SZ", "508086.SH") ~ "1",
      fclass != "全样本" & variable == "ofrate" & dpc == "dpc_60" &
        fcode %in% c("508086.SH", "508017.SH") ~ "1",

      fclass != "全样本" & variable == "p_list_quantile" & dpc == "dpc_20" &
        fcode %in% c("180603.SZ", "508069.SH") ~ "1",
      fclass != "全样本" & variable == "p_list_quantile" & dpc == "dpc_60" &
        fcode %in% c("508008.SH", "180601.SZ") ~ "1",
      
      fclass != "全样本" & variable == "secind_60d" & dpc == "dpc_20" &
        fcode %in% c("180103.SZ", "508008.SH") ~ "1",
      fclass != "全样本" & variable == "secind_60d" & dpc == "dpc_60" &
        fcode %in% c("180103.SZ", "508086.SH") ~ "1",
      
      fclass != "全样本" & variable == "famt_act" & dpc == "dpc_20" &
        fcode %in% c("508056.SH", "180201.SZ") ~ "1",
      fclass != "全样本" & variable == "famt_act" & dpc == "dpc_60" &
        fcode %in% c("508056.SH", "508028.SH") ~ "1",
    )
  ) %>%
  filter(!is.na(y_dpc)) %>%
  mutate(corr = round(cor(y_dpc, x_var), 3), .by = c("dpc", "variable", "fclass")) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    ),
    .by = c("dpc", "variable")
  ) %>%
  mutate(
    dpc = factor(
      dpc, levels = c("dpc_20", "dpc_60"),
      labels = c("上市第20日", "上市第60日")
    ),
    variable = factor(
      variable,
      levels = c("ofrate", "p_list_quantile", "secind_60d", "famt_act"),
      labels = c("网下确认比例%", "发行价询价区间分位数%",
                 "板块指数募集开始日前60日涨跌幅", "实际募集资金 (亿元)")
    )
  ) %>%
  mutate(v_tab = factor_with_valuelabel(
    variable,
    levels = c("网下确认比例%", "发行价询价区间分位数%",
               "板块指数募集开始日前60日涨跌幅", "实际募集资金 (亿元)"),
    value = corr2
  ), .by = "dpc") %>% {
    ggplot(data = ., mapping = aes(
      x = x_var, y = y_dpc,
      group = fclass, color = fclass
    )) +
      facet_wrap(dpc ~ v_tab, scales = "free_x", nrow = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "lm") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.2) +
      geom_point(
        data = filter(., point_filter == "1"),
        alpha = 1, size = 2
      ) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = "REITs 特征", y = "收盘价格较发行价涨幅%") +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  } -> fig_pc_posterior_20

## 5.3. 公众posterior ------------------------------------------------
dt_ofline_j %>%
  left_join(
    dt_famt[, .(fcode, famt_act)], by = join_by(fcode)
  ) %>%
  # filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, x = famt_act, y = pbrate) %>%
  bind_rows(
    mutate(., fclass = "全样本")
  ) %>%
  mutate(
    .by = fclass,
    across(c(x, y), ~ as.numeric(.)),
    corr = round(cor(x, y), 3)
  ) %>%
  mutate(
    corr2 = paste0(
      "全样本", "Corr = ", first(corr[fclass == "全样本"]), ", ",
      "\n产权类", "Corr = ", first(corr[fclass == "产权类"]), ", ",
      "经营权类", "Corr = ", first(corr[fclass == "经营权类"])
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = x, y = y,
                                   group = fclass, color = fclass)) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7, method = "lm") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(
        x = "网下有效认购倍数", y = "公众确认比例%",
        title = unique(.$corr2)
      ) +
      guides(fill = "none") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  }

# 6. I/O ------------------------------------------------
## 导出图片

ggsave(filename = paste0("output_v2/", "fig_overall_dscrp", ".pdf"),
       plot = fig_overall_dscrp,
       width = 9, height = 9)
ggsave(filename = paste0("output_v2/", "fig_overall_dscrp_pc", ".pdf"),
       plot = fig_overall_dscrp_pc,
       width = 9, height = 9)
ggsave(filename = paste0("output_v2/", "fig_idist", ".pdf"),
       plot = fig_idist,
       width = 9, height = 9)
ggsave(filename = paste0("output_v2/", "fig_time_d60", ".pdf"),
       plot = fig_time_d60,
       width = 9, height = 9)
ggsave(filename = paste0("output_v2/", "fig_time_d60_vst0", ".pdf"),
       plot = fig_time_d60_vst0,
       width = 9, height = 9)
ggsave(filename = paste0("output_v2/", "fig_industry_v2", ".pdf"),
       plot = fig_industry_v2,
       width = 12, height = 14)
ggsave(filename = paste0("output_v2/", "fig_participation", ".pdf"),
       plot = fig_participation,
       width = 7, height = 4)
ggsave(filename = paste0("output_v2/", "fig_participation_str", ".pdf"),
       plot = fig_participation_str,
       width = 8, height = 8)
ggsave(filename = paste0("output_v2/", "fig_ofl_str", ".pdf"),
       plot = fig_ofl_str,
       width = 14, height = 8)
ggsave(filename = paste0("output_v2/", "fig_pub_str", ".pdf"),
       plot = fig_pub_str,
       width = 14, height = 8)
ggsave(filename = paste0("output_v2/", "fig_ofl_offline", ".pdf"),
       plot = fig_ofl_offline,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_pub_offline", ".pdf"),
       plot = fig_pub_offline,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_ofl_query", ".pdf"),
       plot = fig_ofl_query,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_pub_query", ".pdf"),
       plot = fig_pub_query,
       width = 12, height = 9)
ggsave(filename = paste0("output_v2/", "fig_ofl_value", ".pdf"),
       plot = fig_ofl_value,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_pub_value", ".pdf"),
       plot = fig_pub_value,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_ofl_sector", ".pdf"),
       plot = fig_ofl_sector,
       width = 12, height = 8)
ggsave(filename = paste0("output_v2/", "fig_pub_sector", ".pdf"),
       plot = fig_pub_sector,
       width = 12, height = 8)
ggsave(filename = paste0("output_v2/", "comb_fig_lm_linear_ofl", ".pdf"), # _unnormalized
       plot = comb_fig_lm_linear_ofl,
       width = 12, height = 12)
ggsave(filename = paste0("output_v2/", "comb_fig_lm_linear_pub", ".pdf"), # _unnormalized
       plot = comb_fig_lm_linear_pub,
       width = 12, height = 12)
ggsave(filename = paste0("output_v2/", "comb_fig_lm_quadratic_ofl", ".pdf"),
       plot = comb_fig_lm_quadratic_ofl,
       width = 14, height = 12)
ggsave(filename = paste0("output_v2/", "comb_fig_lm_quadratic_pub", ".pdf"),
       plot = comb_fig_lm_quadratic_pub,
       width = 14, height = 12)
ggsave(filename = paste0("output_v2/", "fig_lm_pc", ".pdf"),
       plot = fig_lm_pc,
       width = 17, height = 12)
ggsave(filename = paste0("output_v2/", "fig_lm_pc_20", ".pdf"),
       plot = fig_lm_pc_20,
       width = 17, height = 12)
ggsave(filename = paste0("output_v2/", "fig_pc_posterior", ".pdf"),
       plot = fig_pc_posterior,
       width = 17, height = 8)
ggsave(filename = paste0("output_v2/", "fig_pc_posterior_20", ".pdf"),
       plot = fig_pc_posterior_20,
       width = 17, height = 8)
ggsave(filename = paste0("output_v2/", "fig_dxr_rollmean", ".pdf"),
       plot = fig_dxr_rollmean,
       width = 14, height = 5)
ggsave(filename = paste0("output_v2/", "fig_dxr_rollmean_v2", ".pdf"),
       plot = fig_dxr_rollmean_v2,
       width = 7, height = 7)
ggsave(filename = paste0("output_v2/", "fig_lm_str", ".pdf"),
       plot = fig_lm_str,
       width = 17, height = 15)
ggsave(filename = paste0("output_v2/", "fig_ofl_amt", ".pdf"),
       plot = fig_ofl_amt,
       width = 10, height = 4)
ggsave(filename = paste0("output_v2/", "fig_pub_amt", ".pdf"),
       plot = fig_pub_amt,
       width = 10, height = 4)
