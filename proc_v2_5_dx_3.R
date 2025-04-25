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

dt_str_share <- as.data.table(dt_str)[type_pen %in% c("保险", "原始权益人或其关联方", "信托",
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
  mutate(across(ends_with("pct"), ~ ifelse(is.na(.), 0, .)))

# 1. factor reg, table ------------------------------------------------
## 1.1. data for reg ------------------------------------------------
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
  ) # %>% mutate(across(-c(fcode, fclass), ~ normalize(.)))

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
  ) # %>% mutate(across(-c(fcode, fclass), ~ normalize(.)))

vec_predictors_chn <- c(
  "dif_divd" = "预测分派率%-板块二级市场均值",
  "dif_term" = "剩余期限(年)-板块均值",
  "dif_irr" = "0增速IRR%-板块二级市场均值",
  "public_pct" = "流通盘占比%",
  "p_list_quantile" = "发行价询价区间分位数%",
  "p_up" = "询价区间上限/拟聚集金额 per share",
  "p_med" = "询价区间中位数/拟募集金额 per share",
  #"p_low" = "询价区间下限/拟聚集金额 per share",
  "ofl_multi" = "网下有效认购倍数",
  "ofl_num" = "网下有效认购账户数",
  "secind_20d" = "板块指数询价日前20日涨跌幅",
  "secind_60d" = "板块指数询价日前60日涨跌幅",
  "index_20d" = "全市场指数询价日前20日涨跌幅",
  "index_60d" = "全市场指数询价日前60日涨跌幅",
  "index_60d_fst" = "全市场指数询价日前60日(取短)涨跌幅",
  "index_60dm" = "全市场指数询价日前60日涨跌幅均值",
  "secind_60d_fst" = "板块指数询价日前60日(取短)涨跌幅",
  "secind_60dm" = "板块指数询价日前60日涨跌幅均值",
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

## 1.2. regression ------------------------------------------------
### 网下
vec_predictors_chn %<>%
  str_replace_all("T日", "询价日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_result <- lapply(
  names(dt_lm_ofl)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste0("dxr~", predictor))
    sapply(c("产权类", "经营权类", "全样本"), function(fclass_i) {
        reg_org <- lm(formula, data = dt_lm_ofl[fclass != fclass_i, ]) %>% summary()

        dt_lm_uni <- mutate(dt_lm_ofl, across(-c(fcode, fclass), ~ normalize(.)))
        reg_uni <- lm(formula, data = dt_lm_uni[fclass != fclass_i, ]) %>% summary()

        corr <- dt_lm_ofl[fclass != fclass_i, ] %>% select(dxr, all_of(predictor)) %>%
          na.omit() %>% {cor(.[[1]], .[[2]])}

        lapply(list(reg_org, reg_uni), function(lmdt) {
          data.table(
            predictor = predictor,
            coef = lmdt$coefficients[predictor, "Estimate"],
            pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
            R2 = lmdt$r.squared,
            corr = corr
            ) %>% return()
        }) %>% rbindlist() %>%
      mutate(
        coef_type = c("原始回归系数", "标准化回归系数"),
        fclass = rep(c("产权类" = "经营权类", "经营权类" = "产权类", "全样本" = "全样本")[fclass_i], 2)
      ) %>% return()
    }, simplify = FALSE) %>% rbindlist()
  }
) %>% rbindlist() %>%
  mutate(
    predictor = vec_predictors_chn[predictor],
    dxr_type = "网下投资者"
  ) %>%
  arrange(-R2) %>%
  filter(pvalue != " " & R2 >= 0.1)

### 公众
vec_predictors_chn %<>%
  str_replace_all("询价日", "T日") %>%
  `names<-`(names(vec_predictors_chn))

dt_lm_result <- lapply(
  names(dt_lm_pub)[-c(1:3)],
  function(predictor) {
    formula <- as.formula(paste0("dxr~", predictor))
    sapply(c("产权类", "经营权类", "全样本"), function(fclass_i) {
        reg_org <- lm(formula, data = dt_lm_pub[fclass != fclass_i, ]) %>% summary()

        dt_lm_uni <- mutate(dt_lm_pub, across(-c(fcode, fclass), ~ normalize(.)))
        reg_uni <- lm(formula, data = dt_lm_uni[fclass != fclass_i, ]) %>% summary()

        corr <- dt_lm_pub[fclass != fclass_i, ] %>% select(dxr, all_of(predictor)) %>%
          na.omit() %>% {cor(.[[1]], .[[2]])}

        lapply(list(reg_org, reg_uni), function(lmdt) {
          data.table(
            predictor = predictor,
            coef = lmdt$coefficients[predictor, "Estimate"],
            pvalue = lmdt$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
            R2 = lmdt$r.squared,
            corr = corr
          ) %>% return()
        }) %>% rbindlist() %>%
      mutate(
        coef_type = c("原始回归系数", "标准化回归系数"),
        fclass = rep(c("产权类" = "经营权类", "经营权类" = "产权类", "全样本" = "全样本")[fclass_i], 2)
      ) %>% return()
    }, simplify = FALSE) %>% rbindlist()
  }
) %>% rbindlist() %>%
  mutate(
    predictor = vec_predictors_chn[predictor],
    dxr_type = "公众投资者"
  ) %>%
  arrange(-R2) %>%
  filter(pvalue != " " & R2 >= 0.1) %>%
  rbind(dt_lm_result) %>%
  mutate(across(c(coef, R2, corr), ~ round(., 3)))

dt_lm_result %>%
  pivot_wider(
    names_from = "coef_type", values_from = "coef",
    id_cols = c(dxr_type, fclass, predictor, corr, R2)
  ) %>%
  select(dxr_type, fclass, predictor, `标准化回归系数`, `原始回归系数`, corr, R2) %>%
  mutate(
    dxr_type = factor(dxr_type, levels = c("网下投资者", "公众投资者")),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类"))
  ) %>%
  arrange(dxr_type, fclass, -R2) %>%
  rename(`投资者类型` = dxr_type, `REITs分类` = fclass, `因子` = predictor, `相关性系数` = corr) %>%
  write.xlsx("reg_table.xlsx")

# 2. offline account factor ------------------------------------------------
dt_lm_str_pc <- dt_ofline_i %>%
  left_join(dt_ofline_j[, .(fcode, fclass, plist)], by = join_by(fcode)) %>%
  mutate(N_i = unicnt(icode), V_i = sum(qty * plist)/10000, .by = c("fcode", "itype")) %>%
  select(fcode, fname, fclass, itype, plist, N_i, V_i) %>% unique() %>%
  mutate(
    .by = "fcode",
    N_i_pct = N_i / sum(N_i),
    V_i_pct = V_i / sum(V_i),
  ) %>%
  filter(itype %in% c("保险机构", "券商自营", "公募基金专户", "券商资管计划", "集合信托计划", "私募基金")) %>%
  mutate(
    dpc_1 = vlookup(fcode, dt_dxr_D60[tt == 0, .(fcode, pc)]),
    dpc_4 = vlookup(fcode, dt_dxr_D60[tt == 3, .(fcode, pc)]),
    dpc_20 = vlookup(fcode, dt_dxr_D60[tt == 19, .(fcode, pc)]),
    dpc_60 = vlookup(fcode, dt_dxr_D60[tt == 59, .(fcode, pc)]),
    across(starts_with("dpc_"), ~ (. / plist - 1) * 100)
  ) %>%
  select(-plist) %>%
  mutate(across(-c(fcode, fname, itype, fclass), ~ normalize(.)))

vec_predictors_str <- c(
  "N_i" = "账户数量",  "V_i" = "认购金额",
  "N_i_pct" = "账户数量占比", "V_i_pct" = "认购金额占比"
)

lapply(
  c("N_i", "N_i_pct", "V_i_pct"), # "V_i",
  function(predictor) {
    lapply(c(1, 4, 20, 60), function(tt) {
      formula <- as.formula(paste0("dpc_", tt, "~", predictor))
      lapply(c("全样本"), function(fclass_i) {
        lapply(unique(dt_lm_str_pc$itype), function(itype_i) {
          reg_uni <- lm(formula, data = dt_lm_str_pc[fclass != fclass_i & itype == itype_i, ]) %>% summary()
          corr <- dt_lm_str_pc[fclass != fclass_i & itype == itype_i, ] %>%
            select(all_of(c(paste0("dpc_", tt), predictor))) %>%
            na.omit() %>% {cor(.[[1]], .[[2]])}
          data.table(
            dpc = paste0("dpc_", tt),
            itype = itype_i,
            predictor = predictor,
            fclass = c("产权类" = "经营权类", "经营权类" = "产权类", "全样本" = "全样本")[fclass_i],
            coef = reg_uni$coefficients[predictor, "Estimate"],
            pvalue = reg_uni$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
            R2 = reg_uni$r.squared,
            corr = corr
          ) %>% return()
        }) %>% rbindlist() %>% return()
      }) %>% rbindlist() %>% return()
    }) %>% rbindlist() %>% return()
  }) %>% rbindlist() %>%
  filter(pvalue != " " & R2 >= 0.1) %>%
  mutate(
    dpc = factor(dpc, levels = c("dpc_1", "dpc_4", "dpc_20", "dpc_60")),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类"))
  ) %>%
  arrange(dpc, fclass, -R2) %>%
  mutate(
    dpc = paste0("上市后第", str_extract(dpc, "(?<=_)[0-9]+"), "日涨跌幅"),
    predictor = vec_predictors_str[predictor],
    across(c(coef, R2, corr), ~ round(., 3))
  ) %>%
  select(dpc, fclass, itype, predictor, coef, corr, R2) %>%
  rename(
    `被解释变量` = dpc, `REITs分类` = fclass, `因子` = predictor, `相关性系数` = corr, `标准化回归系数` = coef,
    `网下投资者类型` = itype
  ) %>% write.xlsx("reg_str_table.xlsx")

## check
dt_check <- dt_lm_str_pc %>%
  left_join(dt_ofline_j[, .(fcode, ofrate, dxr_ofl)], by = "fcode") %>%
  select(fcode, fname, itype, N_i, N_i_pct, V_i_pct, ofrate, dpc_1, dxr_ofl)

dt_check %>%
  pivot_longer(cols = c(N_i, N_i_pct, V_i_pct), names_to = "x_lab", values_to = "x_val") %>%
  filter(itype == "公募基金专户") %>% {
    ggplot(data = ., mapping = aes(x = x_val, y = dpc_1)) +
      facet_wrap(~ x_lab, nrow = 1) +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      geom_smooth(se = F, linewidth = 0.7, method = "lm") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  }

## table of reg results, dxr_ofl
dt_lm_str_dxr <- dt_ofline_i %>%
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

lapply(unique(dt_lm_str_dxr$itype), function(itype_i) {
  lapply(c("N_i", "N_i_pct", "V_i", "V_i_pct"), function(predictor) {
    lapply(c("ofl", "pub"), function(dxr_type) {
      formula <- as.formula(paste0("dxr_", dxr_type, "~", predictor))
      lapply(c("产权类", "经营权类", "全样本"), function(fclass_i) {
        reg_uni <- lm(formula, data = dt_lm_str_dxr[fclass != fclass_i & itype == itype_i, ]) %>% summary()
        corr <- dt_lm_str_dxr[fclass != fclass_i & itype == itype_i, ] %>%
          select(all_of(c(paste0("dxr_", dxr_type), predictor))) %>%
          na.omit() %>% {cor(.[[1]], .[[2]])}
        data.table(
          dxr = paste0("dxr_", dxr_type),
          itype = itype_i,
          predictor = predictor,
          fclass = c("产权类" = "经营权类", "经营权类" = "产权类", "全样本" = "全样本")[fclass_i],
          coef = reg_uni$coefficients[predictor, "Estimate"],
          pvalue = reg_uni$coefficients[predictor, "Pr(>|t|)"] %>% get_stars,
          R2 = reg_uni$r.squared,
          corr = corr
        ) %>% return()
      }) %>% rbindlist() %>% return()
    }) %>% rbindlist() %>% return()
  }) %>% rbindlist() %>% return()
}) %>% rbindlist() %>%
  filter(pvalue != " " & R2 >= 0.1) %>%
  mutate(
    dxr = factor(dxr, levels = c("dxr_pub", "dxr_ofl")),
    fclass = factor(fclass, levels = c("全样本", "产权类", "经营权类"))
  ) %>%
  arrange(dxr, fclass, -R2) %>%
  mutate(
    dxr = case_match(dxr, "dxr_ofl" ~ "网下投资者", "dxr_pub" ~ "公众投资者"),
    predictor = vec_predictors_str[predictor],
    across(c(coef, R2, corr), ~ round(., 3))
  ) %>%
  select(dxr, fclass, itype, predictor, coef, corr, R2) %>%
  rename(
    `打新收益率` = dxr, `REITs分类` = fclass, `因子` = predictor, `相关性系数` = corr, `标准化回归系数` = coef,
    `网下投资者类型` = itype
  ) %>%
  write.xlsx("reg_str_dxr_tabel.xlsx")

# 3. breakeven ------------------------------------------------
dt_be <- dt_ofline_j %>%
  left_join(dt_dxr_D60[tt == 0, .(fcode, pc)], by = "fcode") %>%
  left_join(dt_dxr_D60[tt == 3, .(fcode, pc_4 = pc)], by = "fcode") %>%
  apply(MARGIN = 1, function(fc.ofl) {
  ofrate <- as.numeric(fc.ofl["ofrate"])
  pbrate <- as.numeric(fc.ofl["pbrate"])
  ### 计息日期
  t0_ofl <- fc.ofl["Ldate"]; t0_pub <- fc.ofl["Tdate"] # 缴款日
  t_ref <- date_add(fc.ofl["Ldate"], 2)                # 退款日
  t1_norst <- date_add(fc.ofl["Listdate"], -1)         # 无限售卖出日
  t1_rst <- date_add(fc.ofl["Listdate"], 3 - 1)        # 有限售剩余卖出日
  ### 实际溢价
  RP1 <- as.numeric(fc.ofl["pc"]) / as.numeric(fc.ofl["plist"])
  RP4 <- as.numeric(fc.ofl["pc_4"]) / as.numeric(fc.ofl["plist"])
  ### 网下无限售breakeven
  OC_ofl_norst <- prod(r_gc01(t0_ofl, t1_norst, dt_fcost))
  R0_ofl_norst <- prod(r_gc01(t_ref, t1_norst, dt_fcost))
  be_ofl_norst <- (OC_ofl_norst - (1 - ofrate) * R0_ofl_norst) / ofrate
  ### 网下有限售breakeven (第4日)
  OC_ofl_rst <- prod(r_gc01(t0_ofl, t1_rst, dt_fcost))
  R0_ofl_rst <- prod(r_gc01(t_ref, t1_rst, dt_fcost))
  R1_ofl_rst_1 <- ofrate * 0.2 * RP1 * prod(r_gc01(fc.ofl["Listdate"], t1_rst, dt_fcost))
  be_ofl_rst <- (OC_ofl_rst - (1 - ofrate) * R0_ofl_rst - R1_ofl_rst_1) / (ofrate * 0.8)
  ### 公众breakeven
  OC_pub <- prod(r_gc01(t0_pub, t1_norst, dt_fcost))
  R0_pub <- prod(r_gc01(t_ref,  t1_norst, dt_fcost))
  be_pub <- (OC_pub - (1 - pbrate) * R0_pub) / pbrate
  ### 回收结果
  data.table(
    "fcode" = fc.ofl["fcode"],
    "fname" = fc.ofl["fname"],
    "restrict" = ifelse(fc.ofl["restrict"] == "FALSE", FALSE, TRUE),
    "be_ofl_norst" = 100 * (be_ofl_norst - 1),
    "be_ofl_rst" = 100 * (be_ofl_rst - 1),
    "be_ofl" = ifelse(fc.ofl["restrict"] == "FALSE",
                      100 * (be_ofl_norst - 1),
                      100 * (be_ofl_rst - 1)),
    "be_pub" = 100 * (be_pub - 1),
    "dxr_ofl" = as.numeric(fc.ofl["dxr_ofl"]),
    "dxr_pub" = as.numeric(fc.ofl["dxr_pub"]),
    "dp1" = (RP1 - 1) * 100,
    "dp4" = (RP4 - 1) * 100
  ) %>%
    return()
}, simplify = FALSE) %>%
  rbindlist()

dt_be %>%
  mutate(
    check_ofl = (dxr_ofl > 0) == ifelse(restrict, dp4 >= be_ofl, dp1 >= be_ofl),
    check_pub = (dxr_pub > 0) == (dp1 >= be_pub)
  )

dt_be %>%
  mutate(
    restrict = ifelse(restrict, "有限售", "无限售"),
    across(c(be_ofl_norst:dp4), ~ . / 100),
  ) %>%
  left_join(
    dt_ofline_j[, .(fcode, Listdate, ofrate, pbrate)], by = "fcode"
  ) %>%
  select(
    `证券代码` = fcode, `证券简称` = fname, `限售条款` = restrict, `上市日期` = Listdate,
    `网下打新保本涨幅` = be_ofl, `公众打新保本涨幅` = be_pub,
    `网下确认比例` = ofrate, `公众确认比例` = pbrate,
    `上市首日价格涨幅` = dp1, `上市第4日价格涨幅` = dp4,
    `网下打新收益率` = dxr_ofl, `公众打新收益率` = dxr_pub
  ) %>%
  write.xlsx(file = "breakeven.xlsx")

# 4. fig (ver. 2), comb-fig ------------------------------------------------
## 部分图修改成组合图
## 4.1. 板块指标组合图 ------------------------------------------------
fig_sector_ofl <- dt_ofline_j %>%
  select(fcode, fclass, ftype, dxr = dxr_ofl, Xdate) %>%
  mutate(
    ftype = recode(ftype,
                   "商业" = "消费", "物流园" = "仓储物流",
                   "保障性租赁住房" = "保租房"),
  ) %>%
  left_join(
    select(dt_sector, date, ftype, secind_60d, index_60d),
    by = join_by(Xdate == date, ftype == ftype)
  ) %>%
  select(-ends_with("date")) %>%
  pivot_longer(cols = -c(fcode, fclass, ftype, dxr),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "secind_60d" = "板块指数询价日前60日涨跌幅",
                      "index_60d" = "全市场指数询价日前60日涨跌幅")
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
  mutate(
    variable = factor_with_valuelabel(
      variable,
      levels = c("板块指数询价日前60日涨跌幅", "全市场指数询价日前60日涨跌幅"),
      value = corr2
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr, group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = NULL, y = "(网下投资者) 打新收益率 %") +
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

fig_sector_pub <- dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, ftype, dxr = dxr_pub, Tdate) %>%
  mutate(
    ftype = recode(ftype,
                   "商业" = "消费", "物流园" = "仓储物流",
                   "保障性租赁住房" = "保租房"),
  ) %>%
  left_join(
    select(dt_sector, date, ftype, secind_60d, index_60d),
    by = join_by(Tdate == date, ftype == ftype)
  ) %>%
  select(-ends_with("date")) %>%
  pivot_longer(cols = -c(fcode, fclass, ftype, dxr),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "secind_60d" = "板块指数T日前60日涨跌幅",
                      "index_60d" = "全市场指数T日前60日涨跌幅")
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
  mutate(
    variable = factor_with_valuelabel(
      variable,
      levels = c("板块指数T日前60日涨跌幅", "全市场指数T日前60日涨跌幅"),
      value = corr2
    )
  ) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr, group = fclass, color = fclass)) +
      facet_wrap(~ variable, scales = "free_x", ncol = 2) +
      geom_point(alpha = 0.7, size = 0.7, shape = 1) +
      geom_smooth(se = F, linewidth = 0.7, method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      scale_color_manual(name = NULL,
                         values = c("经营权类" = "#b68e55",
                                    "产权类" = "#5b0d07",
                                    "全样本" = "skyblue")) +
      labs(x = NULL, y = "(公众投资者) 打新收益率 %") +
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

comb_fig_sector <- plot_grid(
  fig_sector_ofl + theme(legend.position = "none"),
  fig_sector_pub + theme(legend.position = "none"),
  ncol = 1, align = "v", axis = "t"
) %>%
  plot_grid(
    get_legend(fig_sector_pub),
    nrow = 1,
    align = "h", axis = "tb", rel_widths = c(1, 0.15)
  )

## 4.2. 流通盘组合图 ------------------------------------------------
fig_pubamt_ofl <- dt_ofline_j %>%
  select(fcode, fclass, dxr = dxr_ofl, famt, public_amt) %>%
  mutate(public_pct = public_amt/famt * 100) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value)) & variable %in% c("public_pct")) %>%
  mutate(
    variable = recode(variable, "public_pct" = "流通盘金额占比%")
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
    levels = c("流通盘金额占比%"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7,
                  method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      labs(x = NULL, y = "(网下投资者) 打新收益率 %") +
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
  }

fig_pubamt_pub <- dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fclass, dxr = dxr_pub, famt, public_amt) %>%
  mutate(public_pct = public_amt/famt * 100) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value)) & variable %in% c("public_pct")) %>%
  mutate(
    variable = recode(variable, "public_pct" = "流通盘金额占比%")
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
    levels = c("流通盘金额占比%"),
    value = corr2
  )) %>% {
    ggplot(data = ., mapping = aes(x = value, y = dxr,
                                   group = fclass, color = fclass)) +
      facet_wrap(~ v_tab, scales = "free_x") +
      geom_point(alpha = 0.7, size = 0.7, shape = 5) +
      geom_smooth(se = F, linewidth = 0.7,
                  method = "loess") +
      geom_line(linetype = "dashed", linewidth = .7, alpha = 0.1) +
      labs(x = NULL, y = "(公众投资者) 打新收益率 %") +
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
  }

comb_fig_pubamt <- plot_grid(
  fig_pubamt_ofl + theme(legend.position = "none"),
  fig_pubamt_pub + theme(legend.position = "none"),
  nrow = 1, align = "h", axis = "t"
) %>%
  plot_grid(
    get_legend(fig_pubamt_pub),
    nrow = 1,
    align = "h", axis = "tb", rel_widths = c(1, 0.15)
  )

## 4.3. 估值指标组合图 ------------------------------------------------
fig_value_ofl <- dt_ofline_j %>%
  select(fcode, fname, fclass, dxr = dxr_ofl,
         dividend, avg_smdivd, term, avg_term,
         irr, avg_irr_sm,
         p_up, p_low, famt_persh) %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_irr  = irr - avg_irr_sm) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "dif_divd" = "预测分派率% - 板块二级市场均值",
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
      labs(x = NULL, y = "(网下投资者) 打新收益率 %") +
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

fig_value_pub <- dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fname, fclass, dxr = dxr_pub, term, avg_term) %>%
  left_join(
    select(dt_public_j, fcode, dividend, avg_smdivd, irr, avg_irr_sm),
    by = "fcode"
  ) %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_irr  = irr - avg_irr_sm) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable,
                      "dif_divd" = "预测分派率% - 板块二级市场均值",
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
      labs(x = NULL, y = "(公众投资者) 打新收益率 %") +
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

comb_fig_value <- plot_grid(
  fig_value_ofl + theme(legend.position = "none"),
  fig_value_pub + theme(legend.position = "none"),
  nrow = 2, align = "v", axis = "t"
) %>%
  plot_grid(
    get_legend(fig_value_ofl),
    nrow = 1,
    align = "h", axis = "tb", rel_widths = c(1, 0.15)
  )

## 4.4. 期限指标组合图 ------------------------------------------------
fig_term_ofl <- dt_ofline_j %>%
  select(fcode, fname, fclass, dxr = dxr_ofl,
         dividend, avg_smdivd, term, avg_term,
         irr, avg_irr_sm,
         p_up, p_low, famt_persh) %>%
  mutate(dif_term = term - avg_term) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable, "dif_term" = "剩余期限(年) - 板块均值")
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
    levels = c("剩余期限(年) - 板块均值"),
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
      labs(x = NULL, y = "(网下投资者) 打新收益率 %") +
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

fig_term_pub <- dt_ofline_j %>%
  filter(fname != "华安百联消费REIT") %>%
  select(fcode, fname, fclass, dxr = dxr_pub, term, avg_term) %>%
  left_join(
    select(dt_public_j, fcode, dividend, avg_smdivd, irr, avg_irr_sm),
    by = "fcode"
  ) %>%
  mutate(dif_term = term - avg_term) %>%
  select(fcode, fclass, dxr, starts_with("dif_")) %>%
  pivot_longer(cols = -c("fcode", "fclass", "dxr"),
               names_to = c("variable"),
               values_to = c("value")) %>%
  filter(!(is.na(dxr) | is.na(value))) %>%
  mutate(
    variable = recode(variable, "dif_term" = "剩余期限(年) - 板块均值")
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
    levels = c("剩余期限(年) - 板块均值"),
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
      labs(x =  NULL, y = "(公众投资者) 打新收益率 %") +
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

comb_fig_term <- plot_grid(
  fig_term_ofl + theme(legend.position = "none"),
  fig_term_pub + theme(legend.position = "none"),
  nrow = 1, align = "h", axis = "t"
) %>%
  plot_grid(
    get_legend(fig_term_ofl),
    nrow = 1,
    align = "h", axis = "tb", rel_widths = c(1, 0.15)
  )

# 5. I/O ------------------------------------------------
ggsave(filename = paste0("output_v2/", "comb_fig_sector", ".pdf"),
       plot = comb_fig_sector,
       width = 12, height = 9)
ggsave(filename = paste0("output_v2/", "comb_fig_pubamt", ".pdf"),
       plot = comb_fig_pubamt,
       width = 12, height = 5)
ggsave(filename = paste0("output_v2/", "comb_fig_value", ".pdf"),
       plot = comb_fig_value,
       width = 12, height = 9)
ggsave(filename = paste0("output_v2/", "comb_fig_term", ".pdf"),
       plot = comb_fig_term,
       width = 12, height = 5)