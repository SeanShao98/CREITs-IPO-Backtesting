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
setDTthreads(10)

## personal head file
source("ini.R")

## required package:
library(data.table); library(magrittr)
library(see)
library(showtext)

## 中文字体文件
font_add(family = "Hei", regular = "STHeiti Light.ttc")
showtext_auto()

# 1. read excels ------------------------------------------------
## 本节录入网下信息并字符串处理

vec_f_raw <- list.files("release/refined")
ls_dt_raw <- lapply(vec_f_raw, function(ff) {
  readxl::read_excel(path = paste0("release/refined/", ff)) %>% return()
})

vec_varnames <- c(
  "证券代码", "证券简称", "编码/账户",
  "配/售+名/称", "配/售 + 类/型", "元", "份/量", "备注"
)

lapply(ls_dt_raw, function(dt) {
  select_name(dt, c("证券代码",
                    "证券简称|证券名称",
                    "(?s)账户|编码|(^.*配售.*代码$)",
                    "(?s)(配|售).*(名|称)",
                    "(?s)(配|售).*(类|型)",
                    "元",
                    "量",
                    "备注")) %>%
    `names<-`(c("fcode", "fname", "icode", "iname", "itype",
                "cny", "qty", "note")) %>%
    mutate(across(everything(), ~ str_replace_all(., "[\\s\\n\\r]", ""))) %>%
    mutate(across(everything(), ~ str_replace_all(., "一|——", "-"))) %>%
    mutate(across(everything(), ~ str_replace_all(., "\"", "")))
}) -> ls_dt_refined

lapply(ls_dt_refined, function(dt) {
  dt %<>%
    mutate(across(c("cny"), ~ str_replace_all(., ",", "."))) %>%
    mutate(across(c("qty"), ~ str_replace_all(., ",", ""))) %>%
    mutate(across(c("cny", "qty"), ~ as.numeric(.))) %>%
    mutate(across(c("fcode", "fname"), ~ .[1]))
}) -> ls_dt_refined

dt_refined <- do.call(rbind, ls_dt_refined) %>%
  as.data.table() %>%
  `[`(!is.na(icode))

dt_refined[is.na(itype), itype := fuzzy_vlookup(
  col_fuz = .SD$iname,
  col_dic = dt_refined[!is.na(itype), iname],
  value   = dt_refined[!is.na(itype), itype],
  return_val = TRUE
)]

dt_typedic <- dt_refined[fcode != "508086.SH", .(iname, itype)] %>% unique()
dt_refined[fcode == "508086.SH",
           itype := fuzzy_vlookup(col_fuz = iname,
                                  col_dic = dt_typedic$iname,
                                  value = dt_typedic$itype,
                                  return_val = T)]
rm(dt_typedic)
## dt_refined %>% filter(rowSums(is.na(.)) > 0) %>% View

# 2. combine ------------------------------------------------
## 本节将iFind获得的部分信息与网下数据汇总

## restrict
dt_restrict <- readxl::read_xlsx("限售.xlsx") %>% as.data.table()
dt_restrict[, name := fuzzy_vlookup(`名称`, unique(dt_refined$fname))]
dt_restrict[name == "博时蛇口产园REIT", name := "津开产园"]
dt_restrict[, code := vlookup(name, unique(dt_refined[, .(fname, fcode)]))]
## offline rate
dt_rate <- readxl::read_xlsx("比例公告目录.xlsx") %>% as.data.table()
dt_rate[, fname := vlookup(`证券代码`, unique(dt_refined[, .(fcode, fname)]))]
## combine
dt_refined[, ofrate    := vlookup(fcode, dt_rate[, .(`证券代码`, `网下比例`)])]
dt_refined[, pbrate    := vlookup(fcode, dt_rate[, .(`证券代码`, `公众比例`)])]
dt_refined[, fname     := vlookup(fcode, dt_rate[, .(`证券代码`, `证券简称`)])]
dt_refined[, restrict  := fcode %in% dt_restrict$code]

# 3. REITs info ------------------------------------------------
## 本节将iFind获得的部分信息与网下数据汇总，获得核心data frame：
## dt_ofline (母数据),
## dt_ofline_i (仅保留网下投资者信息), dt_ofline_j (基金层面数据)

dt_info <- read_xlsx("REITs基金信息-v2.xlsx", sheet = "Sheet1") %>%
  as.data.table()
dt_return <- read_xlsx("REITs基金收益率-v2.xlsx", sheet = "Sheet1") %>%
  as.data.table() %>%
  `[`(, c("plist", "po_1", "pc_1", "pc_4", "pc_5", "pc_10", "pc_20", "pc_today") :=
        .(`发行价`, `首日开盘价`, `首日收盘价`, `第4日收盘价`,
          `第5日收盘价`, `第10日收盘价`, `第20日收盘价`, `最新收盘价（后复权）`))

dt_value <- read_xlsx("REITs估值-v2.xlsx", sheet = 2) %>%
  as.data.table() %>%
  `[`(, c("PNAV_1", "PNAV_4", "PNAV_5", "PNAV_10", "PNAV_20") :=
        .(`首日收盘价`/NAV, `第4日收盘价`/NAV, `第5日收盘价`/NAV,
          `第10日收盘价`/NAV, `第20日收盘价`/NAV)) %>%
  `[`(, c("vol_1", "vol_4", "vol_5", "vol_10", "vol_20") :=
        .(`成交量/万0`, `成交量/万4`, `成交量/万5`,
          `成交量/万10`, `成交量/万20`)) %>%
  `[`(, c("turnover_1", "turnover_4", "turnover_5", "turnover_10", "turnover_20") :=
        .(`换手率0`, `换手率4`, `换手率5`, `换手率10`, `换手率20`)) %>%
  `[`(, c("Listdate", "NAV") := .(`上市日期`, NAV))

dt_ofline <- dt_refined[
  dt_value[, .(`证券代码`, Listdate
               ## PNAV_1, PNAV_4, PNAV_5, PNAV_10, PNAV_20,
               ## PV_1 = PV0, PV_4 = PV4, PV_5 = PV5, PV_10 = PV10, PV_20 = PV20,
               ,vol_1, vol_4, vol_5, vol_10, vol_20
               ## turnover_1, turnover_4, turnover_5, turnover_10, turnover_20
  )],
  on = .(fcode = `证券代码`)
]

dt_ofline <- dt_ofline[
  dt_return[, .(`证券代码`, plist, pc_1, pc_4, pc_5, pc_10, pc_20, pc_today)],
  on = .(fcode = `证券代码`)
]

# remove newly listed fund
dt_ofline %<>%
  mutate(across(ends_with("4"), ~ ifelse(vol_4 == 0, NA, .))) %>%
  mutate(across(ends_with("5"), ~ ifelse(vol_5 == 0, NA, .))) %>%
  mutate(across(ends_with("10"), ~ ifelse(vol_10 == 0, NA, .))) %>%
  mutate(across(ends_with("20"), ~ ifelse(vol_20 == 0, NA, .))) %>%
  select(!starts_with("vol")) %>% setDT()

# full data
dt_ofline <- dt_ofline[
  dt_info[, .(fcode = `证券代码`,
              ftype = `资产类型`, Tdate = `网下发售起始日`, Ldate = `网下发售截止日`,
              p_up = `询价区间上限`, p_low = `询价区间下限`,
              str_place = `战略配售份额`, str_buy = `战略投资方认购份额`,
              ofl_place = `网下配售份额`, ofl_buy = `网下认购份额`,
              ofl_multi = `网下认购倍数`,
              pub_place = `公众配售份额`, pub_buy = `公众认购份额`)],
  on = .(fcode)
] %>%
  `[`(, dividend := vlookup(fcode, dt_value[, .(`证券代码`, `预测分派率`)])) %>%
  `[`(dividend == 0, dividend := NA) %>%
  mutate(across(p_up:dividend, ~ as.numeric(.))) %>%
  mutate(ftype = recode(ftype,
                        "产业园区" = "产业园",
                        "港口仓储物流" = "物流园",
                        "污水处理" = "生态环保",
                        "垃圾处理及生物质发电" = "生态环保",
                        "水利设施" = "生态环保",
                        "收费公路" = "高速公路",
                        "天然气发电" = "能源",
                        "房地产租赁经营行业" = "保障性租赁住房",
                        "租赁住房行业" = "保障性租赁住房",
                        "仓储物流" = "物流园",
                        "可再生能源发电" = "能源",
                        "光伏能源发电" = "能源",
                        "商业地产" = "商业",
                        "商业综合体管理服务" = "商业",
                        "消费基础设施" = "商业",
                        "水利发电" = "能源",
                        "购物中心" = "商业",
                        "消费基础设施行业" = "商业"))

dt_ofline <- dt_ofline[str_detect(note, "有效")] %>%
  `[`(, ofl_qty := sum(qty), by = fcode) %>%
  `[`(, ofl_num := .N, by = .(fcode)) %>%
  ### 验证后发现同花顺上 508021.SH 的网下倍数有误
  `[`(fcode == "508021.SH", ofl_multi := ofl_qty/ofl_buy)

dt_ofline_i <- dt_ofline[, fcode:note]
dt_ofline_i %<>%
  mutate(
    itype = case_match(
      itype,
      c("基金公司或其资产管理子公司-对多专户理财产品",
        "基金公司或其资产管理子公司-对-专户理财产品",
        "基金公司或其资产管理计划") ~
        c("公募基金专户"),
      c("保险资金投资账户", "保险资金证券投资账户", "保险机构资产管理产品") ~
        c("保险机构"),
      c("期货公司或其资产管理子公司-对-资产管理计划",
        "期货公司或其资产管理子公司-对多资产管理计划") ~
        c("期货公司"),
      c("证券公司单-资产管理计划", "证券公司定向资产管理计划",
        "证券公司集合资产管理计划") ~
        c("券商资管计划"),
      c("机构自营投资账户") ~ c("券商自营"),
      .default = itype
    )
  )

dt_ofline_j <- dt_ofline %>%
  select(!icode:note) %>%
  select(fcode:ofl_place, ofl_qty, ofl_buy, ofl_multi, ofl_num,
         pub_place:dividend) %>% unique()

## output excel
dt_refined %>%
  # filter(note == "有效报价") %>%
  mutate(
    Listdate = vlookup(fcode, dt_ofline_j[, .(fcode, Listdate)]),
    value = qty * vlookup(fcode, dt_ofline_j[, .(fcode, plist)]),
    itype = str_replace_all(itype, "-", "一"),
    ofrate = ofrate * 100
  ) %>%
  select(fcode, fname, iname, qty, itype, value, Listdate, note, ofrate) %>%
  `names<-`(c("证券代码", "证券简称", "网下账户名称", "拟认购份额", "网下账户类型(穿透)",
              "拟认购金额", "上市日期", "备注", "网下确认比例%")) %>%
  write.xlsx(file = "REITs网下拟认购明细.xlsx")

# 4. strategy return ------------------------------------------------
## 本节计算打新收益

shibor <- readxl::read_xlsx("Shibor隔夜.xlsx") %>% setDT() %>%
  `names<-`(c("date", "shibor")) %>%
  `[`(, date := as.character(date)) %>%
  `[`(, shibor := shibor/100)

GC01 <- read_xlsx("国债逆回购.xlsx", sheet = "Sheet2") %>%
  `names<-`(c("date", "GC01")) %>%
  filter(date %in% shibor$date) %>%
  mutate(GC01 = GC01/100)

## 选择融资成本: shibor, GC01
dt_fcost <- `names<-`(GC01, c("date", "cost")) %>%
  mutate(
    next.day = shift(date, n = 1, type = "lead"),
    next.nd = as.numeric(difftime(next.day, date, units = "days")),
    nd = shift(next.nd, n = 1, type = "lead"),
    nd = ifelse(date == "2024-11-28", 3, nd),
    nd = ifelse(date == "2024-11-29", 1, nd),
    cost = cost * nd / 365 + 1
  )

GC01 %>%
  mutate(
    GC01 = GC01 * 100,
    nd = vlookup(date, dt_fcost, 5)
  ) %>%
  write.xlsx("GC01.xlsx")

dt_fcost %<>%
  select(date, cost) %>%
  arrange(date)

## GC01收益率, 复利
r_gc01 <- function(t0, t1, dt_fcost) {
  dt_fcost %>%
    filter(as.Date(date) >= as.Date(t0) & as.Date(date) <= as.Date(t1)) %>%
    pull(cost) %>%
    return()
}

## 严格打新（考虑限售）
dt_ofline_j %<>% apply(MARGIN = 1, function(fc.ofl) {
  ofrate <- as.numeric(fc.ofl["ofrate"])
  pbrate <- as.numeric(fc.ofl["pbrate"])
  ### 计息日期
  t0_ofl <- fc.ofl["Ldate"]; t0_pub <- fc.ofl["Tdate"] # 缴款日
  t_ref <- date_add(fc.ofl["Ldate"], 2)                # 退款日
  t1_norst <- date_add(fc.ofl["Listdate"], -1)         # 无限售卖出日
  t1_rst <- date_add(fc.ofl["Listdate"], 3 - 1)        # 有限售剩余卖出日
  ### 溢价
  RP1 <- as.numeric(fc.ofl["pc_1"]) / as.numeric(fc.ofl["plist"])
  RP4 <- as.numeric(fc.ofl["pc_4"]) / as.numeric(fc.ofl["plist"])
  ### 网下首日收益
  OC_ofl_norst <- prod(r_gc01(t0_ofl, t1_norst, dt_fcost))
  R0_ofl_norst <- prod(r_gc01(t_ref, t1_norst, dt_fcost))
  R_ofl_norst <- ofrate * RP1 + (1 - ofrate) * R0_ofl_norst - OC_ofl_norst
  R_ofl_norst_4 <- R_ofl_norst * prod(r_gc01(fc.ofl["Listdate"], t1_rst, dt_fcost))
  ### 网下有限售4日收益
  OC_ofl_rst <- prod(r_gc01(t0_ofl, t1_rst, dt_fcost))
  R0_ofl_rst <- prod(r_gc01(t_ref, t1_rst, dt_fcost))
  R1_ofl_rst_1 <- ofrate * 0.2 * RP1 * prod(r_gc01(fc.ofl["Listdate"], t1_rst, dt_fcost))
  R1_ofl_rst_4 <- ofrate * 0.8 * RP4
  R_ofl_rst <- R1_ofl_rst_1 + R1_ofl_rst_4 + (1 - ofrate) * R0_ofl_rst - OC_ofl_rst
  ### 公众首日收益
  OC_pub <- prod(r_gc01(t0_pub, t1_norst, dt_fcost))
  R0_pub <- prod(r_gc01(t_ref,  t1_norst, dt_fcost))
  R_pub <- pbrate * RP1 + (1 - pbrate) * R0_pub - OC_pub
  ### 公众第4日收益
  OC_pub_4 <- prod(r_gc01(t0_pub, t1_rst, dt_fcost))
  R0_pub_4 <- prod(r_gc01(t_ref,  t1_rst, dt_fcost))
  R_pub_4 <- pbrate * RP4 + (1 - pbrate) * R0_pub_4 - OC_pub_4
  ### 回收结果
  data.table(
    "fcode" = fc.ofl["fcode"],
    "dxr_ofl_norst" = 100 * R_ofl_norst,
    "dxr_ofl_norst_4" = 100 * R_ofl_norst_4,
    "dxr_ofl_rst" = 100 * R_ofl_rst,
    "dxr_pub" = 100 * R_pub,
    "dxr_pub_4" = 100 * R_pub_4
  ) %>%
    return()
}, simplify = FALSE) %>%
  rbindlist() %>% {
    left_join(dt_ofline_j, ., by = "fcode")
  }

## 长时间持有（60日panel）
dt_dxr_D60 <- readxl::read_xlsx("REITs基金收益率-v2.xlsx", sheet = "D60-Value") %>%
  as.data.table() %>%
  rename(fcode = `证券代码`, fname = `证券名称`, Listdate = `上市日期`,
         po_d0 = `首日开盘价`, plist = `发行价`) %>%
  mutate(
    `61` = vlookup(fcode, dt_return[, .(`证券代码`, pc_today)])
  ) %>%
  select(-c(po_d0, plist)) %>%
  pivot_longer(
    cols = -c("fcode", "fname", "Listdate"),
    values_to = "pc", names_to = "tt"
  ) %>%
  rowwise() %>%
  mutate(
    tt = as.numeric(tt),
    days.listed = which(shibor$date == Listdate)
  ) %>%
  ungroup() %>%
  filter((tt + 1 <= days.listed) | tt == 61) %>%
  select(-days.listed) %>%
  left_join(
    dt_ofline_j[, .(fcode, ofrate, pbrate, restrict, Ldate, Tdate, plist)],
    by = "fcode"
  ) %>%
  as.data.table() %>%
  `[`(, c("OC", "R0") := .(
    prod(r_gc01(Ldate, date_add(Listdate, tt - 1), dt_fcost)),
    prod(r_gc01(date_add(Ldate, 2), date_add(Listdate, tt - 1), dt_fcost))
  ), by = .(fcode, tt)) %>%
  mutate(
    dxr_fc = c((1 - ofrate) * R0 + ofrate * pc / plist - OC) * 100,
    dxr_wofc = c((1 - ofrate) * 1 + ofrate * pc / plist - 1) * 100
  ) %>%
  select(-c(OC, R0))

# 5. strategic investor ------------------------------------------------
## 本节整理战略投资者信息

dt_str <- readxl::read_excel(path = "REITs获配机构明细20241129.xlsx", sheet = 1) %>%
  setDT() %>%
  `[`(, .(
    fcode = `万得代码`,
    fname = `项目简称`,
    iname = `配售对象名称`,
    is_corr = `原始权益人或其关联方（Y/N）`,
    type_cicc = `投资者类型`,
    qty = `实际配售数量(万份)`
    #pct = `占总份额比例(%)`
  ))
# `[`(fname == "国泰君安城投宽庭保租房REIT", fcode := "508031.SH")
dt_str_type <- readxl::read_excel(path = "REITs获配机构明细20241129.xlsx",
                                  sheet = "mapping") %>%
  setDT() %>% `[`(, -2) %>% `names<-`(c("iname", "type_pen"))

dt_str %<>%
  mutate(
    type_pen = fuzzy_vlookup(iname, dt_str_type$iname, dt_str_type$type_pen,
                             return_val = T),
    ## type_pen_check1 = fuzzy_vlookup(iname, dt_str_type$iname) == iname,
    ## type_pen_check2 = vlookup(iname, dt_str_type) == type_pen,
    type_pen = if_else(is_corr == "Y", "原始权益人或其关联方", type_pen)
  )

dt_str %<>%
  mutate(qty_type = sum(qty), .by = c(fcode, type_pen)) %>%
  mutate(qty_ttl =
           vlookup(fcode, dt_ofline_j[, .(fcode, str_place)]) +
           vlookup(fcode, dt_ofline_j[, .(fcode, ofl_place)]) +
           vlookup(fcode, dt_ofline_j[, .(fcode, pub_place)]),
         pct_type = qty_type/qty_ttl * 100)

rm(dt_restrict, dt_str_type, ls_dt_raw, ls_dt_refined)

# 6. further info ------------------------------------------------
## 本节整理二级市场等信息，来自``打新数据.xlsx''文件

dt_cashflow <- readxl::read_excel(path = "打新数据-v2.xlsx",
                                  sheet = "拟募集金额per share & 发行预测分派率") %>%
  setDT() %>%
  `[`(c(3:4, 22:24), ) %>% t() %>%
  `colnames<-`(.[1, ]) %>% as.data.table() %>% `[`(-1, ) %>%
  `names<-`(c("fname", "fcode", "famt", "famt_persh", "dividend_1st")) %>%
  mutate(across(c(famt, famt_persh, dividend_1st), ~ as.numeric(.))) %>%
  mutate(
    dividend_1st = dividend_1st * 100,
    ## should use dividend_1st as dividend rate
    dividend = vlookup(fcode, dt_ofline_j[, .(fcode, dividend)]),
    public_amt =
      famt - vlookup(fcode, dt_ofline_j[, .(fcode, str_place)]) *
      vlookup(fcode, dt_ofline_j[, .(fcode, plist)]) / 1e4
  ) %>%
  mutate(across(c(famt, public_amt), ~ round(. * 1e4, 3))) %>%
  ## 变量名释义：fund_code, 拟募集金额, 流通盘金额, 拟募集金额/share, 分派率
  select(fcode, famt, public_amt, famt_persh, dividend_1st)

dt_ofline_j[, dividend := vlookup(fcode, dt_cashflow[, .(fcode, dividend_1st)])]
dt_ofline[, dividend := vlookup(fcode, dt_cashflow[, .(fcode, dividend_1st)])]

## 二级市场分派率
dt_smdivd <- readxl::read_excel(path = "打新数据-v2.xlsx",
                                sheet = "二级市场分派率",
                                col_names = F) %>%
  t() %>% `colnames<-`(.[1, ]) %>% as.data.table() %>% `[`(-1, ) %>%
  pivot_longer(cols = starts_with("20"), names_to = "date",
               values_to = "smdivd") %>%
  mutate(smdivd = as.numeric(smdivd) * 100)

## 二级市场市值
dt_smv <- readxl::read_excel(path = "打新数据-v2.xlsx",
                             sheet = "市值", col_names = F) %>%
  t() %>% `colnames<-`(.[1, ]) %>% as.data.table() %>% `[`(-1, ) %>%
  pivot_longer(cols = starts_with("20"), names_to = "date",
               values_to = "smv") %>%
  mutate(smv = round(as.numeric(smv) * 1e4, 3))

dt_term <- readxl::read_excel(path = "打新数据-v2.xlsx",
                              sheet = "现金流剩余期限", col_names = F) %>%
  `names<-`(unlist(.[1, ])) %>% `[`(-1, ) %>% as.data.table() %>%
  `[`(, .(fcode = `代码`,
          term = as.numeric(`剩余期限`),
          estab_date = `基金成立日`,
          end_date = `权属到期日`))

dt_sm <- dt_smdivd %>% setDT() %>%
  `[`(as.data.table(dt_smv)[, .(fcode, date, smv)], on = .(fcode, date)) %>%
  apply(MARGIN = 1, function(xrow) {
    difftime(dt_term[fcode == xrow["fcode"], as.Date(end_date)],
             as.Date(xrow["date"]),
             units = "days") %>% as.numeric() -> terms_left
    c(xrow, "term" = terms_left/365) %>% return()
  }, simplify = FALSE) %>% {do.call(rbind, .)} %>% as.data.table() %>%
  mutate(across(6:8, ~ as.numeric(.)))

dt_sm <- dt_sm %>%
  filter(date == "2021-06-21") %>%
  mutate(
    date = "2021-06-07",
    term = term - 14/365,
    smdivd = NA, smv = NA
  ) %>%
  rbind(dt_sm) %>%
  mutate(ftype = recode(
    ftype,
    "仓储物流" = "物流园",
    "园区基础设施" = "产业园",
    "生态环保" = "生态环保",
    "交通基础设施" = "高速公路",
    "能源基础设施" = "能源",
    "保障性租赁住房" = "保障性租赁住房",
    "消费基础设施" = "商业"
  ))

dt_ofline_j %<>%
  mutate(
    fclass = vlookup(fcode, unique(dt_sm[, .(fcode, fclass)])),
    Xdate = vlookup(fcode, dt_info[, .(fcode = `证券代码`, Xdate = `询价日`)]),
    Estdate = vlookup(fcode, dt_term[, .(fcode, estab_date)])
  ) %>%
  select(
    fcode:restrict, Xdate, Tdate, Ldate, Estdate, Listdate,
    ftype, fclass, plist, p_up:dxr_pub_4
  ) %>%
  apply(MARGIN = 1, function(fund) {
    ### avg_term: 基金成立日时, 已有项目的平均期限
    dt_sm[date == fund["Estdate"] & !is.na(smv) & ftype == fund["ftype"], ] %>%
      mutate(avg_term = weighted.mean(term, smv)) %>%
      select(avg_term) %>% unique() %>% unlist() -> avg_term
    avg_term <- ifelse(length(avg_term) == 0, NA, avg_term)
    ### avg_smdivd: 询价日时, 已有项目的平均分派率
    dt_sm[date == fund["Xdate"] & !is.na(smv) & ftype == fund["ftype"], ] %>%
      mutate(avg_smdivd = weighted.mean(smdivd, smv)) %>%
      select(avg_smdivd) %>% unique() %>% unlist() -> avg_smdivd
    avg_smdivd <- ifelse(length(avg_smdivd) == 0, NA, avg_smdivd)
    ### term: 到期日-基金成立日
    term <- dt_sm[fcode == fund["fcode"] & date == fund["Estdate"], term]
    term <- ifelse(length(term) == 0, NA, term)
    
    avg_sm <- c("avg_smdivd" = avg_smdivd, "avg_term" = avg_term, "term" = term)
    return(c(fund, avg_sm))
  }, simplify = F) %>% {do.call(rbind, .)} %>% as.data.table() %>%
  mutate(across(c(ofrate, pbrate, plist:term), ~ as.numeric(.))) %>%
  mutate(across(c(str_place:ofl_buy, pub_place, pub_buy), ~ . * plist)) %>%
  `[`(dt_cashflow[, -"dividend_1st"], on = .(fcode))

# 7. sector info ------------------------------------------------
dt_sector <- read.xlsx(
  "板块.xlsx", sheet = "附 细分板块表现 (后复权)", detectDates = TRUE
) %>%
  select(1, 1 + which(str_detect(slice(., 5), "^\\d{4}-\\d{2}-\\d{2}$"))) %>%
  slice(-c(1:4)) %>%
  `names<-`(c("date", "产业园", "仓储物流", "生态环保",
              "能源", "高速公路", "保租房", "消费")) %>%
  pivot_longer(
    cols = -date,
    names_to = "ftype",
    values_to = "secind"
  ) %>%
  mutate(secind = as.numeric(secind)) %>%
  arrange(ftype, date) %>%
  mutate(
    secind_20d = (secind - lag(secind, 20)) / lag(secind, 20) * 100,
    secind_60d = (secind - lag(secind, 60)) / lag(secind, 60) * 100,
    .by = ftype
  ) %>%
  mutate(
    .by = ftype,
    secind_60d_fst = zoo::rollapply(
      secind, width = 60, align = "right",
      FUN = function(x) first(na.omit(x)),
      fill = NA, partial = TRUE
    ),
    secind_60d_fst = (secind / lag(secind_60d_fst, 1) - 1) * 100
  ) %>%
  mutate(
    .by = ftype,
    secind_60dm = 100 * (secind / shift(secind, n = 1, type = "lag") - 1),
    secind_60dm = zoo::rollapply(
      secind_60dm, width = 60, align = "right",
      FUN = function(x) mean(x, na.rm = TRUE),
      fill = NA, partial = TRUE
    )
  )

dt_sector <- read.xlsx(
  "板块.xlsx", sheet = "附 收益情况及流通情况",
  detectDates = TRUE
) %>%
  select(1:2) %>% slice(-c(1:3)) %>%
  `names<-`(c("date", "index")) %>%
  mutate(index = as.numeric(index)) %>%
  arrange(date) %>%
  mutate(
    index_20d = (index - lag(index, 20)) / lag(index, 20) * 100,
    index_60d = (index - lag(index, 60)) / lag(index, 60) * 100
  ) %>%
  mutate(
    index_60d_fst = zoo::rollapply(
      index, width = 60, align = "right",
      FUN = function(x) first(na.omit(x)),
      fill = NA, partial = TRUE
    ),
    index_60d_fst = (index / lag(index_60d_fst, 1) - 1) * 100
  ) %>%
  mutate(
    index_60dm = 100 * (index / shift(index, n = 1, type = "lag") - 1),
    index_60dm = zoo::rollapply(
      index_60dm, width = 60, align = "right",
      FUN = function(x) mean(x, na.rm = TRUE),
      fill = NA, partial = TRUE
    )
  ) %>%
  right_join(dt_sector, by = c("date"))

# 8. IRR ------------------------------------------------
irr_cal <- function(pv, term, cf) {
  T_1 <- floor(term); T_2 <- term - T_1
  npv <- function(irr) {
    - pv + sum(cf / (1 + irr) ^ (1:T_1)) + cf * T_2 / (1 + irr) ^ term
  }
  irr <- uniroot(npv, interval = c(-0.5, 1))
  return(irr$root)
}

dt_irr <- readxl::read_excel(
  path = "打新数据-v2.xlsx", sheet = "拟募集金额per share & 发行预测分派率"
) %>%
  setDT() %>% `[`(c(3:4, 20), ) %>% t() %>%
  `colnames<-`(.[1, ]) %>% as.data.table() %>% `[`(-1, ) %>%
  `names<-`(c("fname", "fcode", "cf_persh")) %>%
  mutate(across(cf_persh, ~ as.numeric(.))) %>%
  left_join(
    select(dt_ofline_j, fcode, fclass, ftype, term, p_low, p_up), by = "fcode"
  ) %>%
  mutate(
    p_med = (p_low + p_up) / 2,
  ) %>%
  select(fname, fcode, fclass, ftype, cf_persh, term, p_med) %>%
  rowwise() %>%
  mutate(
    irr = irr_cal(pv = p_med, term = term, cf = cf_persh) * 100
  ) %>%
  ungroup()

## 板块均值
dt_irr_sm <- read.xlsx("REITs基金价格-询价日.xlsx", sheet = "Sheet2") %>%
  select(which(!duplicated(names(.)))) %>%
  rename(fcode = "证券代码", fname = "证券名称") %>%
  pivot_longer(
    cols = -c(fcode, fname),
    names_to = "date",
    values_to = "pc"
  ) %>%
  left_join(
    select(dt_sm, fcode, ftype, date, smdivd, smv, term),
    join_by(fcode, date)
  ) %>%
  filter(pc != 0) %>%
  mutate(cf_persh = pc * smdivd / 100) %>%
  rowwise() %>%
  mutate(
    irr_sm = irr_cal(pv = pc, term = term, cf = cf_persh) * 100
  ) %>%
  ungroup() %>%
  mutate(
    .by = c("ftype", "date"),
    avg_irr_sm = weighted.mean(irr_sm, smv)
  ) %>%
  select(date, ftype, avg_irr_sm) %>%
  unique()

dt_irr %<>%
  mutate(
    Xdate = vlookup(fcode, dt_ofline_j[, .(fcode, Xdate)])
  ) %>%
  left_join(
    dt_irr_sm, join_by(Xdate == date, ftype)
  )

dt_ofline_j %<>%
  left_join(
    select(dt_irr, fcode, contains("irr")), by = "fcode"
  )

# 9. public ------------------------------------------------
dt_public_j <- dt_ofline_j %>%
  select(fcode, ftype, Tdate) %>%
  apply(MARGIN = 1, function(fund) {
    dt_sm[date == fund["Tdate"] & !is.na(smv) & ftype == fund["ftype"], ] %>%
      mutate(avg_smdivd = weighted.mean(smdivd, smv)) %>%
      select(avg_smdivd) %>% unique() %>% unlist() -> avg_smdivd
    avg_smdivd <- ifelse(length(avg_smdivd) == 0, NA, avg_smdivd)
    return(c(fund, "avg_smdivd" = avg_smdivd))
  }, simplify = F) %>% {do.call(rbind, .)} %>% as.data.table() %>%
  mutate(avg_smdivd = as.numeric(avg_smdivd)) %>%
  left_join(
    select(dt_ofline_j, fcode, dividend, famt_persh, plist, term, avg_term),
    by = "fcode"
  ) %>%
  mutate(
    dividend = dividend * famt_persh / plist,
    cf_persh = dividend * plist / 100
  ) %>%
  select(-famt_persh) %>%
  rowwise() %>%
  mutate(
    irr = irr_cal(pv = plist, term = term, cf = cf_persh) * 100
  ) %>%
  ungroup() %>%
  left_join(
    dt_irr_sm, join_by(Tdate == date, ftype)
  )

# 10. amount ------------------------------------------------
dt_famt <- read.xlsx("info.xlsx", sheet = "总市值和流通市值因子") %>%
  t() %>% as.data.table() %>%
  `names<-`(unlist(.[1, ])) %>%
  slice(-1) %>% select(1, 2, 6:9) %>%
  `names<-`(c("fcode", "fname", "famt_act", "pubfamt_act", "famt", "pubfamt")) %>%
  mutate(across(-c(fcode, fname), ~ as.numeric(.)))

# 11. I/O ------------------------------------------------
save(
  dt_info, dt_rate, dt_refined, dt_return, dt_str, dt_sm, dt_cashflow,
  dt_fcost, r_gc01,
  dt_sector, dt_irr, dt_famt,
  dt_ofline, dt_ofline_i, dt_ofline_j, dt_public_j, dt_dxr_D60,
  file = "MAIN_V2.RData"
)
## 至此全部数据准备工作结束，将数据保存于MAIN.RData文件中