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

# 1. j-level, renewed ------------------------------------------------
## 基于核密度抽样
gen_data_KDE <- function(.data, should.plot = FALSE, n.new = 1000) {
  ## 计算最优带宽矩阵
  H <- Hpi(x = .data)
  fhat <- kde(x = .data, H = H)
  ## 从估计的密度中抽样
  set.seed(123)
  new_samples <- rkde(fhat, n = n.new)
  ## 将新样本转换为数据框
  new_data <- data.frame(new_samples[, 1], new_samples[,2]) %>%
    `names<-`(names(.data))
  if (should.plot == TRUE) {
    par(mfrow = c(3, 2))
    ## 绘制数据直方图
    hist(unlist(.data[, 1]), breaks = 30,
         main = "Original Data", xlab = names(.data)[1])
    hist(unlist(new_data[, 1]), breaks = 30,
         main = "Generated Data", xlab = names(.data)[1])
    hist(unlist(.data[, 2]), breaks = 30,
         main = "Original Data", xlab = names(.data)[2])
    hist(unlist(new_data[, 2]), breaks = 30,
         main = "Generated Data", xlab = names(.data)[2])
    
    ## 绘制数据散点图
    plot(unlist(.data[, 1]), unlist(.data[, 2]), main = "Original Data",
         xlab = names(.data)[1], ylab = names(.data)[2])
    plot(unlist(new_data[, 1]), unlist(new_data[, 2]), main = "Generated Data",
         xlab = names(.data)[1], ylab = names(.data)[2])
    plot_object <- recordPlot()
    print(plot_object)
    return(list(data = new_data, plot = plot_object))
  }
  return(new_data)
}

dt_ofline_j %<>%
  mutate(fclass = vlookup(fcode, unique(dt_sm[, .(fcode, fclass)]))) %>%
  select(fcode:ftype, fclass, Tdate:ncol(.))

dt_sm %<>%
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

# 2. LOESS ------------------------------------------------
## 准备LOESS数据: N = 35
dt_ofline_loess <- dt_ofline_j %>%
  mutate(dif_divd = dividend - avg_smdivd,
         dif_term = term - avg_term) %>%
  select(fcode, fclass, ftype, dxr = dxr_ofl_rst, dif_divd, dif_term, fclass) %>%
  filter(!is.na(dif_divd)) %>%
  mutate(fclass = ifelse(fclass == "产权类", 1, 0)) %>%
  setDT()

## LOESS
loess_model <- loess(
  dxr ~ dif_divd + dif_term,
  data = dt_ofline_loess,
  parametric = "fclass",
  span = 0.5,
  control = loess.control(
    surface = "direct",
    statistics = "exact",
    trace.hat = "exact"
  )
)

## 预测与真实趋势对比
dt_ofline_loess %>%
  mutate(dxr_predicted = predict(loess_model, dt_ofline_loess)) %>%
  mutate(fclass = ifelse(fclass == 1, "产权类", "经营权类")) %>% {
    ggplot(data = .,
           mapping = aes(x = dxr_predicted, y = dxr,
                         group = fclass, color = fclass)) +
      geom_point(alpha = 0.7) +
      geom_smooth(linewidth = 0.7, se = F, method = "lm") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_color_manual(
        name = NULL, values = c("产权类" = "#5b0d07", "经营权类" = "#b68e55")
      ) +
      labs(x = "LOESS预测首日超额收益率%", y = "真实首日超额收益率%") +
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))
  } -> fig_loess_fit

### 坐标系网格
dt_grid <- rbind(
  cbind(
    expand.grid(
      dif_divd = rseq(-2,  4, by = 0.01, digits = 2),
      dif_term = rseq(-10, 7, by = 0.1,  digits = 2)
    ),
    fclass = 1
  ),
  cbind(
    expand.grid(
      dif_divd = rseq(-5,  5, by = 0.01, digits = 2),
      dif_term = rseq(-5, 15, by = 0.1,  digits = 2)
    ),
    fclass = 0
  )
) %>% as.data.table() %>%
  mutate(dxr_predicted = predict(loess_model, .))

### 等高线图
fig_grid_prop <- fig_grid_oprt <- dt_grid %>%
  filter(
    fclass == 1
  ) %>% {
    ggplot(mapping = aes(x = dif_divd, y = dif_term)) +
      ### 0以上部分
      geom_contour_filled(
        data = .[dxr_predicted >= 0,],
        mapping = aes(z = dxr_predicted),
        bins = 17, alpha = 0.9
      ) + scale_fill_manual(
        values = color_scale(17, dark = "#5b0d07", light = "white"),
        name = NULL
      ) +
      guides(fill = "none") +
      ### 0以下部分
      new_scale_fill() +
      geom_contour_filled(
        data = .[dxr_predicted <= 0,],
        mapping = aes(z = dxr_predicted),
        bins = 17, alpha = 0.9
      ) + scale_fill_manual(
        values = color_scale(17, dark = "white", light = "#804d05"),
        name = NULL
      ) +
      guides(fill = "none") +
      ## 隐形散点图
      new_scale_fill() +
      new_scale_color() +
      geom_point(
        data = data.table(
          dif_divd = rep(0, 10),
          dif_term = rep(0, 10),
          zz = c(seq(-6, 0, length.out = 5), seq(0, 3, length.out = 5))
        ),
        alpha = 0,
        mapping = aes(color = zz)
      ) +
      scale_color_gradient2(name = "产权类\n打新收益率",
                            low = "#804d05",
                            mid = "white",
                            high = "#5b0d07") +
      labs(x = "分派率% - 二级市场均值", y = "剩余期限 (年) - 二级市场均值") +
      theme_bw() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
  }

fig_grid_oprt <- dt_grid %>%
  filter(
    fclass == 0,
    -2 <= dif_divd & dif_divd <= 1,
    -5 <= dif_term & dif_term <= 15
  ) %>% {
    ggplot(mapping = aes(x = dif_divd, y = dif_term)) +
      ### 0以上部分
      geom_contour_filled(
        data = .[dxr_predicted >= 0,],
        mapping = aes(z = dxr_predicted),
        bins = 15, alpha = 0.9
      ) + scale_fill_manual(
        values = color_scale(15, dark = "#5b0d07", light = "white"),
        name = NULL
      ) +
      guides(fill = "none") +
      ### 0以下部分
      new_scale_fill() +
      geom_contour_filled(
        data = .[dxr_predicted <= 0,],
        mapping = aes(z = dxr_predicted),
        bins = 15, alpha = 0.9
      ) + scale_fill_manual(
        values = color_scale(15, dark = "white", light = "#804d05"),
        name = NULL
      ) +
      guides(fill = "none") +
      ## 隐形散点图
      new_scale_fill() +
      new_scale_color() +
      geom_point(
        data = data.table(
          dif_divd = rep(0, 10),
          dif_term = rep(0, 10),
          zz = c(seq(-2.5, 0, length.out = 5), seq(0, 2.5, length.out = 5))
        ),
        alpha = 0,
        mapping = aes(color = zz)
      ) +
      scale_color_gradient2(name = "经营权类\n打新收益率",
                            low = "#804d05",
                            mid = "white",
                            high = "#5b0d07") +
      # coord_cartesian(xlim = c(-2, 1), ylim = c(-5, 15)) +
      # xlim(-2, 1) + ylim(-5, 15) +
      labs(x = "分派率% - 二级市场均值", y = "剩余期限 (年) - 二级市场均值") +
      theme_bw() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
  }



