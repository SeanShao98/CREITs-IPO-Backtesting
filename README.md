# CREITs-IPO-Backtesting
Backtesting of C-REITs IPO strategy returns

author: yifu.shao@CICC

The author holds the copyright for this project. This project is part of the Fixed Income Research Group at CICC RS Department. The code in this project represents non-confidential technical information developed and published by Yifu Shao as part of related topics within the Fixed Income Research Group at CICC. The purpose of open-sourcing this code is to share algorithms for strategy return calculations and data processing workflows. Copyright for the code belongs to the author, and usage is subject to the MIT open-source license.

**Special Disclaimer:** The reliability and applicability of the publicly disclosed content of this code are for reference only. The publication of this code does not constitute any liability on the part of CICC for any consequences, nor does it serve as any form of investment advice. For data beyond the publicly available information **in the repository**, please contact the relevant personnel at CICC.

**Notice:** The data and code in the repository have been stripped of potential confidential information, making the project non-reproducible. Additionally, certain displayed output results have also been redacted for confidentiality.

## intro

The fundraising for C-REITs **precedes** their public listing on the stock exchange. Specifically, the fund’s total raised amount is determined during the filing stage, with fundraising targeting **strategic investors**, **offline investors**, and **public investors**. After confirming the subscription shares of strategic investors, the fund manager initiates a public bidding process for offline investors (i.e., the **pricing inquiry phase** of the C-REITs issuance). Based on the bids submitted by offline investors, the fund manager determines the offering price and announces the fundraising period. During this period, offline investors are required to pay the subscription amount at the offering price. Public investors may also purchase any desired amount of fund shares at the offering price during the fundraising period.

Actual subscriptions often **exceed** the allocated fund shares, resulting in the fund manager refunding the excess amount after the fundraising period. However, interest generated during the fundraising period is **not refunded** but is instead credited to the fund shares. After listing, both offline and public investors can (conditionally) sell the shares allocated during the fundraising period on the secondary market. The strategy of subscribing to fund shares during the fundraising period and selling them shortly after listing is referred to as **C-REITs IPO trading**. Similarly, the strategy of buying fund shares shortly after listing on the secondary market and then selling them is referred to as **C-REITs post-IPO trading**.

This project calculates the returns of strict IPO/post-IPO trading strategies, using GC01 (204001.SH) as the cost of funds (risk-free return) and backtests the excess returns of these strategies. Furthermore, the project investigates the factors influencing the returns of IPO/post-IPO trading strategies.

**Note:** Since the relevant data originates from the Chinese market, the associated code, data, and annotations inevitably contain Chinese characters.

## repository

`ini.R`: initializing profile

`proc_v2_1_data.R`: (main `R` code) data processing and strategy return calculation (IPO trading)

`proc_v2_2_dx_1.R`: (main `R` code) factor analysis (IPO trading), part 1

`proc_v2_3_dx_2.R`: (main `R` code) factor analysis (IPO trading), part 2

`proc_v2_4_cx.R`: (main `R` code) strategy return calculation (post-IPO trading)

`REITs估值-v2.xlsx`: public data of fund valuation

`REITs基金价格-询价日.xls`: public data of pricing inquiry

`REITs基金信息-v2.xlsx`: public data of C-REITs info

`REITs基金收益率-v2.xlsx`: public data of C-REITs closing price

`Shibor隔夜.xlsx`: public data of SHIBOR

`国债逆回购.xlsx`: public data of GC01 (annualized)

`比例公告目录.xlsx`: public data of offline rate

`限售.xlsx`: public data of restriction condition

`output/`: (part of) output files (only for description)





