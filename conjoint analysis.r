preference <-read.csv("tv_preference.csv")

library(dplyr)

team <- preference %>%
  filter(Name=="William Gao"|Name =="Ethan"|Name =="Lan Mei"|Name =="heidi"|Name =="shini yang")

colnames(team)

screen52 <- team$Screen.52.inch
screen65 <- team$Screen.65.inch
technology <- team$X2D.or.3D
brand <- team$Sony...1
price <- team$Price..low...0..hi..1.


fit1 <- lm(team$Preference.Ranks ~ screen52+screen65+technology+brand+price)
summary(fit1)

# lm(formula = team$Preference.Ranks ~ screen52 + screen65 + technology + 
#      brand + price)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -13.958  -5.098  -1.471   6.108  14.458 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   12.808      1.528   8.380 1.56e-13 ***
#   screen52      -0.225      1.528  -0.147   0.8832    
# screen65       0.750      1.528   0.491   0.6246    
# technology     0.900      1.248   0.721   0.4723    
# brand          1.400      1.248   1.122   0.2643    
# price         -3.267      1.248  -2.618   0.0101 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.835 on 114 degrees of freedom
# Multiple R-squared:  0.07375,	Adjusted R-squared:  0.03313 
# F-statistic: 1.815 on 5 and 114 DF,  p-value: 0.1153
# 

# a. Partworth estimates
intercept <- coef(fit1)[1]
coef_screen52 <- coef(fit1)[2]
coef_screen65 <- coef(fit1)[3]
coef_technology <- coef(fit1)[4]
coef_brand <- coef(fit1)[5]
coef_price <- coef(fit1)[6]

partworth_df <- rbind(intercept,coef_screen52,coef_screen65,coef_technology,coef_brand,coef_price)
colnames(partworth_df) <- c("Partworth_Coef")
partworth_df
#                    Partworth_Coef
# intercept            12.808333
# coef_screen52        -0.225000
# coef_screen65         0.750000
# coef_technology       0.900000
# coef_brand            1.400000
# coef_price           -3.266667

# b. Attribute importance
range_screen <- abs(coef_screen65 - coef_screen52)
range_technology <- abs(coef_technology-0)
range_brand <- abs(coef_brand-0)
range_price <- abs(coef_price-0)

importance_tb <- as_tibble(rbind(range_screen,range_technology,range_brand,range_price))
colnames(importance_tb) <- c("Range")

tb <- importance_tb%>%
  mutate(Importance = round(Range / sum(Range), 2))

importance_df <- as.data.frame(tb)
rownames(importance_df) <- c("Screen", "Technology", "Brand", "Price")
importance_df

# Range Importance
# Screen     0.975000       0.15
# Technology 0.900000       0.14
# Brand      1.400000       0.21
# Price      3.266667       0.50

# c. Dollar value of 1 util 
price_range <- 2500 - 2000
(unit_util <- price_range / range_price)
# 153.0612

# d. Willingness to pay for screen size, brand name, and 3D technology.
(wtp_screen65 <- unit_util*coef_screen65)
# 114.7959 
(wtp_screen52 <- unit_util*coef_screen52)
# -34.43878
(wtp_sony <- unit_util*coef_brand)
# 214.2857 
(wtp_3d <- unit_util*coef_technology)
# 137.7551
WTP_df <- rbind(wtp_screen65,wtp_screen52,wtp_sony,wtp_3d)
colnames(WTP_df) <- c("Willingness To Pay")

# f. Market share at best price for any design
est_partworth <- c(intercept, coef_screen52, coef_screen65, coef_technology, coef_brand, coef_price)

#same order as est_partworth
design_sony_best_price <- c(1,1,0,1,1, (2500-2000)/(2500-2000))
design_sharp_best_price <- c(1,0,1,1,0, (2000-2000)/(2500-2000))

cost <- c(1000,500,1000,250,250)

design1 <- c(1,0,0,0,0)
design2 <- c(1,0,0,0,1)
design3 <- c(1,0,0,1,0)
design4 <- c(1,0,1,0,0)
design5 <- c(1,1,0,0,0)
design6 <- c(1,0,0,1,1)
design7 <- c(1,0,1,1,0)
design8 <- c(1,0,1,0,1)
design9 <- c(1,1,0,1,0)
design10 <- c(1,1,0,0,1)
design11 <- c(1,0,1,1,1)
design12 <- c(1,1,0,1,1)


net_cost1 <- sum(cost * design1)
net_cost2 <- sum(cost * design2)
net_cost3 <- sum(cost * design3)
net_cost4 <- sum(cost * design4)
net_cost5 <- sum(cost * design5)
net_cost6 <- sum(cost * design6)
net_cost7 <- sum(cost * design7)
net_cost8 <- sum(cost * design8)
net_cost9 <- sum(cost * design9)
net_cost10 <- sum(cost * design10)
net_cost11 <- sum(cost * design11)
net_cost12 <- sum(cost * design12)


#design with best price
price <- best_price <- 2000
design1_price <- c(1,0,0,0,0, (price-2000)/(2500-2000))
design2_price <- c(1,0,0,0,1, (price-2000)/(2500-2000))
design3_price <- c(1,0,0,1,0, (price-2000)/(2500-2000))
design4_price <- c(1,0,1,0,0, (price-2000)/(2500-2000))
design5_price <- c(1,1,0,0,0, (price-2000)/(2500-2000))
design6_price <- c(1,0,0,1,1, (price-2000)/(2500-2000))
design7_price <- c(1,0,1,1,0, (price-2000)/(2500-2000))
design8_price <- c(1,0,1,0,1, (price-2000)/(2500-2000))
design9_price <- c(1,1,0,1,0, (price-2000)/(2500-2000))
design10_price <- c(1,1,0,0,1, (price-2000)/(2500-2000))
design11_price <- c(1,0,1,1,1, (price-2000)/(2500-2000))
design12_price <- c(1,1,0,1,1, (price-2000)/(2500-2000))

#Utility competition
(Utility_sony <- sum(design_sony_best_price * est_partworth))
(Utility_sharp <- sum(design_sharp_best_price * est_partworth))

#attractiveness competition
(attractiveness_sony <- exp(Utility_sony))
(attractiveness_sharp <- exp(Utility_sharp))

# my designs
Utility_design1_best_price <- sum(design1_price * est_partworth)
Utility_design2_best_price <- sum(design2_price * est_partworth)
Utility_design3_best_price <- sum(design3_price * est_partworth)
Utility_design4_best_price <- sum(design4_price * est_partworth)
Utility_design5_best_price <- sum(design5_price * est_partworth)
Utility_design6_best_price <- sum(design6_price * est_partworth)
Utility_design7_best_price <- sum(design7_price * est_partworth)
Utility_design8_best_price <- sum(design8_price * est_partworth)
Utility_design9_best_price <- sum(design9_price * est_partworth)
Utility_design10_best_price <- sum(design10_price * est_partworth)
Utility_design11_best_price <- sum(design11_price * est_partworth)
Utility_design12_best_price <- sum(design12_price * est_partworth)

#attractiveness
attractiveness_design1_best_price <- exp(Utility_design1_best_price)
attractiveness_design2_best_price <- exp(Utility_design2_best_price)
attractiveness_design3_best_price <- exp(Utility_design3_best_price)
attractiveness_design4_best_price <- exp(Utility_design4_best_price)
attractiveness_design5_best_price <- exp(Utility_design5_best_price)
attractiveness_design6_best_price <- exp(Utility_design6_best_price)
attractiveness_design7_best_price <- exp(Utility_design7_best_price)
attractiveness_design8_best_price <- exp(Utility_design8_best_price)
attractiveness_design9_best_price <- exp(Utility_design9_best_price)
attractiveness_design10_best_price <- exp(Utility_design10_best_price)
attractiveness_design11_best_price <- exp(Utility_design11_best_price)
attractiveness_design12_best_price <- exp(Utility_design12_best_price)

total_attractiveness1 <- attractiveness_design1_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness2 <- attractiveness_design2_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness3 <- attractiveness_design3_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness4 <- attractiveness_design4_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness5 <- attractiveness_design5_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness6 <- attractiveness_design6_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness7 <- attractiveness_design7_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness8 <- attractiveness_design8_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness9 <- attractiveness_design9_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness10 <- attractiveness_design10_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness11 <- attractiveness_design11_best_price + attractiveness_sony + attractiveness_sharp
total_attractiveness12 <- attractiveness_design12_best_price + attractiveness_sony + attractiveness_sharp

#Marketshare of My designs
(marketshare_design1_best_price <- 100 * attractiveness_design1_best_price/total_attractiveness1)
(marketshare_design2_best_price <- 100 * attractiveness_design2_best_price/total_attractiveness2)
(marketshare_design3_best_price <- 100 * attractiveness_design3_best_price/total_attractiveness3)
(marketshare_design4_best_price <- 100 * attractiveness_design4_best_price/total_attractiveness4)
(marketshare_design5_best_price <- 100 * attractiveness_design5_best_price/total_attractiveness5)
(marketshare_design6_best_price <- 100 * attractiveness_design6_best_price/total_attractiveness6)
(marketshare_design7_best_price <- 100 * attractiveness_design7_best_price/total_attractiveness7)
(marketshare_design8_best_price <- 100 * attractiveness_design8_best_price/total_attractiveness8)
(marketshare_design9_best_price <- 100 * attractiveness_design9_best_price/total_attractiveness9)
(marketshare_design10_best_price <- 100 * attractiveness_design10_best_price/total_attractiveness10)
(marketshare_design11_best_price <- 100 * attractiveness_design11_best_price/total_attractiveness11)
(marketshare_design12_best_price <- 100 * attractiveness_design12_best_price/total_attractiveness12)

marketshare_best_price <- rbind(marketshare_design1_best_price,
                                 marketshare_design2_best_price,
                                 marketshare_design3_best_price,
                                 marketshare_design4_best_price,
                                 marketshare_design5_best_price,
                                 marketshare_design6_best_price,
                                 marketshare_design7_best_price,
                                 marketshare_design8_best_price,
                                 marketshare_design9_best_price,
                                 marketshare_design10_best_price,
                                 marketshare_design11_best_price,
                                 marketshare_design12_best_price
                                 )
colnames(marketshare_best_price) <- c("marketshare")
plot(marketshare_best_price)
rownames(marketshare_best_price) <- c("1,0,0,0,0","1,0,0,0,1","1,0,0,1,0","1,0,1,0,0","1,1,0,0,0","1,0,0,1,1","1,0,1,1,0","1,0,1,0,1","1,1,0,1,0","1,1,0,0,1","1,0,1,1,1","1,1,0,1,1")
marketshare_best_price


# g. profit at best price for any design

(profit_design1_best_price <- (2000 - net_cost1) * marketshare_design1_best_price/100)
(profit_design2_best_price <- (2000 - net_cost2) * marketshare_design2_best_price/100)
(profit_design3_best_price <- (2000 - net_cost3) * marketshare_design3_best_price/100)
(profit_design4_best_price <- (2000 - net_cost4) * marketshare_design4_best_price/100)
(profit_design5_best_price <- (2000 - net_cost5) * marketshare_design5_best_price/100)
(profit_design6_best_price <- (2000 - net_cost6) * marketshare_design6_best_price/100)
(profit_design7_best_price <- (2000 - net_cost7) * marketshare_design7_best_price/100)
(profit_design8_best_price <- (2000 - net_cost8) * marketshare_design8_best_price/100)
(profit_design9_best_price <- (2000 - net_cost9) * marketshare_design9_best_price/100)
(profit_design10_best_price <- (2000 - net_cost10) * marketshare_design10_best_price/100)
(profit_design11_best_price <- (2000 - net_cost11) * marketshare_design11_best_price/100)
(profit_design12_best_price <- (2000 - net_cost12) * marketshare_design12_best_price/100)


profit_best_price <- rbind(profit_design1_best_price,
                                profit_design2_best_price,
                                profit_design3_best_price,
                                profit_design4_best_price,
                                profit_design5_best_price,
                                profit_design6_best_price,
                                profit_design7_best_price,
                                profit_design8_best_price,
                                profit_design9_best_price,
                                profit_design10_best_price,
                                profit_design11_best_price,
                                profit_design12_best_price 
                           )
colnames(profit_best_price) <- c("profit")
plot(profit_best_price)
rownames(profit_best_price) <- c("1,0,0,0,0","1,0,0,0,1","1,0,0,1,0","1,0,1,0,0","1,1,0,0,0","1,0,0,1,1","1,0,1,1,0","1,0,1,0,1","1,1,0,1,0","1,1,0,0,1","1,0,1,1,1","1,1,0,1,1")
profit_best_price


# h. best design that yields the largest profit

price_range <- c(1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 3000)

getmaxprofit <- function (design) {
  profit_vec <- c()
  for (price in price_range) {
    current_design = design
    design_price <- append(current_design, (price-2000)/(2500-2000))
    my_design_marketshare <- exp(sum(design_price * est_partworth))/ (exp(sum(design_price * est_partworth)) + attractiveness_sony + attractiveness_sharp)
    profit <- my_design_marketshare * (price - sum(cost*current_design))
    profit_vec <- append(profit_vec, profit)
  }
  max_profit <- max(profit_vec)
  max_price <- price_range[match(max_profit, profit_vec)]
  return(c(max_profit, max_price)) 
}

max1 <- getmaxprofit(design1)
max2 <- getmaxprofit(design2)
max3 <- getmaxprofit(design3)
max4 <- getmaxprofit(design4)
max5 <- getmaxprofit(design5)
max6 <- getmaxprofit(design6)
max7 <- getmaxprofit(design7)
max8 <- getmaxprofit(design8)
max9 <- getmaxprofit(design9)
max10 <- getmaxprofit(design10)
max11 <- getmaxprofit(design11)
max12 <- getmaxprofit(design12)


max_max_df <- rbind(max1,
                    max2,
                    max3,
                    max4,
                    max5,
                    max6,
                    max7,
                    max8,
                    max9,
                    max10,
                    max11,
                    max12
                    )

colnames(max_max_df) <- c("max_profit", "price")
max_max_df
max_max_df[max_max_df[,1] == max(max_max_df[,1])]
# [1] max1 427.3876 1600.0000

#Concludion, in our group's designs, the 1st model 1,0,0,0,0 yields most profit
# Our Group is price sensitive
# Model 1 detail: basic design with 46 inch screen, with 2D, Sharp brand, priced at $1600
