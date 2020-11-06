library(tidyverse)
library(SciViews)

field_data_filepath <- "C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\HE3011_WTP_PPC\\Data\\field_survey_data.csv"
field_survey <- read.csv(field_data_filepath)
online_survey <- read.csv("C:\\Users\\Lui Yu Sen\\Documents\\Github projects\\HE3011_WTP_PPC\\Data\\online_survey_data.csv")

# removed people who don't give income and protest bid, one that government should pay.Didn't remove the respondent who said business should pay
# because he was not willing to pay for conservation in the first place
field_survey <- field_survey[-(which(field_survey$X7..What.is.Your.monthly.income.=="Prefer Not to Say")),]
field_survey <- field_survey[-which(field_survey$If.you.have.ZERO.willingness.to.pay..which.of.the.following.reason.best.explain.your.zero.willingness.to.pay...Please.select.1.option.only.=="The government should pay for it, not me."),]

colnames(field_survey)[seq(2, 12)] <- c("nationality", "age", "conservation", 
                                        "paymentcard_tax", "max_wtp", "zero_wtp", 
                                        "ethnicity", "monthlyincome", "visitationrate", 
                                        "traveltime", "propertyownership")

colnames(online_survey)[seq(2, 12)] <- c("nationality", "age", "conservation", 
                                        "paymentcard_tax", "max_wtp", "zero_wtp", 
                                        "ethnicity", "monthlyincome", "visitationrate", 
                                        "traveltime", "propertyownership")

# 7 observations that reported 0 wtp, unsure or yes should conserve but said gov should pay. These are classified as protest bids and are removed
online_survey <- filter(online_survey, max_wtp != 0 & zero_wtp != "The government should pay for it, not me.")

field_survey <- rbind(field_survey, online_survey)
# rm(online_survey)

field_survey <- mutate(field_survey, visitationrate = str_sub(visitationrate, 1, 3))
for (n in seq(1:nrow(field_survey))){
    field_survey[n, "paymentcard_tax"] <- str_sub(field_survey[n, "paymentcard_tax"],
                                                  3, nchar(field_survey[n, "paymentcard_tax"])
                                                  )
    if (field_survey[n, "monthlyincome"]=="No Income"){
        field_survey[n, "monthlyincome"] <- "0"
    } else if (field_survey[n, "monthlyincome"]=="Below S$1,000"){
        field_survey[n, "monthlyincome"] <- "<1000"
    } else if (field_survey[n, "monthlyincome"]=="S$1,000 â€“ S$1,999"){
        field_survey[n, "monthlyincome"] <- "1000-1999"
    } else if (field_survey[n, "monthlyincome"]=="S$2,000 - S$2,999"){
        field_survey[n, "monthlyincome"] <- "2000-2999"
    } else if (field_survey[n, "monthlyincome"]=="S$3,000 - S$3,999"){
        field_survey[n, "monthlyincome"] <- "3000-3999"
    } else if (field_survey[n, "monthlyincome"]=="S$4,000 - S$4,999"){
        field_survey[n, "monthlyincome"] <- "4000-4999"
    } else if (field_survey[n, "monthlyincome"]=="S$5,000 - S$5,999"){
        field_survey[n, "monthlyincome"] <- "5000-5999"
    } else if (field_survey[n, "monthlyincome"]=="S$6,000 - S$6,999"){
        field_survey[n, "monthlyincome"] <- "6000-6999"
    } else if (field_survey[n, "monthlyincome"]=="S$7,000 - S$7,999"){
        field_survey[n, "monthlyincome"] <- "7000-7999"
    } else if (field_survey[n, "monthlyincome"]=="S$8,000 - S$8,999"){
        field_survey[n, "monthlyincome"] <- "8000-8999"
    } else if (field_survey[n, "monthlyincome"]=="S$9,000 - S$9,999"){
        field_survey[n, "monthlyincome"] <- "9000-9999"
    } else if (field_survey[n, "monthlyincome"]=="S$10,000 and above"){
        field_survey[n, "monthlyincome"] <- ">10000"
    }

    if (field_survey[n, "visitationrate"]=="0 t"){
        field_survey[n, "visitationrate"] <- "0"
    } else if (field_survey[n, "visitationrate"]=="10 "){
        field_survey[n, "visitationrate"] <- ">10"
    }

    if (field_survey[n, "traveltime"]=="0 â€“ 0.5 hours"){
        field_survey[n, "traveltime"] <- "0-30"
    } else if (field_survey[n, "traveltime"]=="0.5â€“ 1 hour"){
        field_survey[n, "traveltime"] <- "30-60"
    } else if (field_survey[n, "traveltime"]=="1 to 1.5 hours"){
        field_survey[n, "traveltime"] <- "60-90"
    } else if (field_survey[n, "traveltime"]=="1.5 to 2 hours"){
        field_survey[n, "traveltime"] <- "90-120"
    }
}

field_survey$visitationrate <- factor(field_survey$visitationrate,
                                      levels = c("0", "1-3", "4-6", "7-9", ">10"),
                                      labels = c("existence", "option", "option", 
                                                 "use", "use"))
field_survey$ethnicity <- factor(field_survey$ethnicity, levels = c("Chinese", "Malay", "Indian", 
                                                                    "Eurasian", "idk"))
field_survey$monthlyincome <- factor(
    field_survey$monthlyincome, levels = c("0","<1000", "1000-1999", "2000-2999", "3000-3999",
                                           "4000-4999", "5000-5999", "6000-6999", "7000-7999",
                                           "8000-8999", "9000-9999", ">10000")
    )
field_survey$traveltime <- factor(field_survey$traveltime,
                                  levels = c("0-30", "30-60", "60-90", "90-120", "More than 2 hours")
                                  labels = c("0-30", "30-60", "60-90", "90-120", ">20")
)
field_survey$nationality <- factor(field_survey$nationality,
                                   levels = c("Singaporean", "Singapore Permanent Resident", "Others"))

field_survey$age <- factor(field_survey$age,
                           levels = c("14", "18-35", "36-50", "51-65", "Above 65"),
                           labels = c("<18", "18-35", "36-50", "51-65", ">65"))

regression_report <- lm(formula = max_wtp ~ monthlyincome + visitationrate + traveltime + age, field_survey)
summary(regression_report)

regression_report_locals <- lm(formula = max_wtp ~ monthlyincome + visitationrate + traveltime + age,
                               filter(field_survey, field_survey$nationality == "Singaporean" |
                                          field_survey$nationality == "Singapore Permanent Resident")
                               )
summary(regression_report_locals)

# test for heteroscedascity
hetero_test <- field_survey
hetero_test <- mutate(hetero_test, residual_squared = regression_report$residuals**2)
hetero_test <- lm(residual_squared ~ monthlyincome + visitationrate + traveltime + age, hetero_test)
summary(hetero_test)
# 0.9745 > 0.05, don't reject null hypothesis of homoscedascity

# test for heteroscedascity for local data
hetero_test2 <- filter(field_survey, field_survey$nationality == "Singaporean" |
                           field_survey$nationality == "Singapore Permanent Resident")
hetero_test2 <- mutate(hetero_test2, residual_squared = regression_report$residuals**2)
hetero_test <- lm(residual_squared ~ monthlyincome + visitationrate + traveltime + age, hetero_test2)
summary(hetero_test)
# 0.9745 > 0.05, don't reject null hypothesis of homoscedascity

# dont run this, you cant ln0. I'm still trying to think of a solution
# field_survey_loglevel <- mutate(field_survey, max_wtp = ln(max_wtp))
# regression_report_loglevel <- lm(formula = max_wtp ~ monthlyincome + visitationrate + traveltime + age, field_survey_loglevel)
# summary(regression_report_loglevel)
# hetero_test_loglevel <- mutate(regression_report_loglevel, residual_squared = regression_report_loglevel$residuals**2)
# hetero_test_loglevel <- lm(residual_squared ~ monthlyincome + visitationrate + traveltime + age, hetero_test_loglevel)
# summary(hetero_test_loglevel)

# calculating TEV
locals_data <- filter(field_survey, field_survey$nationality == "Singaporean" |
                          field_survey$nationality == "Singapore Permanent Resident")
mean_existence <- mean(locals_data[which(locals_data$visitationrate=="existence"),"max_wtp"])
mean_option <- mean(locals_data[which(locals_data$visitationrate=="option"),"max_wtp"])
mean_use <- mean(locals_data[which(locals_data$visitationrate=="use"),"max_wtp"])
mean_wtp <- mean(locals_data$max_wtp)
existence_proportion <- nrow(locals_data[which(locals_data$visitationrate=="existence"),])/nrow(locals_data)
option_proportion <- nrow(locals_data[which(locals_data$visitationrate=="option"),])/nrow(locals_data)
use_proportion <- nrow(locals_data[which(locals_data$visitationrate=="use"),])/nrow(locals_data)
print(mean_existence)
print(mean_option)
print(mean_use)
print(mean_wtp)
GDP_2019 <- 102636.5*1000000
population <- 5685.8*1000
TEV_existence <- mean_existence*(population*existence_proportion)
TEV_option <- mean_option*(population*option_proportion)
TEV_use <- mean_use*(population*use_proportion)
TEV_total <- TEV_existence + TEV_option + TEV_use
TEV_existence_GDP <- TEV_existence/GDP_2019
TEV_option_GDP <- TEV_option/GDP_2019
TEV_use_GDP <- TEV_use/GDP_2019
TEV_mean_GDP <- mean_wtp/GDP_2019
existence <- c(existence_proportion, mean_existence, TEV_existence, TEV_existence_GDP)
option <- c(option_proportion, mean_option, TEV_option, TEV_option_GDP)
use <- c(use_proportion, mean_use, TEV_use, TEV_use_GDP)
total <- c(1, mean_wtp, TEV_total, TEV_mean_GDP)
TEV <- rbind(existence, option, use, total)
colnames(TEV) <- c("proportion", "mean", "sum", "percentageGDP2019")
