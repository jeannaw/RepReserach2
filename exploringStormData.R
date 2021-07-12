
## To denote the start time
print(Sys.time())

## 1. Data Processing

## 1.1 Loading packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(cowplot)

## 1.2 Loading data
filename <- "data/repdata_data_StormData.csv.bz2"
if (!file.exists(filename)) {
    file_url <-
        "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(file_url, filename, method = "curl")
}

## 1.3 Reading storm data set and processing the data
df.storm <- read.csv(filename)
cat("In the data file, there are",
    nrow(df.storm),
    "records on",
    ncol(df.storm),
    "variables",
    "\n"
)
## Convert date character to date type
df.storm$BGN_DATE <- as.Date(df.storm$BGN_DATE, format = "%m/%d/%Y")
df.storm$END_DATE <- as.Date(df.storm$END_DATE, format = "%m/%d/%Y")
## Add Year column
df.storm$BGN_YEAR <- as.character(df.storm$BGN_DATE, format = "%Y")


## 1.4 Aggregate fatalities, injuries, and property damage by event type and beginning year
df.byevtypeyear <-
    df.storm %>% group_by(EVTYPE, BGN_YEAR, PROPDMGEXP, CROPDMGEXP) %>%
    summarize(
        sumFatalities = sum(FATALITIES),
        sumInjuries = sum(INJURIES),
        sumPropdmg = sum(PROPDMG),
        sumCropdmg = sum(CROPDMG)
    )
cat("To reduce data, roughly group data  by EVTYPE, BGN_YEAR, PROPDMGEXP, CROPDMGEXP.",
    "And the number of reduced data rows",
    nrow(df.byevtypeyear),
    "\n"
)

checkDataType <- unique(df.byevtypeyear[, "EVTYPE"])
cat ("The official events type are 48.",
    "However, 'EVTYPE' column has", nrow(checkDataType), "events",
    "\n"
)

## 1.5 Cleaning Event Type
## Convert EVTYPE to only the first letter to Uppercase and remove whitespace
dfNew <- df.byevtypeyear
dfNew$EVTYPE <- str_trim(str_to_title(dfNew$EVTYPE))
## convert backslash \ to forward slash / 
dfNew$EVTYPE <- gsub("\\\\", "/", dfNew$EVTYPE )

## Loading Typo Mapping data manually created for correcting typos
refTypo <- read.csv("data/ref_Evtype_Corrections.csv", header = TRUE)
for (i in 1:nrow(refTypo)) {
    dfNew$EVTYPE <-
        sub(refTypo$EVTXT[i],
            refTypo$EVSUBSTR[i],
            dfNew$EVTYPE)
}

## Loading Event Type Mapping data manually created,
## which are used for simplifying remove operation and grepl operation
refEvType <- read.csv("data/ref_Evtype_Mapping.csv", header = TRUE)
## Subseting event type for remove operation and processing data
refEvTypeRemove <- refEvType [refEvType$OPER=="remove", ]
for (i in 1:nrow(refEvTypeRemove)) {
    patx <- refEvTypeRemove$SEARCHEVTYPE [i]
    searchLoc <- grepl(patx, dfNew$EVTYPE)
    dfNew <- dfNew[!(searchLoc), ]
}
## Fetching and sorting Event Type for grepl operation by priority
refEvTypeGrepl <- refEvType [refEvType$OPER=="grepl", ]
refEvTypeGrepl <- arrange(refEvTypeGrepl, PRIO, STORMEVENT)
## updating storm event with official one.
dfNew["StormEvent"] <- NA
for (i in 1:nrow(refEvTypeGrepl)) {
    patx <- refEvTypeGrepl$SEARCHEVTYPE [i]
    searchLoc <- grepl(patx, dfNew$EVTYPE)
    dfNew$StormEvent[is.na(dfNew$StormEvent) & searchLoc==TRUE] <- refEvTypeGrepl$STORMEVENT[i]
}
## Remove rows with NA StormEvent
dfNew <- dfNew[!(is.na(dfNew$StormEvent)), ]

cat(
    "Removed",
    nrow(df.byevtypeyear) - nrow(dfNew),
    "uncount records, and left data has",
    nrow(dfNew),
    "records.",
    "\n"
)

checkDataType <- unique(dfNew[ , c("StormEvent")]) 
cat ("Currently, the storm events type are", 
     nrow(checkDataType), 
     "\n"
)



## 1.6 Convert Propdmg and Cropdmg to million
dfNew$PROPDMGEXP <- toupper(dfNew$PROPDMGEXP)
dfNew$CROPDMGEXP <- toupper(dfNew$CROPDMGEXP)
# H,h = hundreds = 100
# K,k = kilos = thousands = 1,000
# M,m = millions = 1,000,000
# B,b = billions = 1,000,000,000
# (+) = 1
# (-) = 0
# (?) = 0
# black/empty character = 0
# numeric 0..8 = 10
iNum <- 0:8
dfNew <- mutate(
    dfNew,
    PROPDMG_M = case_when(
        PROPDMGEXP == "B" ~  sumPropdmg * 1000,
        PROPDMGEXP == "M" ~  sumPropdmg,
        PROPDMGEXP == "K" ~  sumPropdmg / 1000,
        PROPDMGEXP == "H" ~  sumPropdmg / 10000,
        PROPDMGEXP %in% iNum ~ sumPropdmg / 100000,
        PROPDMGEXP == "+" ~ sumPropdmg / 1000000,
        TRUE ~ 0
    )
)
dfNew <- mutate(
    dfNew,
    CROPDMG_M = case_when(
        CROPDMGEXP == "B" ~  sumCropdmg * 1000,
        CROPDMGEXP == "M" ~  sumCropdmg,
        CROPDMGEXP == "K" ~  sumCropdmg / 1000,
        CROPDMGEXP == "H" ~  sumCropdmg / 10000,
        CROPDMGEXP %in% iNum ~ sumCropdmg / 100000,
        CROPDMGEXP == "+" ~ sumCropdmg / 1000000,
        TRUE ~ 0
    )
)

## 1.7 Calculate data
byEvtype <- dfNew %>%
    group_by(StormEvent) %>%
    summarise(
        DMG = round(sum(PROPDMG_M) + sum(CROPDMG_M)),
        FATALITIES = sum(sumFatalities),
        INJURIES = sum(sumInjuries)
    )
byEvtypeYear <- dfNew %>%
    group_by(StormEvent, BGN_YEAR) %>%
    summarise(
        DMG = round(sum(PROPDMG_M) + sum(CROPDMG_M)),
        FATALITIES = sum(sumFatalities),
        INJURIES = sum(sumInjuries)
    )
# Pivot data from wide to long
byEvTypeEcon <- top_n(byEvtype, 10, DMG)
byEvTypeHlth <- top_n(byEvtype, 10, (FATALITIES + INJURIES))
byEvtypeHlthPiv <-
    subset(byEvTypeHlth, select = -DMG) %>% pivot_longer(-StormEvent,
                                                         names_to = "TYPE",
                                                         values_to = "CONSEQVALUE")
# byEvtypePiv2 <-
#     byEvtype %>% pivot_longer(
#         cols = c(FATALITIES, INJURIES),
#         names_to = "CONSEQTYPE",
#         values_to = "CONSEQVALUE"
#     )

## 2. Results

## Plot the top 10 harmful events to population health
g <-
    ggplot(byEvtypeHlthPiv, aes(StormEvent, CONSEQVALUE, fill = TYPE)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom" ) +
    ggtitle("The Top 10 Events Harmful to Population Health") +
    labs(x = "Storm Data Event", y = "Impacted Number of Individuals")
print(g)


## Histogram the top 10 harmful events to economic consequences
g <- ggplot(data = byEvTypeEcon, aes(x = StormEvent, y = DMG, fill = DMG)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle =  70, vjust = 0.5),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low="pink",high="navy") +
    ggtitle("The Top 10 Events to Economic Damage") +
    labs(x = "Storm Data Event", y = "Damage (Million)")
print(g)



## Check Storm Event Tornado/Flood/Heat/Hurricane (Typhoon) impacts
byEvtypeYearTop <-
    subset(
        byEvtypeYear,
        StormEvent == "Tornado" |
            StormEvent == "Flood" |
            StormEvent == "Hurricane (Typhoon)" |
            StormEvent == "Heat"
    )

plot.yHlth <-
    ggplot(byEvtypeYearTop,
           aes(x = BGN_YEAR,
               y = FATALITIES + INJURIES,
               group = StormEvent
           )) +
    geom_line(aes(color = StormEvent), size = 1.2)+
    geom_point(aes(color = StormEvent), size = 2)+
    scale_x_discrete(breaks = seq(1950, 2015, by = 5)) +
    theme(
        axis.text.x = element_text(angle = 70, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    labs(x = "Year", y = "Fatalities & Injuries")

plot.yEcon <-
    ggplot(data = byEvtypeYearTop,
           aes(x = BGN_YEAR, y = DMG, fill = StormEvent)) +
    # geom_point(size = 3, alpha = 0.5) +
    geom_bar(stat = "identity") +
    theme(
        axis.text.x = element_text(angle = 70, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_discrete(breaks = seq(1950, 2015, by = 5)) +
    labs(x = "Year", y = "Damage (Million)")

gg <-
    ggdraw() + 
    draw_plot(plot.yHlth, 0, 0, 0.5, 1) + 
    draw_plot(plot.yEcon, 0.5, 0, 0.5, 1) 
print(gg)

## To denote the end time
print(Sys.time())



