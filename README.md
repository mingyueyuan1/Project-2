# Project-2
#this it Limiting factors for microbial growth in the subarctic
#lalalla


setwd("/Users/mingyueyuan/Document/project 2/2020 september")
getwd()

library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(egg)
library(gridExtra)
library(plotrix)
library(gggap)
library(gg.gap)
library(agricolae)
display.brewer.all(type = "all")

cue <- read_excel("sep20201.xlsx", sheet = "CUE",
                 range = "A5:J68")
cue<-na.omit(cue)
cue$treatment<-factor(cue$treatment,levels = c("CO","L","N","NL","XL","XN"))
cue$addition<-factor(cue$addition,levels = c("co","C","N","P","CN","CP","NP","CNP"))

#two-way ANOVA#
cueaov<-melt(cue,var.ids=c("treatment", "addition"),
            measure.vars=c(3:8), value.name="cue")

cue.aov<-aov(cue~treatment*addition,data = cueaov)
summary(cue.aov)

#LSD test
cuelsd1<-LSD.test(cue.aov, "treatment")
cuelsd1

cuelsd2<-LSD.test(cue.aov, "addition")
cuelsd2
