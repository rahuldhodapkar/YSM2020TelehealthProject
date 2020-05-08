#!/usr/bin/env Rscript
#
# Build intuitive visualizations from tables for the telehealth project
#
# @author Rahul Dhodapkar <rahul.dhodapkar@yale.edu>
# @version 2020.05.08
#

library(hashmap)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
attach(mtcars)

FIG_HEIGHT_IN_PER_ORD <- 0.6
FIG_WIDTH_IN <- 12

# read data
clean_df <- function(df) {
  ci.mat <- str_match(df$X95..CI, "([\\d\\.]+), ([\\d\\.]+)")
  df$lo.95.ci <- as.numeric(ci.mat[,2])
  df$hi.95.ci <- as.numeric(ci.mat[,3])
  
  df$Var <- gsub('(.{1,45})(\\s|$)', '\\1\n', df$Var)
  gsub('\n$', '', df$Var)
  
  return(df)
}

care.df <- read.csv('./data/health_care.csv', as.is = T) %>% clean_df
seeking.df <- read.csv('./data/health_information_seeking.csv', as.is = T) %>% clean_df
engagement.df <- read.csv('./data/health_engagement.csv', as.is = T) %>% clean_df

#####################################
# plot Health Care
#####################################
df <- care.df
dem2demgroup <- hashmap(df$Dem,df$DemGroup)
x.ord <- unique(df$Dem)
df$x.lab <- paste(dem2demgroup[[x.ord]], "|", x.ord)

df$log.or <- log2(df$OR)
df$log.lo.ci <- log2(df$lo.95.ci)
df$log.hi.ci <- log2(df$hi.95.ci)

p <- ggplot(df, aes(x=x.lab, y=log.or, group=Var, fill=Var)) +
  geom_bar(stat="identity", position="dodge", alpha=0.4) +
  geom_errorbar( aes(x=x.lab, ymin=log.lo.ci, ymax=log.hi.ci, color=Var), 
                 width=0.4, alpha = 0.9, position=position_dodge(0.9)) +
  ylab(bquote(log2(OR))) + xlab('') + geom_hline(yintercept=0) +
  ggtitle('Health Care') +
  coord_flip()
p
ggsave('./figs/health_care.png', 
       height = FIG_HEIGHT_IN_PER_ORD*length(x.ord), width = FIG_WIDTH_IN)

#####################################
# plot Health Information Seeking
#####################################
df <- seeking.df
dem2demgroup <- hashmap(df$Dem,df$DemGroup)
x.ord <- unique(df$Dem)
df$x.lab <- paste(dem2demgroup[[x.ord]], "|", x.ord)

df$log.or <- log2(df$OR)
df$log.lo.ci <- log2(df$lo.95.ci)
df$log.hi.ci <- log2(df$hi.95.ci)

p <- ggplot(df, aes(x=x.lab, y=log.or, group=Var, fill=Var)) +
  geom_bar(stat="identity", position="dodge", alpha=0.4) +
  geom_errorbar( aes(x=x.lab, ymin=log.lo.ci, ymax=log.hi.ci, color=Var), 
                 width=0.4, alpha = 0.9, position=position_dodge(0.9)) +
  ylab(bquote(log2(OR))) + xlab('') + geom_hline(yintercept=0) +
  ggtitle('Health Information Seeking') +
  coord_flip()
p
ggsave('./figs/health_information_seeking.png', 
       height = FIG_HEIGHT_IN_PER_ORD*length(x.ord), width = FIG_WIDTH_IN)

#####################################
# plot Health Engagement
#####################################
df <- engagement.df
dem2demgroup <- hashmap(df$Dem,df$DemGroup)
x.ord <- unique(df$Dem)
df$x.lab <- paste(dem2demgroup[[x.ord]], "|", x.ord)

df$log.or <- log2(df$OR)
df$log.lo.ci <- log2(df$lo.95.ci)
df$log.hi.ci <- log2(df$hi.95.ci)

p <- ggplot(df, aes(x=x.lab, y=log.or, group=Var, fill=Var)) +
  geom_bar(stat="identity", position="dodge", alpha=0.4) +
  geom_errorbar( aes(x=x.lab, ymin=log.lo.ci, ymax=log.hi.ci, color=Var), 
                 width=0.4, alpha = 0.9, position=position_dodge(0.9)) +
  ylab(bquote(log2(OR))) + xlab('') + geom_hline(yintercept=0) +
  ggtitle('Health Engagement') +
  coord_flip()
p
ggsave('./figs/health_engagement.png', 
       height = FIG_HEIGHT_IN_PER_ORD*length(x.ord), width = FIG_WIDTH_IN)