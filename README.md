# OQ_SEA
R script for Southeast Asian archaeofaunal data. 


################################################################################
# R scripts to create figures on archaeozoological data from mainland Southeast
# Asian prehistoric sites. 
# 
# written by Cyler Conrad, Department of Anthropology, University of New Mexico
# cylerc@unm.edu
#
# Results described in:
# Conrad, C. (in review). Archaeozoology in Mainland Southeast Asia: Changing
# Methodology and Pleistocene to Holocene Forager Subsistence Patterns in 
# Thailand and Peninsular Malaysia. Open Quaternary. 
#
# NOTES
# All data required to perform the analyses can be found at
# http://hdl.handle.net/1928/25699 (Conrad 2015). The script was run using R 
# version 3.1.1 on Mac OS X 10.8.5
#
# Reference: 
# Conrad, C. 2015. Faunal Data for Pleistocene-Holocene Archaeological Sites in 
# Thailand and Peninsular Malaysia [dataset]. University of New Mexico. 
# http://hdl.handle.net/1928/25699
#
# LICENSE
# 
# The MIT License (MIT)
# 
# Copyright (c) 2015 Cyler Conrad
#   
#   Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################



################################################################################
# CHUNK 1: load libraries
################################################################################

library(ggplot2) #Version 1.0.0
library(reshape2)
library(gridExtra)

################################################################################
# CHUNK 1-Figure 2: Decadal division of faunal publications using differential 
# methodological recording techniques during the 1930s–2010s. Download data file 
# "figure2.csv" from Conrad (2015). This file includes counts per decade for 
# each recording technique used in archaeozoological publications. This figure 
# is constructed by reshaping the data into long format, then using 
# facet_wrap() to construct four plots, one per recording technique. 
################################################################################


# Set absolute path to csv file on your computer
fn <- "figure2.csv" 
sea <- read.csv(fn, stringsAsFactors = FALSE, check.names = FALSE)
sea
str(sea)

sea.long <- melt(sea, value.name="Value")
sea.long  
ggplot(sea.long, aes(`Faunal Publications`, Value, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  facet_wrap(~ variable) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, face="bold", 
                                   angle=90),
        axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
        axis.title.y = element_text(vjust=2.0, color="black", size=20, face="bold"),
        axis.title.x = element_text(vjust=0.5, color="black", size=20, face="bold"),
        strip.text.x = element_text(size=30))

################################################################################
# CHUNK 2-Figure 3: Grouped classifications from Thailand sites expressing total NISP 
# counts per site (left column), and total NISP counts for all sites (right column).
# Download data file "figure3.csv" from Conrad (2015). This file 
# includes total NISP counts for all Thai sites in this dataset, grouped by 
# highest taxon specific values. To create this figure the data is reshaped 
# to a long format and wrapped to express NISP counts per taxonomic classification 
# in each site, and total for all sites. The plot gives total counts per site on
# the left half, with total aggregated NISPs appearing on the right half. 
# Colored points denote sites.
################################################################################


fn <- "figure3.csv"
thai <- read.csv(fn, stringsAsFactors = FALSE, skip = 1, check.names=FALSE)
thai
str(thai)

thai$`Taxonomic Classification order` <- thai$`Taxonomic Classification`
thai$`Taxonomic Classification` <- factor(thai$`Taxonomic Classification`, 
                                levels = rev(thai$`Taxonomic Classification order`))

thai.long <- melt(thai
                  , measure.vars = c("NISP", "Tham Lod", "Lang Kamnan", 
                                     "Lang Rongrien", "Khao Toh Chong", 
                                     "Moh Khiew II", "Thung Nong Nien")
                  , variable.name = "Site"
                  , value.name = "NISP"
                  , na.rm = TRUE)
thai.long$Total.ind <- factor(ifelse((thai.long$Site == "NISP"), "Total", "Sites"))

p1 <- ggplot(thai.long, aes(x = NISP, y = `Taxonomic Classification`))
p1 <- p1 + geom_point(aes(colour = Site), alpha = 0.75, size=5)
p1 <- p1 + facet_grid(. ~ Total.ind)
p1 <- p1 + theme_bw() +
  theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, angle=90, 
                                   face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=20, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=20, face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        legend.position = "bottom", 
        legend.text = element_text(size=20, face="bold"), 
        legend.title = element_text(size=20))
p1

################################################################################
# CHUNK 3-Figure 4: Grouped classifications from Thailand sites expressing total NISP 
# counts per site (left column), and total NISP counts for all sites (right column) 
# without Moh Khiew Cave II. Download data file "figure4.csv" from Conrad (2015).  
# This file includes total NISP counts for all Thai sites in this dataset, 
# excluding Moh Khiew Cave II. This csv file is also grouped by highest taxon 
# specific values.To create this figure the data is reshaped like prior. The plot 
# gives total counts per site on the left half, with total aggregated NISPs 
# appearing on the right half. Colored points denote sites.
################################################################################


fn <- "figure4.csv"
thai2 <- read.csv(fn, stringsAsFactors = FALSE, skip = 1, check.names=FALSE)
thai2
str(thai2)

thai2$`Taxonomic Classification order` <- thai2$`Taxonomic Classification`
thai2$`Taxonomic Classification` <- factor(thai2$`Taxonomic Classification`, 
                                levels = rev(thai2$`Taxonomic Classification order`))

thai2.long <- melt(thai2
                  , measure.vars = c("NISP", "Tham Lod", "Lang Kamnan", 
                                     "Lang Rongrien", "Khao Toh Chong", 
                                     "Thung Nong Nien")
                  , variable.name = "Site"
                  , value.name = "NISP"
                  , na.rm = TRUE)
thai2.long$Total.ind <- factor(ifelse((thai2.long$Site == "NISP"), "Total", "Sites"))

p2 <- ggplot(thai2.long, aes(x = NISP, y = `Taxonomic Classification`))
p2 <- p2 + geom_point(aes(colour = Site), alpha = 0.75, size=5)
p2 <- p2 + facet_grid(. ~ Total.ind)
p2 <- p2 + theme_bw() +
  theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, angle=90, 
                                   face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=20, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=20, face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        legend.position = "bottom", 
        legend.text = element_text(size=20, face="bold"), 
        legend.title = element_text(size=20))
p2

################################################################################
# CHUNK 4-Figure 5: Grouped classifications from Thailand sites expressing total NISP 
# counts per site (left column), and total NISP counts for all sites 
# (right column) without Testudines. Download data file "figure5.csv" from 
# Conrad (2015). This file includes total NISP counts for all Thai sites in 
# this dataset, excluding Testudines. This csv file is also grouped by highest 
# taxon specific values. To create this figure the data is reshaped like prior. 
# The plot gives total counts per site on the left half, with total aggregated 
# NISPs appearing on the right half. Colored points denote sites.
################################################################################


fn <- "figure5.csv"
thai3 <- read.csv(fn, stringsAsFactors = FALSE, skip = 1, check.names=FALSE)
thai3
str(thai3)

thai3$`Taxonomic Classification order` <- thai3$`Taxonomic Classification`
thai3$`Taxonomic Classification` <- factor(thai3$`Taxonomic Classification`, 
                                levels = rev(thai3$`Taxonomic Classification order`))

thai3.long <- melt(thai3
                  , measure.vars = c("NISP", "Tham Lod", "Lang Kamnan", 
                                     "Lang Rongrien", "Khao Toh Chong", 
                                     "Moh Khiew II", "Thung Nong Nien")
                  , variable.name = "Site"
                  , value.name = "NISP"
                  , na.rm = TRUE)
thai3.long$Total.ind <- factor(ifelse((thai3.long$Site == "NISP"), "Total", "Sites"))

p3 <- ggplot(thai3.long, aes(x = NISP, y = `Taxonomic Classification`))
p3 <- p3 + geom_point(aes(colour = Site), alpha = 0.75, size=5)
p3 <- p3 + facet_grid(. ~ Total.ind)
p3 <- p3 + theme_bw() +
  theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, angle=90, 
                                   face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=20, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=20, face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        legend.position = "bottom", 
        legend.text = element_text(size=20, face="bold"), 
        legend.title = element_text(size=20))
p3

################################################################################
# CHUNK 5-Figure 6: Grouped classifications from Peninsular Malaysian sites expressing 
# total NISP counts per site (left column), and total NISP counts for all sites 
# (right column). Download data file "figure6.csv" from Conrad (2015). This file 
# includes total NISP counts for all Peninsula Malaysia sites in this dataset.
# This csv file is also grouped by highest taxon specific values. 
# To create this figure the data is reshaped like prior. The plot gives total 
# counts per site on the left half, with total aggregated NISPs appearing 
# on the right half. Colored points denote sites.
################################################################################


fn <- "figure6.csv"
ma <- read.csv(fn, stringsAsFactors = FALSE, skip = 1, check.names=FALSE)
ma
str(ma)

ma$`Taxonomic Classification order` <- ma$`Taxonomic Classification`
ma$`Taxonomic Classification` <- factor(ma$`Taxonomic Classification`, 
                                 levels = rev(ma$`Taxonomic Classification order`))

ma.long <- melt(ma
                 , measure.vars = c("Total NISP", "Gua Gunung Runtuh", 
                                    "Gua Peraling", "Gua Kechil", "Gua Tenggek", 
                                    "Gua Sagu")
                 , variable.name = "Site"
                 , value.name = "NISP"
                 , na.rm = TRUE)
ma.long$Total.ind <- factor(ifelse((ma.long$Site == "Total NISP"), "Total", "Sites"))

p4 <- ggplot(ma.long, aes(x = NISP, y = `Taxonomic Classification`))
p4 <- p4 + geom_point(aes(colour = Site), alpha = 0.75, size=5)
p4 <- p4 + facet_grid(. ~ Total.ind)
p4 <- p4 + theme_bw() +
  theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, angle=90, 
                                   face="bold"),
        axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
        axis.title.y = element_text(vjust=1.0, color="black", size=20, face="bold"),
        axis.title.x = element_text(vjust=0.1, color="black", size=20, face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        legend.position = "bottom", 
        legend.text = element_text(size=20, face="bold"), 
        legend.title = element_text(size=20))
p4


################################################################################
# CHUNK 6-Figure 7: Squared chord distance values for Thailand and Peninsular 
# Malaysian sites. S=Surface context. This group of data is arranged in its final 
# form to include eight different plots into one. For these plots, download all 
# site specific csv files "figure7..." in Conrad (2015), then use the function 
# list.files () to upload into R. These files include squared chord distance 
# values, quantified between contexts, for each site. The code is constructed to 
# input the values, plot each site and loop all sites together into one plot. 
################################################################################


fig7_file_names <- list.files(pattern = "figure7") 
fig7_data <- lapply(fig7_file_names, read.csv)
pattern_to_remove <- c("figure7_", ".csv")
names(fig7_data) <- gsub(paste0(pattern_to_remove, collapse = "|"), "", fig7_file_names)
my_plots <- vector("list", length = length(fig7_data))
for(i in seq_along(fig7_data)){
  p <- ggplot(fig7_data[[i]], aes(Context, SCD, group = 1)) +
    geom_point(position = "identity", na.rm=FALSE, size=4) +
    geom_line() +
    labs(title = names(fig7_data)[i]) + xlab("") +ylab("") + ylim(0,2) +
    coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(vjust=0.5, color="black", size=20, face="bold"),
          axis.text.y = element_text(vjust=0.5, color="black", size=20, face="bold"), 
          axis.title.y = element_text(vjust=2.0, color="black", size=20, face="bold"),
          axis.title.x = element_text(vjust=0.5, color="black", size=20, face="bold"),
          strip.text.x = element_text(size=30),
          plot.title = element_text(vjust=0.5, color="black", size=20, face="bold"))
  my_plots[[i]] <- p
}

do.call("grid.arrange", c(my_plots, ncol=4))

