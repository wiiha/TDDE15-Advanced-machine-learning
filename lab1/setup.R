setwd("~/Projects/TDDE15-Advanced-machine-learning/lab1")

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")
BiocManager::install("gRain")

install.packages('bnlearn')
