library(tidyverse)
library(readxl)

data <- read_xlsx("results_rep1.xlsx")

df <- pivot_longer(data[,c("Tool", "FDR", "True Crosslinks", "False Crosslinks", "True FDR")], -c("Tool", "FDR", "True FDR"), names_to = "variable", values_to = "value")

ggplot(df, aes(x=Tool, y=value, fill=variable)) +
  geom_bar(stat="identity", color="black", width = 0.7) +
  ggtitle("Dataset of synthetic Peptides by Matzinger et al., 2022:\nNumber of identified Crosslinks per Tool and FDR (Replicate 1, Crosslinker: DSSO)\n") +
  facet_grid(~ `FDR`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks") +
  ylim(c(0, 600)) +
  labs(fill="Crosslinks") +
  geom_text(aes(label=value), size=5.0, position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 525, label = paste0(round(`True FDR`, 2), "%")), size=5.0, angle = 90, hjust = 0) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

data <- read_xlsx("results_rep2.xlsx")

df <- pivot_longer(data[,c("Tool", "FDR", "True Crosslinks", "False Crosslinks", "True FDR")], -c("Tool", "FDR", "True FDR"), names_to = "variable", values_to = "value")

ggplot(df, aes(x=Tool, y=value, fill=variable)) +
  geom_bar(stat="identity", color="black", width = 0.7) +
  ggtitle("Dataset of synthetic Peptides by Matzinger et al., 2022:\nNumber of identified Crosslinks per Tool and FDR (Replicate 2, Crosslinker: DSSO)\n") +
  facet_grid(~ `FDR`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks") +
  ylim(c(0, 600)) +
  labs(fill="Crosslinks") +
  geom_text(aes(label=value), size=5.0, position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 525, label = paste0(round(`True FDR`, 2), "%")), size=5.0, angle = 90, hjust = 0) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

data <- read_xlsx("results_rep3.xlsx")

df <- pivot_longer(data[,c("Tool", "FDR", "True Crosslinks", "False Crosslinks", "True FDR")], -c("Tool", "FDR", "True FDR"), names_to = "variable", values_to = "value")

ggplot(df, aes(x=Tool, y=value, fill=variable)) +
  geom_bar(stat="identity", color="black", width = 0.7) +
  ggtitle("Dataset of synthetic Peptides by Matzinger et al., 2022:\nNumber of identified Crosslinks per Tool and FDR (Replicate 3, Crosslinker: DSSO)\n") +
  facet_grid(~ `FDR`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks") +
  ylim(c(0, 600)) +
  labs(fill="Crosslinks") +
  geom_text(aes(label=value), size=5.0, position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 525, label = paste0(round(`True FDR`, 2), "%")), size=5.0, angle = 90, hjust = 0) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 
