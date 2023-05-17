library(tidyverse)
library(readxl)

rep1 <- read_xlsx("results_rep1.xlsx")
rep2 <- read_xlsx("results_rep2.xlsx")
rep3 <- read_xlsx("results_rep3.xlsx")

all <- rbind(rep1, rep2, rep3)

aggregated <- all %>% 
  group_by(Tool, FDR) %>% 
  summarise(
    True = mean(`True Crosslinks`),
    False = mean(`False Crosslinks`),
    meanTrue = mean(`True Crosslinks`),
    meanFalse = mean(`False Crosslinks`),
    sdTrue = sd(`True Crosslinks`),
    sdFalse = sd(`False Crosslinks`)
  )

pivoted <- pivot_longer(aggregated, -c("Tool", "FDR", "meanTrue", "meanFalse","sdTrue", "sdFalse"), names_to = "mean", values_to = "mean_value")

df <- pivoted %>% 
  mutate(sd = ifelse(mean == "True", sdTrue, sdFalse)) %>% 
  mutate(ymin = ifelse(mean == "True", meanTrue, meanTrue + meanFalse) - sd) %>% 
  mutate(ymax = ifelse(mean == "True", meanTrue, meanTrue + meanFalse) + sd)

ggplot(df, aes(x=Tool, y=mean_value, fill=mean)) +
  geom_bar(stat="identity", color="black", width = 0.7) +
  geom_errorbar(aes(x=Tool, ymin=ymin, ymax=ymax), width = 0.4, colour = "black", stat = "identity", size = 0.7) +
  ggtitle("Dataset of synthetic Peptides by Matzinger et al., 2022:\nNumber of identified Crosslinks per Tool and FDR (3 Replicates, Crosslinker: DSSO)\n") +
  facet_grid(~ `FDR`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks (mean)") +
  ylim(c(0, 500)) +
  labs(fill="Crosslinks") +
  geom_text(aes(label=round(mean_value, 0)), size=5.0, position = position_stack(vjust = 0.5)) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

w = 0.7
ggplot(df, aes(x=Tool, y=mean_value, fill=mean)) +
  geom_bar(stat="identity", color="black", width = 0.7, position = position_dodge(width = w)) +
  geom_errorbar(aes(ymin=mean_value-sd, ymax=mean_value+sd), width = 0.4, colour = "black", stat = "identity", size = 0.7, position = position_dodge(width = w)) +
  ggtitle("Dataset of synthetic Peptides by Matzinger et al., 2022:\nNumber of identified Crosslinks per Tool and FDR (3 Replicates, Crosslinker: DSSO)\n") +
  facet_grid(~ `FDR`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks (mean)") +
  ylim(c(0, 500)) +
  labs(fill="Crosslinks") +
  geom_text(aes(x = Tool, y = round(mean_value/2, 0), label=round(mean_value, 0)), size=5.0, position = position_dodge(width = w)) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

selected <- aggregated %>% 
  filter(Tool %in% c("MaXLinker wC", "MS2 Annika dWF", "MS3 Annika dWF", "XlinkX d5-10-10")) %>%
  filter(FDR == "Estimated 1%") %>% 
  mutate(Tool = case_when(Tool == "MaXLinker wC" ~ "MaXLinker",
                          Tool == "MS2 Annika dWF" ~ "MS Annika 1.0",
                          Tool == "MS3 Annika dWF" ~ "MS Annika 2.0",
                          Tool == "XlinkX d5-10-10" ~ "XlinkX",
                          TRUE ~ Tool))

pivoted <- pivot_longer(selected, -c("Tool", "FDR", "meanTrue", "meanFalse","sdTrue", "sdFalse"), names_to = "mean", values_to = "mean_value")

df <- pivoted %>% 
  mutate(sd = ifelse(mean == "True", sdTrue, sdFalse)) %>% 
  mutate(ymin = ifelse(mean == "True", meanTrue, meanTrue + meanFalse) - sd) %>% 
  mutate(ymax = ifelse(mean == "True", meanTrue, meanTrue + meanFalse) + sd) %>% 
  mutate(position = ifelse(mean == "True", round(meanTrue / 2), round(meanTrue + meanFalse) + 25)) %>% 
  mutate(mean = ifelse(mean == "True", "True Crosslinks", "False Crosslinks")) %>% 
  mutate(coloring = paste(Tool, mean, sep = " ")) %>% 
  mutate(coloring_fct = factor(coloring, levels = c(
    "MaXLinker True Crosslinks", "MS Annika 1.0 True Crosslinks", "MS Annika 2.0 True Crosslinks", "XlinkX True Crosslinks",
    "MaXLinker False Crosslinks", "MS Annika 1.0 False Crosslinks", "MS Annika 2.0 False Crosslinks", "XlinkX False Crosslinks"
  ))) %>% 
  mutate(true_fdr = meanFalse / (meanFalse + meanTrue))

ggplot(df, aes(x=Tool, y=mean_value, fill=coloring_fct)) +
  geom_bar(stat="identity", color="black", width = 0.7, position = position_stack(reverse = T)) +
  geom_errorbar(aes(x=Tool, ymin=ymin, ymax=ymax), width = 0.3, colour = rep(c("black", "red2"), 4), stat = "identity", size = 0.8) +
  #ggtitle("Dataset of synthetic peptides by Matzinger et al., 2022:\nNumber of identified crosslinks per tool at 1% FDR (3 replicates, crosslinker: DSSO)") +
  scale_fill_manual(values = alpha(c("#F72585", "#7209B7", "#4361EE", "#4CC9F0",
                                     "#FB9DC7", "#B450F7", "#B4C1F8", "#C6EEFA"), 0.6),
                    labels = c(rep("T", 4), rep("F", 4))) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks (mean) at 1% estimated FDR") +
  ylim(c(0, 525)) +
  labs(fill="Crosslinks\n(True/False)") +
  geom_text(aes(y=position, label=round(mean_value, 0)), size=5.0) +
  geom_text(aes(y = 525, label = paste0(round(true_fdr * 100, 2), "%")), size=5.0, angle = 0, hjust = 0.5) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  guides(fill = guide_legend(ncol = 2, label.position = "right"))
