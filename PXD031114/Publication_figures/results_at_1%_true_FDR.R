library(tidyverse)
library(readxl)

data <- read_xlsx("results_at_1%_true_FDR.xlsx")

df <- pivot_longer(data[,c("Tool", "Replicate", "Sample", "True Crosslinks 1%")], 
                   -c("Tool", "Replicate", "Sample"), names_to = "variable", values_to = "value") %>%
        mutate(Replicate = paste0("Replicate ", Replicate)) %>% 
        mutate(Sample = paste0("Sample ", Sample)) %>% 
        mutate(variable_coloring = ifelse(variable == "True Crosslinks 1%", Tool, variable)) %>% 
        mutate(variable_coloring_fct = factor(variable_coloring, levels = c(
          "MaXLinker", "MS Annika 1.0", "MS Annika 2.0", "XlinkX",
          "True Crosslinks > 1%", "False Crosslinks"
        )))

ggplot(df, aes(x=Tool, y=value, fill=variable_coloring_fct)) +
  geom_bar(stat="identity", color="black", width = 0.7, position = position_stack(reverse = T)) +
  #ggtitle("HEK steppedHCD MS2 + CID MS3 dataset by Ruwolt et al., 2022:\nNumber of identified crosslinks at 1% true FDR per tool (crosslinker: DSSO)") +
  scale_fill_manual(values = alpha(c("#F72585", "#7209B7", "#4361EE", "#4CC9F0",
                                     "#8D99AE", "#EF233C"), 0.6),
                    labels = c(rep("HEK (1% true FDR)", 4), "HEK (> 1% true FDR)", "Not HEK")) +
  ylim(c(0, 325)) +
  facet_grid(vars(Sample),vars(Replicate)) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks at 1% entrapment FDR") +
  labs(fill="Crosslinks") +
  geom_text(aes(label=ifelse(value == 0, "", value)), size=5.0, position = position_stack(vjust = 0.5, reverse = T)) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5), legend.position = "none") +
  guides(fill = guide_legend(ncol = 1, label.position = "right"))
