library(tidyverse)
library(readxl)
library(ggfittext)

data <- read_xlsx("results3.xlsx")

data["Tool"][data["Tool"] == "MaxLinker"] <- "MaXLinker"

df <- pivot_longer(data[,c("Tool", "Data Source", "True Crosslinks", "False Crosslinks", "True FDR")], 
                   -c("Tool", "Data Source", "True FDR"), names_to = "variable", values_to = "value") %>% 
        mutate(variable_coloring = paste(Tool, variable, sep = " ")) %>% 
        mutate(variable_coloring_fct = factor(variable_coloring, levels = c(
          "MaXLinker True Crosslinks", "MS2 Annika True Crosslinks", "MS3 Annika True Crosslinks", "XlinkX True Crosslinks",
          "MaXLinker False Crosslinks", "MS2 Annika False Crosslinks", "MS3 Annika False Crosslinks", "XlinkX False Crosslinks"
        )))

val = c("", "5", "", "6", "", "3", "", "6", "", "", "", "7", "", "10", "", "8")

ggplot(df, aes(x=Tool, y=value, fill=variable_coloring_fct)) +
  geom_bar(stat="identity", color="black", width = 0.7, position = position_stack(reverse = T)) +
  #ggtitle("Dataset of synthetic peptides by Beveridge et al., 2020:\nNumber of identified crosslinks per tool and method (1% FDR, crosslinker: DSSO)") +
  scale_fill_manual(values = alpha(c("#F72585", "#7209B7", "#4361EE", "#4CC9F0",
                                     "#FB9DC7", "#B450F7", "#B4C1F8", "#C6EEFA"), 0.6),
                    labels = c(rep("T", 4), rep("F", 4))) +
  facet_grid(~ `Data Source`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks") +
  labs(fill="Crosslinks\n(True/False)") +
  ylim(0, 250) +
  geom_bar_text(aes(label = value),
                color = "#000001",
                place = "centre",
                size = 5 * ggplot2::.pt,
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(5, "pt"),
                padding.y = grid::unit(5, "pt"),
                position = position_stack(reverse = T),
                outside = TRUE) +
  geom_bar_text(aes(label = val),
                color = "#000001",
                size = 5 * ggplot2::.pt,
                min.size = 5 * ggplot2::.pt,
                padding.x = grid::unit(5, "pt"),
                padding.y = grid::unit(5, "pt"),
                position = position_stack(reverse = T),
                outside = TRUE) +
  #geom_text(aes(label=value), size=5.0, position = position_stack(vjust = 0.5, reverse = T)) +
  geom_text(aes(y = 235, label = paste0(round(`True FDR`, 2), "%")), size=5.0, angle = 90, hjust = 0) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  guides(fill = guide_legend(ncol = 2, label.position = "right"))
