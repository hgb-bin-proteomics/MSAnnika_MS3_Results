library(tidyverse)
library(readxl)

data <- read_xlsx("results_summary.xlsx")

df <- pivot_longer(data[,c("Tool", "Dataset", "True Crosslinks", "False Crosslinks", "True FDR")], 
                   -c("Tool", "Dataset", "True FDR"), names_to = "variable", values_to = "value") %>% 
      mutate(variable_coloring = paste(Tool, variable, sep = " "))


ggplot(df, aes(x=Tool, y=value, fill=variable_coloring)) +
  geom_bar(stat="identity", color="black", width = 0.7) +
  ggtitle("Dataset of synthetic peptides by Matzinger et al., 2022:\nNumber of identified crosslinks per tool, crosslinker and mass analyzer (1% FDR)") +
  scale_fill_manual(values = alpha(c("#FB9DC7", "#F72585", 
                                     "#B450F7", "#7209B7", 
                                     "#B4C1F8", "#4361EE", 
                                     "#C6EEFA", "#4CC9F0"), 1)) +
  facet_grid(~ `Dataset`) +
  xlab("Tool") +
  ylab("Number of identified Crosslinks") +
  ylim(c(0, 505)) +
  labs(fill="Crosslinks") +
  geom_text(aes(label=value), size=5.0, position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 450, label = paste0(round(`True FDR`, 2), "%")), size=5.0, angle = 90, hjust = 0) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 
