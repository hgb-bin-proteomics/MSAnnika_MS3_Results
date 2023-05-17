library(tidyverse)

SCE = c(245, 254, 304, 268, 321, 270)
MS3 = c(244, 237, 303, 264, 244, 300)

values <- c(SCE, MS3)
Method <- c(rep("SCE", 6), rep("SCE+MS3", 6))
groups <- c("A1", "A2", "B1", "B2", "C1", "C2", "A1", "A2", "B1", "B2", "C1", "C2")

data <- data.frame(values, Method, groups)

ggplot(data, aes(fill = Method, y = values, x = groups)) + 
  geom_bar(position="dodge", color = "black", stat="identity") +
  scale_fill_manual(values = alpha(c("#7209B7", "#4361EE"), 0.6),
                    labels = c("SCE", "SCE+MS3")) +
  xlab("Raw File") +
  ylab("Nr. of crosslinks at 1% estimated FDR") +
  ggtitle("Stepped HCD MS2 vs. stepped HCD MS2 + CID MS3") +
  theme_minimal(base_size = 18)
