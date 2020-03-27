## COVID-19 GIRONA ##
setwd('Desktop/COVID19-Girona')

## read data
girona <- read.csv2('Girona.csv', header = T)
girona <- girona[rowSums(is.na(girona)) != ncol(girona)-1, ]


## libraries
library(ggplot2); library(reshape2)

## plots
girona$dia <- factor(girona$dia, levels = as.character(girona$dia))
names(girona)

# total
girona.total <- girona[, names(girona) %in% c('dia', 'totals', 'sanitaris.totals', 'nosanitaris.totals', 'greus.totals', 'morts.totals', 'altes.totals')]
girona.total.m <- melt(girona.total)
ggplot(girona.total.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'blue', 'orange', 'darkorange', 'darkred', 'green')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (totals)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.total.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'blue', 'orange', 'darkorange', 'darkred', 'green')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (totals) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")


# diari
girona.diari <- girona[, names(girona) %in% c('dia', 'nous', 'sanitaris.dia', 'nosanitaris.dia', 'greus.dia', 'morts.dia', 'altes.dia')]
girona.diari.m <- melt(girona.diari)
ggplot(girona.diari.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  # scale_color_manual() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (diaris)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.diari.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  # scale_color_manual() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (diaris)",  x = NULL, y = "log10(#) totals", colour = "Variable")

# previsions
girona.prev <- girona[, names(girona) %in% c('dia', 'nous', 'infectats.ratio07', 'infectats.ratio1', 'infectats.ratio15', 
                                             'infectats.ratio2', 'infectats.ratio3', 'infectats.ratio6')]
girona.prev.m <- melt(girona.prev)
ggplot(girona.prev.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Publicats', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (prediccio segons letalitat)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.prev.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Publicats', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (prediccio segons letalitat) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")


# ratios
girona.ratios <- girona[, names(girona) %in% c('dia', 'increment.dia', 'sanitaris.percent')]
girona.ratios.m <-  melt(girona.ratios)
ggplot(girona.ratios.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
    legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA)) +labs(title = "Evolucio COVID19 - Regio Girona (Percentatges)",  x = NULL, y = "# totals", colour = "Variable")
       
       