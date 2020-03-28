## COVID-19 GIRONA ##
# setwd('Desktop/COVID19-Girona')

## read data
girona <- read.csv2('Girona.csv', header = T)
girona <- girona[rowSums(is.na(girona)) != ncol(girona)-1, ]
poblacio.RS.Girona <- 861.753

## libraries
library(ggplot2); library(reshape2)

## calculations
girona$dia <- factor(girona$dia, levels = as.character(girona$dia))
girona$infectats.ratio07 <- (girona$morts.totals*100)/0.7
girona$infectats.ratio1 <- (girona$morts.totals*100)/1
girona$infectats.ratio15 <- (girona$morts.totals*100)/1.5
girona$infectats.ratio2 <- (girona$morts.totals*100)/2
girona$infectats.ratio3 <- (girona$morts.totals*100)/3
girona$infectats.ratio6 <- (girona$morts.totals*100)/6
girona$morts.altes <- girona$morts.dia/girona$altes.dia
names(girona)

## variacio nous infectats/dia (%)
for (i in 2:nrow(girona)){
  girona$increment[i] <- (girona[i,2]/girona[i-1,2])*100
}

# -------------------------------------------------------------------------


# nous sanitaris/dia
for (i in 2:nrow(girona)){
  girona$sanitaris.dia[i] <- girona[i, 4] - girona[i-1, 4]
}
# nous no sanitaris/dia
girona$nonosanitaris.dia <- girona$nous - girona$sanitaris.dia
# totals no sanitaris
girona$nosanitaris.totals <- girona$totals - girona$sanitaris.totals

## variacio diaria greus (greus totals dia anterior + nous greus - nous morts = noves altes)
girona$greus.evol <- c()
for (i in 2:nrow(girona)){
  girona$greus.evol[i] <- girona[i-1, 5] + girona[i, 6] - girona[i, 8] - girona[i, 10]
}

## plots
girona$dia <- factor(girona$dia, levels = as.character(girona$dia))
names(girona)

# total
girona.total <- girona[, names(girona) %in% c('dia', 'totals', 'sanitaris.totals', 'nosanitaris.totals', 'greus.totals', 'morts.totals', 'altes.totals')]
girona.total.m <- melt(girona.total)
ggplot(girona.total.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'blue', 'orange', 'darkred', 'darkolivegreen3', 'purple')) +
  # geom_col(aes(x = girona$dia, y = girona$nous)) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (totals)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.total.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'blue', 'orange', 'darkred', 'darkolivegreen3', 'purple')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (totals) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")


# diari
girona.diari <- girona[, names(girona) %in% c('dia', 'nous', 'sanitaris.dia', 'nosanitaris.dia', 'greus.dia', 'morts.dia', 'altes.dia')]
girona.diari.m <- melt(girona.diari)
ggplot(girona.diari.m, aes(dia, value, fill = variable, group = variable)) + geom_col (position = 'dodge') +
  scale_fill_manual(labels=c('Nous diagnostics', 'Greus', 'Morts', 'Altes', 'Sanitaris'),
                    values = c('red', 'darkorange', 'black', 'darkolivegreen3', 'cyan3')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (diaris)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.diari.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  # scale_color_manual() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (diaris)",  x = NULL, y = "log10(#) totals", colour = "Variable")

# previsions
girona.prev <- girona[, names(girona) %in% c('dia', 'totals', 'morts.totals', 'infectats.ratio07', 'infectats.ratio1', 'infectats.ratio15', 
                                             'infectats.ratio2', 'infectats.ratio3', 'infectats.ratio6')]
girona.prev.m <- melt(girona.prev)
ggplot(girona.prev.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Diagnosticats', 'Morts', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('purple', 'black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (predicció segons letalitat)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(girona.prev.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Diagnosticats', 'Morts', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('purple', 'black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (predicció segons letalitat) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")


# ratios
girona.ratios <- girona[, names(girona) %in% c('dia', 'increment.dia', 'sanitaris.percent')]
girona.ratios.m <-  melt(girona.ratios)
ggplot(girona.ratios.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
    legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió Girona (Percentatges)",  x = NULL, y = "# totals", colour = "Variable")
       
       