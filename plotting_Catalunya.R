## COVID-19 Catalunya ##

## read data
catalunya <- read.csv2('cat.csv', header = T)

## libraries
library(ggplot2); library(reshape2)

## functions
multiplot<-function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## calculations
catalunya$dia <- factor(catalunya$dia, levels = as.character(catalunya$dia))
catalunya$infectats.ratio07 <- (catalunya$morts.total*100)/0.7
catalunya$infectats.ratio1 <- (catalunya$morts.total*100)/1
catalunya$infectats.ratio15 <- (catalunya$morts.total*100)/1.5
catalunya$infectats.ratio2 <- (catalunya$morts.total*100)/2
catalunya$infectats.ratio3 <- (catalunya$morts.total*100)/3
catalunya$infectats.ratio6 <- (catalunya$morts.total*100)/6
catalunya$morts.altes <- catalunya$morts.dia/catalunya$altes.dia
names(catalunya)

## variacio nous infectats/dia (%)
increment <- c()
for (i in 2:nrow(catalunya)){
  increment[i] <- (catalunya[i,3]/catalunya[i-1,3])*100
}
catalunya$increment.dia <- increment

## variacio diaria greus (greus totals dia anterior + nous greus - nous morts = noves altes)
catalunya$greus.evol <- c()
for (i in 2:nrow(catalunya)){
  catalunya$greus.evol[i] <- catalunya[i-1, 5] + catalunya[i, 6] - catalunya[i, 8] - catalunya[i, 10]
}
  
## plots
# total
catalunya.total <- catalunya[, names(catalunya) %in% c('dia', 'totals', 'greus.total', 'morts.total', 'altes.total')]
catalunya.total.m <- melt(catalunya.total)
c.a <- ggplot(catalunya.total.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'darkorange', 'darkred', 'green')) +
  # geom_col(aes(x = catalunya$dia, y = catalunya$nous)) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Catalunya (totals)",  x = NULL, y = "# totals", colour = "Variable") +
  geom_vline(xintercept = 11, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 7.5, y= 1800, label = c('Confinament \n Conca Òdena'), cex = 3) + 
  geom_segment(aes(x=9.7,xend=10.8,y=1500,yend=1300), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20')  +
  geom_vline(xintercept = 14, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 9.8, y= 2700, label = c('Confinament parcial'), cex = 3) + 
  geom_segment(aes(x=12.7,xend=13.8,y=2500,yend=2300), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20') +
  geom_vline(xintercept = 27, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 22.3, y= 15700, label = c('Confinament total \n Protocol: només PCR'), cex = 3) + 
  geom_segment(aes(x=25.5,xend=26.6,y=15400,yend=15200), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20')  
  
ggplot(catalunya.total.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(values=c('red', 'darkorange', 'darkred', 'green')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Catalunya (totals) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")


# diari
catalunya.diari <- catalunya[, names(catalunya) %in% c('dia', 'nous', 'greus.dia', 'morts.dia', 'altes.dia')]#, 'greus.evol')]
catalunya.diari.m <- melt(catalunya.diari)
c.b <- ggplot(catalunya.diari.m, aes(dia, value, fill = variable, group = variable)) + geom_col(position = 'dodge') +
  scale_fill_manual(labels=c('Nous diagnostics', 'Greus', 'Morts', 'Altes'),#, 'Greus evol.'),
                     values = c('red', 'darkorange', 'black', 'darkolivegreen2')) +#, 'blue')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Catalunya (diaris)",  x = NULL, y = "# totals", colour = "Variable") + 
  geom_vline(xintercept = 11, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 7.7, y= 400, label = c('Confinament \n Conca Òdena'), cex = 3) + 
  geom_segment(aes(x=9.7,xend=10.8,y=350,yend=300), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20')  +
  geom_vline(xintercept = 14, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 10, y= 600, label = c('Confinament parcial'), cex = 3) + 
  geom_segment(aes(x=12.7,xend=13.8,y=550,yend=500), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20') +
  geom_vline(xintercept = 27, lty = 'dashed', col = 'gray75') +
  annotate('text', x= 22.5, y= 2200, label = c('Confinament total \n Protocol: només PCR'), cex = 3) + 
  geom_segment(aes(x=25.5,xend=26.6,y=2100,yend=2050), arrow=arrow(length=unit(0.2,"cm")), col = 'gray20')  


ggplot(catalunya.diari.m, aes(dia, log10(value), color = variable, group = variable)) + geom_point() + geom_line() +
  # scale_color_manual() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Ctalunya (diaris)",  x = NULL, y = "log10(#) totals", colour = "Variable")

# previsions
catalunya.prev <- catalunya[, names(catalunya) %in% c('dia', 'totals', 'morts.total', 'infectats.ratio07', 'infectats.ratio1', 'infectats.ratio15', 
                                             'infectats.ratio2', 'infectats.ratio3', 'infectats.ratio6')]
catalunya.prev.m <- melt(catalunya.prev)
c.c <- ggplot(catalunya.prev.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Diagnosticats', 'Morts', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('purple', 'black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Catalunya (predicció segons letalitat)",  x = NULL, y = "# totals", colour = "Variable")

ggplot(catalunya.prev.m, aes(dia, log(value), color = variable, group = variable)) + geom_point() + geom_line() +
  scale_color_manual(labels = c('Diagnosticats', 'Morts', 'Letalitat 0.7%', 'Letalitat 1%', 'Letalitat 1.5%', 'Letalitat 2%', 'Letalitat 3%', 'Letalitat 6%'),
                     values = c('orange', 'black', 'darkred', '#CC0000', '#FF0000', '#FF3333', '#FF6666', '#FF9999')) +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Catalunya (predicció segons letalitat) - logaritmic",  x = NULL, y = "log10(#) totals", colour = "Variable")

# ratios
catalunya.ratios <- catalunya[, names(catalunya) %in% c('dia', 'increment.dia')]
catalunya.ratios.m <-  melt(catalunya.ratios)
ggplot(catalunya.ratios.m, aes(dia, value, color = variable, group = variable)) + geom_point() + geom_line() +
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.line = element_line(size = 0.4, linetype = "solid"), 
        panel.grid.major = element_line(colour = "gray95", linetype = "dashed"), axis.text = element_text(face = "bold"), 
        axis.text.x = element_text(vjust = 0.5, angle = 90), legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), panel.background = element_rect(fill = NA), legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(title = "Evolució COVID19 - Regió catalunya (Percentatges)",  x = NULL, y = "# totals", colour = "Variable")

## global plot
jpeg('catalunya.jpg', width = 3500, height = 1000, res = 150, units = 'px')       
multiplot(c.a, c.b, c.c, cols = 3)
dev.off()

