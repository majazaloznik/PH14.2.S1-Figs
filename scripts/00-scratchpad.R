###############################################################################
## 0. preliminaries ###########################################################
## 1. plot figure 1 ###########################################################
## 2. latex table 1 ###########################################################
## 3. plot figure 2 ###########################################################
###############################################################################


## 0. preliminaries ###########################################################
###############################################################################
library(tidyverse)
library(xtable)

# save default plot parz
.par <- par()

# import data (copied manually from .docx file into .csv)
df <- read.csv("data/stats.csv")
 
# import data (rearranged  manually from .xlsx file into .csv)
df2 <- read.csv("data/prevalence.csv")

## 1. plot figure 1 ###########################################################
###############################################################################
df[c(1,16:17)] %>% 
  filter(!country %in% c("Arab Countries", "World")) %>% 
  arrange(PO60.2030) -> df.sub

layout(1)
par(mar = c(2.5,3,1,5)+0.2)

# actual plotting
x<- barplot(t(as.matrix(df.sub[2:3])),
            beside = TRUE,
            col = c("black", "white"),
            space = c(0.2,1),
            legend.text = c("2015", "2030"),
            args.legend = list(bty = "n", 
                               x = 14, 
                               y = 20,
                               y.intersp = 1.5),
            axes = FALSE)
axis(2, at =seq(0,18,2), las = 2)
for (i in seq(0, 18, 2)){
  lines(c(0, max(x)+2), c(i,i), lty = 3, col = "gray80")
}
lines(c(0, max(x)+2), c(df[df$country == "World", 16],df[df$country == "World", 16]), lty = 1)
lines(c(0, max(x)+2), c(df[df$country == "World", 17],df[df$country == "World", 17]), lty = 2)

lines(c(0, max(x)+2), c(df[df$country == "Arab Countries", 16],df[df$country == "Arab Countries", 16]), lty = 1)
lines(c(0, max(x)+2), c(df[df$country == "Arab Countries", 17],df[df$country == "Arab Countries", 17]), lty = 2)
text(max(x)+3, df[df$country == "Arab Countries", 16], "A", xpd = TRUE)
text(max(x)+3, df[df$country == "Arab Countries", 17], "B", xpd = TRUE)
text(max(x)+3, df[df$country == "World", 16], "C", xpd = TRUE)
text(max(x)+3, df[df$country == "World", 17], "D", xpd = TRUE)
text(max(x)+10, (df[df$country == "World", 17]+
                   df[df$country == "World", 16])/2, 
     "X", xpd = TRUE)
text(max(x)+10, (df[df$country == "Arab Countries", 17]+df[df$country == "Arab Countries", 16])/2, "y", xpd = TRUE )

arrows(max(x)+4.5, df[df$country == "World", 16]+.7,
       max(x)+4.5, df[df$country == "World", 17]-.7, xpd = TRUE,
       length = 0.1)
arrows(max(x)+4.5, df[df$country == "Arab Countries", 16]+.7,
       max(x)+4.5, df[df$country == "Arab Countries", 17]-.7, xpd = TRUE,
       length = 0.1)

barplot(t(as.matrix(df.sub[2:3])),
        beside = TRUE, 
        add = TRUE,
        col = c( "black", "white"),
        space = c(0.2,1),
        axes = FALSE,
        legend.text = c("2015", "2030"),
        args.legend = list(bty = "n", 
                           x = 14, 
                           y = 20,
                           y.intersp = 1.5))
axis(2, at =seq(0,18,2), las = 2)
text(x = colMeans(x)-.2, y = -.75, letters[1:22], xpd = TRUE, srt = 45, cex = .8)

dev.copy2eps(file = "figures/fig1.eps", width = 10, height = 5)

## 2. latex table 1 ###########################################################
###############################################################################
x <- xtable(df)
print(x, include.rownames = FALSE)


## 3. plot figure 2 ###########################################################
###############################################################################



FunBarplot <- function(cnd = "Hypertension", top = 1) {
  df2 %>% filter(condition == cnd) %>% 
    select(3:4) %>% 
    arrange(male) %>% 
    as.matrix() %>% 
    t() -> bp.ready
  df2 %>% filter(condition == cnd) %>% 
    pull(1) -> bp.names
  
  x <- barplot(bp.ready,
               ylim = c(0,80),
               beside = TRUE,
               axes = FALSE,
               col = c("black", "white"),
               space = c(0.3,1),
               main = paste0(top,"x"))
  axis(2, las = 2, at = seq(0,80,20), labels = paste0(seq(0,80,20),"x"))
  for (i in seq(0,80,20)) {
    lines(c(0, max(x)+2), c(i,i), lty = 2, col = "gray80")
  }
  
  barplot(bp.ready,
          ylim = c(0,80),
          beside = TRUE,
          axes = FALSE,
          col = c("black", "white"),
          space = c(0.3,1), 
          add = TRUE)
  z <- 7*(top-1)+1
  text(x = colMeans(x), y = -4.75, z:(z+length(bp.names)-1), xpd = TRUE,  cex = .8)
  
}

par(mar = c(3,3,3,0))
layout(matrix(1:4, nrow = 2, byrow = TRUE))
FunBarplot(cnd = "Hypertension")
FunBarplot(cnd = "Diabetes", 2)
FunBarplot(cnd = "Obesity", 3)
legend(0, 105, legend = c("m", "w"),           
       fill = c("black", "white"),
       y.intersp = 1.5,
       xpd = TRUE,
       bty = "n")
FunBarplot(cnd = "Smoking", 4)

dev.copy2eps(file = "figures/fig2.eps", width = 10, height = 6)


