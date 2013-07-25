mydf <- read.csv("./data/sample.csv", header=T)

# 각 Group, trt에 대한 평균, 중위수, Q1, Q3 계산
summary.tab <- aggregate(mydf["Value"], mydf[c("Group","Trt")], 
                         summary)
summary.tab <- cbind(summary.tab[,1:2], as.data.frame(summary.tab$Value))

IQR.tab <- aggregate(mydf["Value"], mydf[c("Group","Trt")], 
                     IQR)

summary.tab <- merge(summary.tab, IQR.tab, by=c("Group", "Trt"))
names(summary.tab) <- c("Group", "Trt", "Min", "Q1", "Median",
                        "Mean", "Q3", "Max", "IQR")

summary.tab$IQR15 <- summary.tab$IQR*1.5
summary.tab$IQR30 <- summary.tab$IQR*3.0

# Lower Inner Fence
summary.tab$Lower.IF <- with(summary.tab, Q1 - IQR15)
# Lower Outer Fence
summary.tab$Lower.OF <- with(summary.tab, Q1 - IQR30)
# Upper Inner Fence
summary.tab$Upper.IF <- with(summary.tab, Q3 + IQR15)
# Upper Outter Fence
summary.tab$Upper.OF <- with(summary.tab, Q3 + IQR30)

# Boxplot 그리기
n.obs <- aggregate(mydf["Value"], mydf[c("Trt","Group")], length)
xlabel <- paste("N=", n.obs$Value, sep = "")
at <- c(1:3, 5:7)
labels <- c("trt1","trt2","trt3")
boxplot(Value~Trt, data=mydf, subset=Group=="A", axes=F, outline=F, 
        xlim=c(0,8), at=c(1:3), ylim=c(0, max(mydf$Value)))
boxplot(Value~Trt, data=mydf, subset=Group=="B", axes=F, outline=F, 
        xlim=c(5,7), add=T, at=c(5:7))
axis(2)
axis(1, at=at, labels=xlabel,las=1, cex.axis=0.8 )
mtext(c("Group A","Group B"), side=1, line=3, at=c(2, 6))
mtext(labels, side=3,at=at, cex=0.9)
points(at, summary.tab$Mean, cex=1.2, pch=19)                                                                                                
box()

# Inner Fence와 Outter Fence의 이상값 그리기
nn <- 1 
for (i in c("A","B")) {
  for (j in c("trt1","trt2","trt3")){    
    sub.df <- subset(mydf, Group == i & Trt == j)
    Outer.Fence <- sub.df$Value > summary.tab$Upper.OF[nn] | 
      sub.df$Value < summary.tab$Lower.OF[nn]
    Inner.Fence <- (sub.df$Value <= summary.tab$Upper.OF[nn] &
                sub.df$Value > summary.tab$Upper.IF[nn]) |
      (sub.df$Value >= summary.tab$Lower.OF[nn] &
         sub.df$Value < summary.tab$Lower.IF[nn]) 
    indicator <- ifelse(Inner.Fence , TRUE, FALSE)
    if(sum(indicator) > 0){
      points(rep(at[nn], sum(indicator)), sub.df$Value[which(indicator)], 
             pch=3)      
    }
    indicator <- ifelse(Outer.Fence , TRUE, FALSE)
    if(sum(indicator) > 0){
      points(rep(at[nn], sum(indicator)), sub.df$Value[which(indicator)], 
             pch=8)      
    }
    
    nn <- nn + 1
  }
}


