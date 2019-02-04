


#----- Chord diagram -----#

# From: https://www.r-graph-gallery.com/chord-diagram/
# From: https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2/

# Key points:
# Maps *two* sets of factors to one another 

name=c(3,10,10,3,6,7,8,3,6,1,2,2,6,10,2,3,3,10,4,5,9,10)
feature=paste("feature ", c(1,1,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5) , sep="")
dat <- data.frame(name,feature)
dat <- with(dat, table(name, feature))

# Charge the circlize library
library(circlize)

# Make the circular plot
set.seed(1234)
chordDiagram(as.data.frame(dat), transparency = 0.5)


# Example uses:





#---- Another chord diagram -----#
# From: https://www.r-graph-gallery.com/122-a-circular-plot-with-the-circlize-package/

### You need several libraries
library(circlize)
library(migest)
library(dplyr)

### Make data
m <- data.frame(order = 1:6,
                country = c("Ausralia", "India", "China", "Japan", "Thailand", "Malaysia"),
                V3 = c(1, 150000, 90000, 180000, 15000, 10000),
                V4 = c(35000, 1, 10000, 12000, 25000, 8000),
                V5 = c(10000, 7000, 1, 40000, 5000, 4000),
                V6 = c(7000, 8000, 175000, 1, 11000, 18000),
                V7 = c(70000, 30000, 22000, 120000, 1, 40000),
                V8 = c(60000, 90000, 110000, 14000, 30000, 1),
                r = c(255,255,255,153,51,51),
                g = c(51, 153, 255, 255, 255, 255),
                b = c(51, 51, 51, 51, 51, 153),
                stringsAsFactors = FALSE)
df1 <- m[, c(1,2, 9:11)]
m <- m[,-(1:2)]/1e04
m <- as.matrix(m[,c(1:6)])
dimnames(m) <- list(orig = df1$country, dest = df1$country)
#Sort order of data.frame and matrix for plotting in circos
df1 <- arrange(df1, order)
df1$country <- factor(df1$country, levels = df1$country)
m <- m[levels(df1$country),levels(df1$country)]


### Define ranges of circos sectors and their colors (both of the sectors and the links)
df1$xmin <- 0
df1$xmax <- rowSums(m) + colSums(m)
n <- nrow(df1)
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)

### Plot sectors (outer part)
par(mar=rep(0,4))
circos.clear()

### Basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 90, gap.degree =4)

### Sector details
circos.initialize(factors = df1$country, xlim = cbind(df1$xmin, df1$xmax))

### Plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$country, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa = c(0, 0.5)
                         
                         #plot country labels
                         circos.text(x=mean(xlim), y=1.7, labels=name, facing = dd, cex=0.6,  adj = aa)
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(from=0,to=floor(df1$xmax)[i],by=5), 
                                     minor.ticks=1, labels.away.percentage = 0.15)
                       })

### Plot links (inner part)
### Add sum values to df1, marking the x-position of the first links
### out (sum1) and in (sum2). Updated for further links in loop below.
df1$sum1 <- colSums(m)
df1$sum2 <- numeric(n)

### Create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
df2 <- cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
               timevar="dest", time=rownames(m),  v.names = "m")
df2 <- arrange(df2,desc(m))

### Keep only the largest flows to avoid clutter
df2 <- subset(df2, m > quantile(m,0.6))

### Plot links
for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$country)
  j<-match(df2$dest[k],df1$country)
  
  #plot link
  circos.link(sector.index1=df1$country[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$country[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[i])
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}











#----- Venn Diagram -----#
# From: https://stackoverflow.com/questions/46629193/create-5-way-venn-diagram-from-csv-file-in-r
library(eulerr)

df = data.frame(A = sample(c(0, 1), 100, replace = T),
                B = sample(c(0, 1), 100, replace = T),
                C = sample(c(0, 1), 100, replace = T),
                D = sample(c(0, 1), 100, replace = T))


sp_euler = with(df,
                euler(c("A" = sum(A),
                        "B" = sum(B),
                        "C" = sum(C),
                        "D" = sum(D),
                        "A&B" = sum(A == 1 & B == 1),
                        "A&C" = sum(A == 1 & C == 1),
                        "A&D" = sum(A == 1 & D == 1),
                        "B&C" = sum(B == 1 & C == 1),
                        "B&D" = sum(B == 1 & D == 1),
                        "C&D" = sum(C == 1 & D == 1),
                        "A&B&C" = sum(A == 1 & B == 1 & C == 1),
                        "A&B&D" = sum(A == 1 & B == 1 & D == 1),
                        "A&C&D" = sum(A == 1 & C == 1 & D == 1),
                        "B&C&D" = sum(B == 1 & C == 1 & D == 1),
                        "A&B&C&D" = sum(A == 1 & B == 1 & C == 1 & D == 1)), input = "union"))

plot(sp_euler,  counts = T, fontface = 1)



#----- Data Scientist Venn Diagram -----#
# Another great example 
# Theory: https://rstudio-pubs-static.s3.amazonaws.com/13301_6641d73cfac741a59c0a851feb99e98b.html
# Practice: https://datascience.stackexchange.com/questions/2403/data-science-without-knowledge-of-a-specific-topic-is-it-worth-pursuing-as-a-ca

draw.ellipse <- function(center,angle,semimajor,semiminor,radius,h,s,v,...) {
  shape <- rbind(c(cos(angle),-sin(angle)),c(sin(angle),cos(angle))) %*% diag(c(semimajor,semiminor))
  tt <- seq(0,2*pi,length.out=1000)
  foo <- matrix(center,nrow=2,ncol=length(tt),byrow=FALSE) + shape%*%(radius*rbind(cos(tt),sin(tt)))
  polygon(foo[1,],foo[2,],col=hsv(h,s,v,alpha=0.5),border="black",...)
}
name <- function(x,y,label,cex=1.2,...) text(x,y,label,cex=cex,...)

# png("Venn.png",width=600,height=600)
par(mai=c(0,0,0,0),lwd=1,font=3.5)
plot(c(0,100),c(0,90),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="") %>% par(opar)
draw.ellipse(center=c(30,30),angle=0.75*pi,semimajor=2,semiminor=1,radius=20,h=60/360,s=.068,v=.976)
draw.ellipse(center=c(70,30),angle=0.25*pi,semimajor=2,semiminor=1,radius=20,h=83/360,s=.482,v=.894)
draw.ellipse(center=c(48,40),angle=0.7*pi,semimajor=2,semiminor=1,radius=20,h=174/360,s=.397,v=.8)
draw.ellipse(center=c(52,40),angle=0.3*pi,semimajor=2,semiminor=1,radius=20,h=200/360,s=.774,v=.745)

name(50,90,"The Data Scientist Venn Diagram",pos=1,cex=2)
name(8,62,"Communi-\ncation",cex=1.5,pos=3)
name(30,78,"Statistics",cex=1.5)
name(70,78,"Programming",cex=1.5)
name(92,62,"Business",cex=1.5,pos=3)

name(10,45,"Hot\nAir")
name(90,45,"The\nAccountant")
name(33,65,"The\nData\nNerd")
name(67,65,"The\nHacker")
name(27,50,"The\nStats\nProf")
name(73,50,"The\nIT\nGuy")
name(50,55,"R\nCore\nTeam")
name(38,38,"The\nGood\nConsultant")
name(62,38,"Drew\nConway's\nData\nScientist")
name(50,24,"The\nperfect\nData\nScientist!")
name(31,18,"Comp\nSci\nProf")
name(69,18,"The\nNumber\nCruncher")
name(42,11,"Head\nof IT")
name(58,11,"Ana-\nlyst")
name(50,5,"The\nSalesperson")









#----- Correlation matrix / heatmap -----#
# From: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]

res <- cor(my_data)
round(res, 2)



library(corrplot)
corrplot(res, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)

























