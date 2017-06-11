install.packages("gplots")
install.packages("heatmap.plus")
install.packages("RColorBrewer")

library("gplots")
library("heatmap.plus")
library("RColorBrewer")



test <- read.csv("~/YourPath/SampleAnnotatedHeatmapDataNorm.csv",row.names = 1)
# control <- test[grep("Control",row.names(test)),]
# treatment_3weeks <- test[grep("Treatment_3weeks",row.names(test)),]
# GSM_data <- test[grep("GSM[1][0-2]",row.names(test)),]


# a function to assign colors based on treatment time 
# http://www.rapidtables.com/web/color/RGB_Color.htm
condition_colors <- unlist(lapply(rownames(test),function(x){
  if(grepl("Treatment",x)) '#FFC0CB' #pink
  else if(grepl('Control',x)) '#808080' #grey
  
}))

# I like to have a line just to assign input in one step
input <- as.matrix(t(test))

heatmap.2(input, trace="none", density="none", col=bluered(20), cexRow=1, cexCol=0.2, margins = c(20,13),
          ColSideColors=condition_colors, scale="row")

treatment_times <- c(0,1,3,8,24)
treatment_color_options <- c(brewer.pal(5, "Set1"))

treatment_colors <- unlist(lapply(rownames(test),function(x){
  for(t_time in 1:length(treatment_times)){
    if(grepl(paste("_",treatment_times[t_time],"weeks",sep=""),x)) return(treatment_color_options[t_time])
  }
}))

myCols <- cbind(condition_colors,treatment_colors)
colnames(myCols)[1] <- "Condition"
colnames(myCols)[2] <- "Treatment Time"

# for exporting pdf or jpeg etc:
pdf(file="~/YourPath/AnnotatedHeatmap.pdf")  
par(cex.main=0.8,mar=c(1,1,1,1))
heatmap.plus(input, col=bluered(20),cexRow=1,cexCol=0.2, margins = c(20,13), main="Your Title",
             ColSideColors=myCols)

legend(0.8,1,legend=paste(treatment_times,"weeks"),fill=treatment_color_options,cex=0.5)
legend(0.8,0.9,legend=c("Control","Treatment"),fill=c('#808080','#FFC0CB'),cex=0.5)
# see ?legend for keywords
# "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
# use RowSideColors for annotating rows 

# for exporting pdf or jpeg etc
dev.off()

heatmap.2(input, trace="none", density="none", col=bluered(20), cexRow=1, cexCol=0.2, margins = c(20,13),
          ColSideColors=condition_colors, scale="row",
          hclust=function(x) hclust(x,method="average"),distfun=function(x) as.dist((1-cor(t(x)))/2))


### MORE GREP EXAMPLES #####
# For odd numbered "Entities"
control <- test[,grepl("[:digit:]*[13579]$",colnames(test))]


