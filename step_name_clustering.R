library(stringdist)
library(maptree)


#load the data
data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/student_data.csv")
student_data <- as.data.frame(data)
rm(data)

#create distance matrix for problem hierarchy
uniquesns <- unique(as.character(student_data$step_name))
distancemodels <- stringdistmatrix(uniquesns,uniquesns,method = "osa")
rownames(distancemodels) <- uniquesns

#train the model
set.seed(1)
hc_step_name <- hclust(as.dist(distancemodels), method = "ward.D")
plot.new()
plot(hc_step_name)
rect.hclust(hc_step_name,k=8)

#print the number of members in each cluster 
summary(rect.hclust(hc_step_name,k=8))

#add the clusters to data
clusterCut <- cutree(hc_step_name,8)
x<-cbind(student_data$step_name,clusterCut)

for(i in 1:length(x[,1]))
{
  if(x[i,2]==1)
  {
    student_data[i,"SN_C1"]  <- 1
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==2)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 1
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==3)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 1
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==4)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 1
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==5)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 1
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==6)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 1
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==7)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 1
    student_data[i,"SN_C8"]  <- 0
  }
  else if(x[i,2]==8)
  {
    student_data[i,"SN_C1"]  <- 0
    student_data[i,"SN_C2"]  <- 0
    student_data[i,"SN_C3"]  <- 0
    student_data[i,"SN_C4"]  <- 0
    student_data[i,"SN_C5"]  <- 0
    student_data[i,"SN_C6"]  <- 0
    student_data[i,"SN_C7"]  <- 0
    student_data[i,"SN_C8"]  <- 1
  }
}
student_data <- student_data[-c(1:19)]
write.csv(student_data[1:8], file="d:/step_name.csv")
