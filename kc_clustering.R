library(stringdist)
library(maptree)

#load the data
data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/student_data.csv")
student_data <- as.data.frame(data)
rm(data)

#create distance matrix for KC
uniquekcs <- unique(as.character(student_data$KC))
distancemodels <- stringdistmatrix(uniquekcs,uniquekcs,method = "jw")
rownames(distancemodels) <- uniquekcs

#train the model
set.seed(1)
hc <- hclust(as.dist(distancemodels), method = "ward.D2")
rect.hclust(hc,k=5)

#print the number of members in each cluster 
summary(rect.hclust(hc,k=5))

#add the clusters to data
clusterCut <- cutree(hc,5)
x<-cbind(student_data$KC,clusterCut)

for(i in 1:length(x[,1]))
{
  if(x[i,2]==1)
  {
    student_data[i,"KC_C1"]  <- 1
    student_data[i,"KC_C2"]  <- 0
    student_data[i,"KC_C3"]  <- 0
    student_data[i,"KC_C4"]  <- 0
    student_data[i,"KC_C5"]  <- 0
  }
  else if(x[i,2]==2)
  {
    student_data[i,"KC_C1"]  <- 0
    student_data[i,"KC_C2"]  <- 1
    student_data[i,"KC_C3"]  <- 0
    student_data[i,"KC_C4"]  <- 0
    student_data[i,"KC_C5"]  <- 0
  }
  else if(x[i,2]==3)
  {
    student_data[i,"KC_C1"]  <- 0
    student_data[i,"KC_C2"]  <- 0
    student_data[i,"KC_C3"]  <- 1
    student_data[i,"KC_C4"]  <- 0
    student_data[i,"KC_C5"]  <- 0
  }
  else if(x[i,2]==4)
  {
    student_data[i,"KC_C1"]  <- 0
    student_data[i,"KC_C2"]  <- 0
    student_data[i,"KC_C3"]  <- 0
    student_data[i,"KC_C4"]  <- 1
    student_data[i,"KC_C5"]  <- 0
  }
  else if(x[i,2]==5)
  {
    student_data[i,"KC_C1"]  <- 0
    student_data[i,"KC_C2"]  <- 0
    student_data[i,"KC_C3"]  <- 0
    student_data[i,"KC_C4"]  <- 0
    student_data[i,"KC_C5"]  <- 1
  }
  
}
student_data <- student_data[-c(1:19)]
write.csv(student_data[1:5], file="d:/studentDataClustered.csv")
