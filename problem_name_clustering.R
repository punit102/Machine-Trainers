library(stringdist)
library(maptree)


#load the data
data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/student_data.csv")
student_data <- as.data.frame(data)
rm(data)

#create distance matrix for problem hierarchy
uniquepns <- unique(as.character(student_data$problem_name))
distancemodels <- stringdistmatrix(uniquepns,uniquepns,method = "jw")
rownames(distancemodels) <- uniquepns

#train the model
set.seed(1)
hc_problem_name <- hclust(as.dist(distancemodels), method = "ward.D")
plot.new()
plot(hc_problem_name)
rect.hclust(hc_problem_name,k=7)

#print the number of members in each cluster 
summary(rect.hclust(hc_problem_name,k=7))

#add the clusters to data
clusterCut <- cutree(hc_problem_name,7)
x<-cbind(student_data$problem_name,clusterCut)

for(i in 1:length(x[,1]))
{
  if(x[i,2]==1)
  {
    student_data[i,"PN_C1"]  <- 1
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==2)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 1
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==3)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 1
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==4)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 1
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==5)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 1
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==6)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 1
    student_data[i,"PN_C7"]  <- 0
  }
  else if(x[i,2]==7)
  {
    student_data[i,"PN_C1"]  <- 0
    student_data[i,"PN_C2"]  <- 0
    student_data[i,"PN_C3"]  <- 0
    student_data[i,"PN_C4"]  <- 0
    student_data[i,"PN_C5"]  <- 0
    student_data[i,"PN_C6"]  <- 0
    student_data[i,"PN_C7"]  <- 1
  }
  
}
student_data <- student_data[-c(1:19)]
write.csv(student_data[1:7], file="d:/problem_name.csv")
