library("neuralnet")

#set working directory
setwd("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III");

#load the data and assemble it to create a new data for neural network
data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/student_data.csv")
student_data <- as.data.frame(data)
rm(data)

kc_data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/kc.csv")
kc <- as.data.frame(kc_data)
rm(kc_data)

ph_data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/problem_hierarchy.csv")
ph <- as.data.frame(ph_data)
rm(ph_data)

problem_name_data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/problem_name.csv")
problem_name <- as.data.frame(problem_name_data)
rm(problem_name_data)

step_name_data <- read.csv("D:/study-material/umb-classes/2-spring-2016/cs638/term-project/phase_III/step_name.csv")
step_name <- as.data.frame(step_name_data)
rm(step_name_data)

problem_view <- student_data[c(5)]
for(i in 1:length(problem_view))
{
  problem_view[i] <- (problem_view[i] - min(problem_view, na.rm=TRUE))/(max(problem_view, na.rm=TRUE) - min(problem_view, na.rm=TRUE))
}

data <- cbind(problem_view, kc[2:6], ph[2:6], problem_name[2:8], step_name[2:9], student_data[c(14)])


#generate validation set
sd1 <- sample(1099, 900)
student_train <- data[sd1,]
student_test <- data[-sd1,]

#train the network
student_net <- neuralnet(correct_first_attempt ~ problem_view + KC_C1 + KC_C2 + KC_C3 + KC_C4 + KC_C5 + PH_C1 + PH_C2 + PH_C3 + PH_C4 + PH_C5 + PN_C1 + PN_C2 + PN_C3 + PN_C4 + PN_C5 + PN_C6 + PN_C7 + SN_C1 + SN_C2 + SN_C3 + SN_C4 + SN_C5 + SN_C6 + SN_C7 + SN_C8, data =  student_train, hidden = 3, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.01, err.fct = "sse", act.fct = "logistic")
summary(student_net)
plot(student_net, rep = "best")

prediction <- compute(student_net,student_test[,1:26])

finaloutput <- cbind(prediction$net.result, student_test$correct_first_attempt)
write.csv(finaloutput, "result.csv")

correct = 0
wrong = 0
for(i in 1:length(prediction$net.result))
{
  if(prediction$net.result[i]>=0.5)
  {
    if(student_test[i,27]==1)
    {
      correct = correct + 1
    }
  }
  else
  {
    if(student_test[i,27]==0)
    {
      correct = correct + 1
    }
  }
}

accuracy <- (correct/length(student_test[,1]))*100
print(paste('Accuracy = ',accuracy,'%'))
print(student_net)
