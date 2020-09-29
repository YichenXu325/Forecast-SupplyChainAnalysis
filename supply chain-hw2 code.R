# Read data
# The data is transferred from the origin 6*11 data into a one-list-table csv document
datadirectory <- "~/Desktop/supply chain analysis/hw2"
origin_data <- read.csv(file.path(datadirectory,"hw2 origin_data.csv"),header = FALSE)
names(origin_data) <- "D_t"
origin_data[,2] <- c(rep(c(1,2,3,4,5,6),11),1,2,3,4)
names(origin_data)[2] <- "Season"
origin_data[,3:6] <- 0
names(origin_data)[3:6] <- c("CMA_j","S*_j","S_j","D*_t")
# De-seasonalize
Sj <- c(0,0,0,0,0,0)
nSj <- c(0,0,0,0,0,0)
for(i in 4:67){
  origin_data[i,3] <- (origin_data[i-2,1]+origin_data[i-1,1]+origin_data[i,1]+origin_data[i+1,1]+origin_data[i+2,1]+0.5*origin_data[i-3,1]+0.5*origin_data[i+3,1])/6
  origin_data[i,4] <- origin_data[i,1]/origin_data[i,3]
  Sj[origin_data[i,2]] <- Sj[origin_data[i,2]] + origin_data[i,4]
  nSj[origin_data[i,2]] <- nSj[origin_data[i,2]] + 1
}
for(i in 1:6) {
  Sj[i] <- Sj[i] / nSj[i]
}
sumSj <- sum(Sj)
for(i in 1:6) {
  Sj[i] <- Sj[i] * 6 / sumSj
}
for(i in 1:70) {
  origin_data[i,5] <- Sj[origin_data[i,2]]
  origin_data[i,6] <- origin_data[i,1] / origin_data[i,5]
}
# Build the result table
# a_d <- c(0.05,0.1,0.15,0.2,0.25,0.3) for both single and double exponential
# b_d <- c(0.01,0.04,0.07,0.1,0.13,0.16,0.2)
TestResult <- data.frame(approach_type = c(rep("Single Exp",6),rep("Double Exp",42)))
TestResult[,2:17] <- 0
names(TestResult)[2:17] <- c("alpha value",
                             "beta value",
                             "2020-S5",
                             "2020-S6",
                             "2021-S1",
                             "2021-S2",
                             "2021-S3",
                             "2021-S4",
                             "2021-S5",
                             "2021-S6",
                             "ME",
                             "MAE",
                             "MSE",
                             "MAPE",
                             "mean of E_t",
                             "mean of MAD_t")
# Single Exponential Smoothing Approach
s_exp <- data.frame(D_t=origin_data[,1],S_j=origin_data[,5])
s_exp[71,2] <- Sj[5]
s_exp[72,2] <- Sj[6]
for(i in 1:6) {
  s_exp[72+i,2] <- Sj[i]
}
s_exp[1:70,3] <- origin_data[,6]
s_exp[,4:9] <- 0
names(s_exp)[3:9] <- c("D*_t","F*_t,t+1","F_t,t+1","e_t","|e|_t","E_t","MAD_t")
a_s <- c(0.1,0.15,0.2,0.25,0.3,0.35)
# Initialize: the F*_t,t+1 value for No.29 come from D*_29
s_exp[29,4] <- s_exp[29,3]
for(a in 1:length(a_s)) {
  for(i in 30:70){
    s_exp[i,4] <- a_s[a] * s_exp[i,3] + (1 - a_s[a]) * s_exp[i-1,4]
  }
  s_exp[71:77,4] <- s_exp[70,4]
  for(i in 30:77) {
    s_exp[i,5] <- s_exp[i,4] * s_exp[i+1,2]
  }
  for(i in 31:70) {
    s_exp[i,6] <- s_exp[i,1] - s_exp[i-1,5]
    s_exp[i,7] <- abs(s_exp[i,6])
  }
  s_exp[37,8] <- mean(s_exp[31:36,6])
  s_exp[37,9] <- mean(s_exp[31:36,7])
  for(i in 38:70) {
    s_exp[i,8] <- 0.1 * s_exp[i,6] + 0.9 * s_exp[i-1,8]
    s_exp[i,9] <- 0.1 * s_exp[i,7] + 0.9 * s_exp[i-1,9]
  }
  # Write in the result table
  for(i in 1:8) {
    TestResult[a,i+3] <- s_exp[i+69,5]
  }
  TestResult[a,2] <- a_s[a]
  TestResult[a,3] <- "---"
  TestResult[a,12] <- mean(s_exp[38:70,6])
  TestResult[a,13] <- mean(s_exp[38:70,7])
  TestResult[a,14] <- mean(s_exp[38:70,7]^2)
  TestResult[a,15] <- mean(s_exp[38:70,7]/s_exp[38:70,1])
  TestResult[a,16] <- mean(s_exp[38:70,8])
  TestResult[a,17] <- mean(s_exp[38:70,9])
}
#Double Exponential Smoothing Approach
d_exp <- data.frame(D_t=origin_data[,1],S_j=origin_data[,5])
d_exp[71,2] <- Sj[5]
d_exp[72,2] <- Sj[6]
for(i in 1:6) {
  d_exp[72+i,2] <- Sj[i]
}
d_exp[1:70,3] <- origin_data[,6]
d_exp[,4:11] <- 0
names(d_exp)[3:11] <- c("D*_t","F*_t,t+1","L_t","T_t","F_t,t+1","e_t","|e|_t","E_t","MAD_t")
# Initialize: the L_t and T_t value for No.29 come from regression test
d_exp[29,6] <- -0.06359
d_exp[29,5] <- d_exp[29,6] * 29 + 18.652
d_exp[29,4] <- d_exp[29,5] + d_exp[29,6]
a_d <- c(0.05,0.1,0.15,0.2,0.25,0.3)
b_d <- c(0.01,0.04,0.07,0.1,0.13,0.16,0.2)
for(a in 1:length(a_d)) {
  for(b in 1:length(b_d)) {
    for(i in 30:70) {
      d_exp[i,5] <- a_d[a] * d_exp[i,3] + (1 - a_d[a]) * d_exp[i-1,4]
      d_exp[i,6] <- b_d[b] * (d_exp[i,5] - d_exp[i-1,5]) + (1 - b_d[b]) * d_exp[i-1,6]
      d_exp[i,4] <- d_exp[i,5] + d_exp[i,6]
    }
    for(i in 2:8) {
      d_exp[i+69,4] <- d_exp[70,5] + i * d_exp[70,6]
    }
    for(i in 30:77) {
      d_exp[i,7] <- d_exp[i,4] * d_exp[i+1,2]
    }
    for(i in 31:70) {
      d_exp[i,8] <- d_exp[i,1] - d_exp[i-1,7]
      d_exp[i,9] <- abs(d_exp[i,8])
    }
    d_exp[37,10] <- mean(d_exp[31:36,8])
    d_exp[37,11] <- mean(d_exp[31:36,9])
    for(i in 38:70) {
      d_exp[i,10] <- 0.1 * d_exp[i,8] + 0.9 * d_exp[i-1,10]
      d_exp[i,11] <- 0.1 * d_exp[i,9] + 0.9 * d_exp[i-1,11]
    }
    # Write in the result table
    # The position in the result table should be 6+7*(a-1)+b
    for(i in 1:8) {
      TestResult[6+7*(a-1)+b,i+3] <- d_exp[i+69,7]
    }
    TestResult[6+7*(a-1)+b,2] <- a_d[a]
    TestResult[6+7*(a-1)+b,3] <- b_d[b]
    TestResult[6+7*(a-1)+b,12] <- mean(d_exp[38:70,8])
    TestResult[6+7*(a-1)+b,13] <- mean(d_exp[38:70,9])
    TestResult[6+7*(a-1)+b,14] <- mean(d_exp[38:70,9]^2)
    TestResult[6+7*(a-1)+b,15] <- mean(d_exp[38:70,9]/d_exp[38:70,1])
    TestResult[6+7*(a-1)+b,16] <- mean(d_exp[38:70,10])
    TestResult[6+7*(a-1)+b,17] <- mean(d_exp[38:70,11])
  }
}
TestResult
# Write the result into csv document
write.csv(TestResult,file.path(datadirectory,"hw2 test result.csv"))


