# https://archive.ics.uci.edu/dataset/222/bank+marketing

# Packages
library(tidyverse)

# Pull in Data
data<-read.csv("C:/Users/aabro/OneDrive/Desktop/SMU Program/Classes/Stats 2/Final Project/bank-full.csv",stringsAsFactors = T, sep=";")

# Split into training and test
set.seed(1234)
perc_train <- .8 # Should we make this higher?
train_indices <- sample(nrow(data), floor(perc_train * nrow(data)))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
yes_data <- train_data[train_data$y == 'yes',] # 4250
no_data <- train_data[train_data$y == 'no',] # 31918

# bank client data
# Age
class(data[,'age']) # integer
summary(data[,'age']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   33.00   39.00   40.94   48.00   95.00 
summary(yes_data[,'age']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.0    31.0    38.0    41.7    50.0    95.0 
summary(no_data[,'age']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   33.00   39.00   40.82   48.00   89.00  
hist(yes_data$age)
hist(no_data$age) # It looks like is far less likely to be over 60 and no

# bank client data
# Job
class(data[,'job']) # factor
summary(data[,'job']) / nrow(data) * 100
#        admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services       student 
# 11.4374820    21.5257349     3.2890226     2.7426954    20.9196877     5.0076309     3.4925129     9.1880295     2.0747163 
# technician    unemployed       unknown 
# 16.8034328     2.8820420     0.6370131
summary(yes_data[,'job']) / nrow(yes_data) * 100 # blue-color less, retired more, student more
# admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services       student 
# 12.2352941    13.4823529     2.2352941     2.2117647    24.5411765     9.8823529     3.4823529     7.0352941     5.0117647 
# technician    unemployed       unknown 
# 15.6000000     3.6470588     0.6352941
summary(no_data[,'job']) / nrow(no_data) * 100
#     admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services       student 
# 11.4261545    22.5327401     3.3147440     2.8071934    20.5902625     4.3799737     3.5152578     9.4210164     1.6385738 
# technician    unemployed       unknown 
# 16.9872799     2.7507989     0.6360048 
percentage <- prop.table(table(no_data[,'job'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Jobs for No", x = "Factor", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
percentage <- prop.table(table(yes_data[,'job'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Jobs for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bank client data
# marital
class(data[,'marital']) # factor
summary(data[,'marital']) / nrow(data) * 100
# divorced  married   single 
# 11.51711 60.19332 28.28958 
summary(yes_data[,'marital']) / nrow(yes_data) * 100 # married more, single less
# divorced  married   single 
# 12.00000 52.04706 35.95294 
summary(no_data[,'marital']) / nrow(no_data) * 100 
# divorced  married   single 
# 11.46375 61.29457 27.24168
percentage <- prop.table(table(no_data[,'marital'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Marraige for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'marital'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Marraige for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bank client data
# education
class(data[,'education']) # factor
summary(data[,'education']) / nrow(data) * 100
# primary secondary  tertiary   unknown 
# 15.153392 51.319369 29.419831  4.107407 
summary(yes_data[,'education']) / nrow(yes_data) * 100 # primary is lower, tertiary is higher
# primary secondary  tertiary   unknown 
# 10.870588 46.964706 37.388235  4.776471 
summary(no_data[,'education']) / nrow(no_data) * 100 
# primary secondary  tertiary   unknown 
# 15.583683 52.049001 28.419700  3.947616
percentage <- prop.table(table(no_data[,'education'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Education for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'education'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Education for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bank client data
# default: has credit in default?
class(data[,'default']) # factor
summary(data[,'default']) / nrow(data) * 100
# no       yes 
# 98.197341  1.802659 
summary(yes_data[,'default']) / nrow(yes_data) * 100 # about half as many (percentage) people with Yes are in default
# no       yes 
# 98.917647  1.082353
summary(no_data[,'default']) / nrow(no_data) * 100 
# no       yes 
# 98.088853  1.911147
percentage <- prop.table(table(no_data[,'default'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Default for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'default'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Default for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bank client data
# balance: average yearly balance, in euros
class(data[,'balance']) # integer
summary(data[,'balance']) # negative balance is allowed
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8019      72     448    1362    1428  102127
summary(yes_data[,'balance']) # higher median and mean balance for Yes
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3058     205     726    1798    2151   81204 
summary(no_data[,'balance']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6847      60     420    1299    1348  102127 
hist(yes_data$balance)
hist(no_data$balance) 

# bank client data
# housing: has housing loan?
class(data[,'housing']) # factor
summary(data[,'housing']) / nrow(data) * 100
# no       yes 
# 44.41618 55.58382
summary(yes_data[,'housing']) / nrow(yes_data) * 100 # More people with Yes don't have a house loan
# no       yes 
# 63.29412 36.70588
summary(no_data[,'housing']) / nrow(no_data) * 100 
# no       yes 
# 42.03584 57.96416
percentage <- prop.table(table(no_data[,'housing'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Housing for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'housing'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Housing for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bank client data
# loan: has personal loan?
class(data[,'loan']) # factor
summary(data[,'loan']) / nrow(data) * 100
# no       yes 
# 83.97735 16.02265 
summary(yes_data[,'loan']) / nrow(yes_data) * 100 # more people with Yes have no loans
# no       yes 
# 90.658824  9.341176 
summary(no_data[,'loan']) / nrow(no_data) * 100 
# no       yes 
# 83.10044 16.89956
percentage <- prop.table(table(no_data[,'loan'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Loan for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'loan'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Loan for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# related with the last contact of the current campaign
# contact: contact communication type
class(data[,'contact']) # factor
summary(data[,'contact']) / nrow(data) * 100
# cellular telephone   unknown 
# 64.774059  6.427639 28.798301  
summary(yes_data[,'contact']) / nrow(yes_data) * 100 # more with cell, less with unk
# cellular telephone   unknown 
# 82.729412  7.176471 10.094118
summary(no_data[,'contact']) / nrow(no_data) * 100 
# cellular telephone   unknown 
# 62.601040  6.297387 31.101573
percentage <- prop.table(table(no_data[,'contact'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Contact for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'contact'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Contact for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# related with the last contact of the current campaign
# day: last contact day of the month
class(data[,'day']) # integer
summary(data[,'day']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   16.00   15.81   21.00   31.00 
summary(yes_data[,'day']) # these basically look the same, as expected
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   15.00   15.12   22.00   31.00
summary(no_data[,'day']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   16.00   15.89   21.00   31.00
hist(yes_data$day)
hist(no_data$day) 

# related with the last contact of the current campaign
# month: last contact month of year
class(data[,'month']) # factor
summary(data[,'month']) / nrow(data) * 100
# apr        aug        dec        feb        jan        jul        jun        mar        may        nov        oct        sep 
# 6.4851474 13.8174338  0.4733361  5.8591936  3.1032271 15.2507133 11.8134967  1.0550530 30.4483422  8.7810489  1.6323461  1.2806618 
summary(yes_data[,'month']) / nrow(yes_data) * 100 # seems more evenly distributed than no
# apr       aug       dec       feb       jan       jul       jun       mar       may       nov       oct       sep 
# 10.917647 12.964706  1.905882  8.164706  2.611765 12.047059 10.635294  4.494118 17.364706  7.505882  6.188235  5.200000 
summary(no_data[,'month']) / nrow(no_data) * 100 
# apr        aug        dec        feb        jan        jul        jun        mar        may        nov        oct        sep 
# 5.9214236 13.9325772  0.3007707  5.6206529  3.1800238 15.8186603 11.9211730  0.5796103 32.0759446  8.8727364  0.9837709  0.7926562 
percentage <- prop.table(table(no_data[,'month'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Month for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'month'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Month for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# related with the last contact of the current campaign
# duration: last contact duration, in seconds
class(data[,'duration']) # integer
summary(data[,'duration']) # it looks like 0 is a value
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   103.0   180.0   258.2   319.0  4918.0 
summary(yes_data[,'duration']) # mean is significantly higher for yes
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.0   245.0   427.0   535.1   710.0  3881.0 
summary(no_data[,'duration']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    95.0   164.0   221.1   279.0  4918.0
hist(yes_data$duration)
hist(no_data$duration) 

# other attributes
# campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
class(data[,'campaign']) # integer
summary(data[,'campaign']) # It looks like there's a lot of 1's, consider using an indicator variable here
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.764   3.000  63.000 
summary(yes_data[,'campaign']) # 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.113   2.000  29.000 
summary(no_data[,'campaign']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.841   3.000  63.000
hist(yes_data$campaign)
hist(no_data$campaign) 

# other attributes
# pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
class(data[,'pdays']) # integer
summary(data[,'pdays']) # It looks like there's a lot of -1's, consider using an indicator variable here
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.0    -1.0    -1.0    40.2    -1.0   871.0 
summary(yes_data[,'pdays']) # 3rd quartile is more than -1, where 3rd is -1 for no
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.00   -1.00   -1.00   68.96   98.00  854.00 
summary(no_data[,'pdays']) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.00   -1.00   -1.00   36.54   -1.00  838.00 
hist(yes_data$pdays)
hist(no_data$pdays) 

# other attributes
# previous: number of contacts performed before this campaign and for this client
class(data[,'previous']) # integer
summary(data[,'previous']) # 0 is the most common number, consider using an indicator variable here
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000   0.0000   0.0000   0.5803   0.0000 275.0000
summary(yes_data[,'previous']) # 3rd quartile > 0
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   1.167   1.000  58.000
summary(no_data[,'previous']) # 3rd quartile = 0
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.4979  0.0000 51.0000 
hist(yes_data$previous)
hist(no_data$previous) 

# other attributes
# poutcome: outcome of the previous marketing campaign
class(data[,'poutcome']) # factor
summary(data[,'poutcome']) / nrow(data) * 100 # That's a lot of unknowns
# failure     other   success   unknown 
# 10.840282  4.069806  3.342107 81.747805 
summary(yes_data[,'poutcome']) / nrow(yes_data) * 100 # Success is way higher, which makes sense.  I'm honestly surprised it isn't close to 100%
# failure     other   success   unknown 
# 11.647059  5.505882 18.800000 64.047059
summary(no_data[,'poutcome']) / nrow(no_data) * 100 
# failure     other   success   unknown 
# 10.824613  3.816029  1.387932 83.971427 
percentage <- prop.table(table(no_data[,'poutcome'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Poutcome for No", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
percentage <- prop.table(table(yes_data[,'poutcome'])) * 100
df <- as.data.frame(percentage)
df$factor <- df[,'Var1']
ggplot(df, aes(x = factor, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), vjust = -0.5) +
  labs(title = "Poutcome for Yes", x = "Factor", y = "Percentage") +
  theme_minimal() + ylim(c(0,100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))