library(tidyverse)
library(corrplot)
library(kableExtra)

data <- read_csv("data/heart.csv") 
data$ca <- ordered(as.factor(data$ca),
                       levels = c("0","1","2","3"))
data$sex <- as_factor(data$sex)
data$cp <- as_factor(data$cp)
data$fbs <- as_factor(data$fbs)
data$restecg <- as_factor(data$restecg)
data$exang <- as_factor(data$exang)
data$slope <- as_factor(data$slope)
data$thal <- as_factor(data$thal)
data$target <- as_factor(data$target)


#Tibble for printing ease
og <- c("sex","cp","fbs","restecg","thal","exang","ca","age","trestbps","chol",
        "thalach","oldpeak")
new <- c("gender","cp_type","fbs_type","restecg_type","thal_type",
         "exang_type","ca","age","trestbps","chol","thalach","oldpeak")
print_string <- c("Sex","Chest Pain Type","Fasting blood sugar",
                  "Resting ECG result","Blood disorder(thalassemia)",
                  "Exercise induced angina",
                  "Number of major vessels colored by flourosopy","Age",
                  "Resting Blood Pressure","Serum cholestoral in mg/dl",
                  "Maximum heart rate",
                  "ST depression induced by exercise relative to rest")
ct_tibble <- tibble(og,new,print_string)
#---------------------------------------------------------------------
#Split data
set.seed(52)
split=0.8
# Get the indexes for training data
train_size <- sample(nrow(data), nrow(data)*split)
# Get training data
train_df <- data[train_size,]
# Get test data
test_df <- data[-train_size,] 
#---------------------------------------------------------------------
#Data manipulation
train_df$gender <- if_else(train_df$sex==1,"Male","Female")

train_df$cp_type <- ifelse(train_df$cp==0,"Typical Angina",
                       if_else(train_df$cp==1,"Atypical Angina",
                               if_else(train_df$cp==2,"Non-Anginal",
                                       "Aysmptomatic")))
train_df$cp_type <- ordered(as.factor(train_df$cp_type),
                        levels = c("Typical Angina","Atypical Angina",
                                   "Non-Anginal","Aysmptomatic"))

train_df$fbs_type <- ifelse(train_df$fbs==1,"Greater than 120 mg/dl",
                            "Less than 120 mg/dl")
train_df$fbs_type <- ordered(as.factor(train_df$fbs_type),
                             levels = c("Less than 120 mg/dl",
                                        "Greater than 120 mg/dl"))

train_df$restecg_type <- ifelse(train_df$restecg==0,"Normal",
                                ifelse(train_df$restecg==1,
                                       "Having ST-T wave abnormality",
                                       "Showing left ventricular hypertrophy"))
train_df$restecg_type <- ordered(as.factor(train_df$restecg_type),
                             levels = c("Normal",
                                        "Having ST-T wave abnormality",
                                        "Showing left ventricular hypertrophy"))

train_df$thal_type <- ifelse(train_df$thal==1,"Normal",
                       if_else(train_df$thal==2,"Fixed defect",
                               if_else(train_df$thal==3,"Reversable defect",
                                       "Not captured")))
train_df$thal_type <- ordered(as.factor(train_df$thal_type),
                        levels = c("Normal","Fixed defect",
                                   "Reversable defect","Not captured"))

train_df$exang_type <- ifelse(train_df$exang==1,"Yes","No")
train_df$exang_type <- ordered(as.factor(train_df$exang_type),
                              levels = c("No","Yes"))

train_df$target_type <- ifelse(train_df$target==0,"No disease","Disease")
train_df$target_type <- ordered(as.factor(train_df$target_type),
                               levels = c("No disease","Disease"))

#----------------------------------------------------------------------
#Numerical Summaries
#Can give the user the option to select the variables for which the user wants
#numerical summaries
df_summary <- train_df %>%
  select(age,trestbps,chol,thalach, oldpeak)
predictor_table <- apply(df_summary, MARGIN = 2,FUN = summary, na.rm = TRUE)
predictor_table %>%
  kbl(caption="Summary table for predictor variables") %>%
  kable_classic(full_width = F)


#---------------------------------------------------------------------
# EDA - Contingency Tables

input_eda1 = "sex"
input_eda2 = "cp"

input_eda1_type <- ct_tibble[og==input_eda1,]$new
input_eda1_str <- ct_tibble[og==input_eda1,]$print_string
input_eda2_type <- ct_tibble[og==input_eda2,]$new
input_eda2_str <- ct_tibble[og==input_eda2,]$print_string

tab <- table(train_df[input_eda1_type][[1]],train_df[input_eda2_type][[1]])
tab %>%
  kbl(caption=paste0("Table for ",input_eda1_str," and ",input_eda2_str)) %>%
  kable_classic(full_width = F)

#Target
tab_target <- table(train_df$target_type)
tab_target %>%
  kbl(caption="Table for target variable") %>%
  kable_classic(full_width = F)
#---------------------------------------------------------------------
#Box plots and histograms for numerical variables

# Age
ggplot(train_df,aes(x=age)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() 
ggplot(train_df,aes(age)) +
  geom_boxplot(fill="steelblue")

# Resting blood pressure
ggplot(train_df,aes(x=trestbps)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() 
ggplot(train_df,aes(trestbps)) +
  geom_boxplot(fill="steelblue")

# Cholesterol
ggplot(train_df,aes(x=chol)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() 
ggplot(train_df,aes(chol)) +
  geom_boxplot(fill="steelblue")

# Max Heart Rate 
ggplot(train_df,aes(x=thalach)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() 
ggplot(train_df,aes(thalach)) +
  geom_boxplot(fill="steelblue")
#---------------------------------------------------------------------
# Visualizations for categorical variables

#Sex
ggplot(train_df,aes(x=gender)) + geom_bar(width=0.5,aes(fill=gender)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Sex") + 
  labs(x ="Gender",y="Count",title="Bar plot for Gender") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Chest pain type
ggplot(train_df,aes(x=cp_type)) + geom_bar(width=0.5,aes(fill=cp_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Chest Pain Type") + 
  labs(x ="Chest pain type",y="Count",title="Bar plot for Chest Pain Type") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Resting ECG
ggplot(train_df,aes(x=restecg_type)) + geom_bar(width=0.5,aes(fill=restecg_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Resting ECG") + 
  labs(x ="Resting ECG",y="Count",title="Bar plot for Resting ECG") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Colored blood vessels
ggplot(train_df,aes(x=ca)) + geom_bar(width=0.5,aes(fill=ca)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Number of major vessels colored by flourosopy") + 
  labs(x ="Number of major vessels colored by flourosopy",
       y="Count",
       title="Bar plot for Number of major vessels colored by flourosopy") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Thal
ggplot(train_df,aes(x=thal_type)) + geom_bar(width=0.5,aes(fill=thal_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Blood disorder(thalassemia)") + 
  labs(x ="Blood disorder(thalassemia)",y="Count",
       title="Bar plot for Blood disorder(thalassemia)") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------
# EDA with Target variable (Categorical variables)

input_target_cat = "thal"
target_variable = "target_type"
input_target_type <- ct_tibble[og==input_target_cat,]$new
input_target_str <- ct_tibble[og==input_target_cat,]$print_string

ggplot(train_df,aes_string(x=input_target_type,fill=target_variable)) + 
  geom_bar(position = "dodge",width=0.5) +
  scale_fill_discrete(name = "Heart Disease") +
  labs(x =input_target_str,y="Count",
       title=paste0("Heart disease based on ",input_target_str)) +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5,
            position = position_dodge(width = 0.5))
#---------------------------------------------------------------------
# EDA with Target variable (Numerical variables)

# slider - 2 to 5 with 0.5 step
input_scatter_size = 2
input_target_num1 = "oldpeak" # X axis
input_target_num2 = "trestbps" # Y axis
input_target_num3 = "sex" # Categorical variable
target_type_str = "target_type"
input_target_num_str1 <- ct_tibble[og==input_target_num1,]$print_string
input_target_num_str2 <- ct_tibble[og==input_target_num2,]$print_string
input_target_num3_type <- ct_tibble[og==input_target_num3,]$new
input_target_num_str3 <- ct_tibble[og==input_target_num3,]$print_string

ggplot(train_df,aes_string(x=input_target_num1,y=input_target_num2)) + 
  geom_point(aes_string(color=input_target_num3_type,shape=target_type_str),
             size=input_scatter_size) + geom_smooth(method = "lm") + 
  scale_color_discrete(name = input_target_num_str3) + 
  scale_shape_discrete(name = "Heart Disease") +
  labs(x =input_target_num_str1,y=input_target_num_str2,
       title=paste0("Scatter plot for ",input_target_num_str2," vs ",
                    input_target_num_str1))
#---------------------------------------------------------------------  

all_corr = cor(select_if(train_df, is.numeric), method = c("spearman"))
correlated_varaibles <- findCorrelation(all_corr,cutoff = 0.3,
                                        verbose=FALSE,names=TRUE,exact=TRUE)

corr_data1 <- train_df %>% select(age,trestbps,chol,thalach,oldpeak)
corr1 = cor(corr_data1,method = c("spearman"))
corrplot(corr1,diag=FALSE)





















