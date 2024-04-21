######################## Include Libraries  #################################################################

library(ggplot2)
library(olsrr)
library(leaps)
library(readxl)
library(corrplot)
library(reshape2)
library(car)
censusDataProject3 <- read_excel("D:/Material/ICS/Project3/Code/Concrete_Data.xls")


###############################################################################################

names(censusDataProject3)

names(censusDataProject3)[1] <- "Cement"
names(censusDataProject3)[2] <- "Blast_Furnace_Slag"
names(censusDataProject3)[3] <- "Fly_Ash"
names(censusDataProject3)[4] <- "Water"
names(censusDataProject3)[5] <- "Superplasticizer"
names(censusDataProject3)[6] <- "Coarse_Aggregate"
names(censusDataProject3)[7] <- "Fine_Aggregate"
names(censusDataProject3)[9] <- "Concrete_Compressive_Strength"


############################  Task 1   #######################################################################
summary(censusDataProject3)

# Calculate the correlation matrix
cor_matrix <- cor(censusDataProject3)
print(cor_matrix)


correlation_df <- melt(cor_matrix) 
summary(correlation_df)

ggplot(data = correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1, size = 3, color = "black") +  # Adjust size and color
  scale_fill_gradient2(low = "lightblue", mid = "white", high = "lightcoral", midpoint = 0,
                       limits = c(-1, 1), na.value = "grey50") +
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

############################  Task 2  ##############################################################################




# Selecting all columns except 'Concrete compressive strength' for independent variables
independent_vars <- names(censusDataProject3)[names(censusDataProject3) != "Concrete_Compressive_Strength"]
print(independent_vars)

# Selecting all columns except 'Concrete co8.
lm_model <- lm(`Concrete_Compressive_Strength` ~ ., data = censusDataProject3[, c("Concrete_Compressive_Strength", independent_vars)])

# Summary of the linear regression model
summary(lm_model)
plot(lm_model)

###################################################  Non - Linearity for age  #####################################

#Now to check the non-linearity for age
censusDataProject3$AgeCube <- censusDataProject3$`Age (day)`^3
# Selecting all columns except 'Concrete compressive strength' for independent variables
independent_vars_new <- names(censusDataProject3)[names(censusDataProject3) != "Concrete_Compressive_Strength"]
print(independent_vars_new)

# Selecting all columns except 'Concrete co8.
lm_model_new <- lm(`Concrete_Compressive_Strength` ~ ., data = censusDataProject3[, c("Concrete_Compressive_Strength", independent_vars_new)])

# Summary of the linear regression model
summary(lm_model_new)


################################ Scatter plot showing non-linearity of age
ggplot(data = censusDataProject3, aes(x = censusDataProject3$AgeCube, y = censusDataProject3$`Concrete_Compressive_Strength`)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Age", y = "Concrete_Compressive_Strength", 
       title = "Scatterplot with Smoother") +
  theme_minimal()


################################### Task 3 & 4  ##################################################################
best_model_Selection <- ols_step_best_subset(model = lm_model)
summary(best_model_Selection)
print(best_model_Selection)

########## Best model considering the New Model for including non-linearity for age
best_model_Selection_new <- ols_step_best_subset(model = lm_model_new)
summary(best_model_Selection_new)
print(best_model_Selection_new)







#Summary of  best model
BestModel <- lm(formula = Concrete_Compressive_Strength ~Cement+Blast_Furnace_Slag +Fly_Ash + Water + `Age (day)`+AgeCube,data = censusDataProject3)
summaryBestModel <- summary(BestModel)
print(summaryBestModel)

coeff_intervals <- confint(BestModel)
print(coeff_intervals)

plot(coeff_intervals)


plot(BestModel)

vif_values <- car::vif(BestModel)

print(vif_values)
############################################################# Task 4  #########################################################
