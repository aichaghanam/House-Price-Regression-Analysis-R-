# House Price Regression Analysis (R)

## Project Overview
This project investigates the main drivers of house prices using **multiple linear regression in R**.  
It includes a full statistical workflow: exploratory analysis, model building, validation of assumptions, response transformation, and variable selection to obtain an interpretable and robust final model.

## Dataset
- Source: `Projet_UA3.csv` (provided for academic purposes)
- Target variable: **price**
- Predictors include structural and socioeconomic variables such as:
  `lingA` (living area), `rooms`, `beds`, `baths`, `lotS`, `age`, `lValue`, `pctCol`, `firep`, etc.

> Note: The dataset file must be placed in the project folder (see **How to Run**).

## Objectives
- Explore correlations between variables and identify potential multicollinearity
- Study linear relationships between predictors and house price
- Build a multiple linear regression model
- Validate model assumptions (linearity, homoscedasticity, normality, independence)
- Improve the model using a transformation of the response variable
- Select the best subset of predictors using **BIC** and compare with **AIC stepwise selection**
- Test non-linearity (polynomial term) and interaction effects
- Provide predictions with prediction intervals

## Methods & Workflow
### 1. Exploratory Data Analysis
- Correlation analysis and pair plots with `GGally::ggpairs()`
- Scatterplot matrix with regression lines using `car::scatterplotMatrix()`

### 2. Initial Multiple Linear Regression
- Fit a full model: `lm(price ~ ., data = data)`
- Identify significant vs non-significant variables

### 3. Model Diagnostics
- Collinearity check (VIF) using `performance::check_collinearity()`
- Homoscedasticity test: Breusch–Pagan `lmtest::bptest()`
- Linearity test: `harvtest()`
- Residual independence: Durbin–Watson `lmtest::dwtest()`
- Normality of residuals: `shapiro.test()`

### 4. Response Transformation
- Box-Cox analysis (`MASS::boxcox()`)
- Log transformation of price: `price <- log(price)`

### 5. Variable Selection
- Best subset selection using `leaps::regsubsets()`
- Model choice based on **BIC** (primary criterion)
- Comparison using Mallows Cp and adjusted R²
- Stepwise selection using AIC (`MASS::stepAIC()`)

### 6. Non-Linearity & Interaction
- LOESS vs linear smoothing plots (ggplot)
- Polynomial model for living area: `poly(lingA, 2, raw = TRUE)`
- Interaction model: `lingA * baths_T`
- Model comparisons: `AIC()` and `anova()`

## Final Model (BIC)
The final selected model (based on BIC) includes:
- `lotS`, `age`, `lValue`, `lingA`, `baths`

It explains ~56% of the variability in **log(price)** while remaining simple and interpretable.

## Tools & Packages
- **R**
- `GGally`, `car`, `performance`, `see`, `lmtest`, `MASS`, `leaps`, `reshape2`, `ggplot2`

## Project Structure
