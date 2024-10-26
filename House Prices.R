library(tidyverse)
library(tidymodels)
library(patchwork)
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
conflicts_prefer(recipes::step())
house_prices_train <-
  read.csv("Assessment/house_prices_train.csv")
house_prices_test <-
  read.csv("Assessment/house_prices_test.csv")
house_prices_data <- bind_rows(
  list("train"=house_prices_train, "test"=house_prices_test),
  .id = "From"
)
glimpse(house_prices_data)
house_prices_data |> select(-SalePrice) |>
  summarise(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(
    everything(), names_to="columns", values_to="na_count"
  ) |> filter(na_count!=0)
house_prices_data <- house_prices_data |>
  mutate(
    MSSubClass=as.factor(MSSubClass),
    Alley=if_else(is.na(Alley), "None", Alley),
    YearBuilt=YrSold - YearBuilt,
    YearBuilt=if_else(
      YearBuilt < 0, 0, YearBuilt
    ),
    YearRemodAdd=YrSold - YearRemodAdd,
    YearRemodAdd=case_when(
      YearRemodAdd <= 0 ~ "0",
      YearRemodAdd / 10 < 1 ~ "1-10",
      YearRemodAdd / 10 < 2 ~ "11-20",
      YearRemodAdd / 10 < 3 ~ "21-30",
      YearRemodAdd / 10 < 4 ~ "31-40",
      YearRemodAdd / 10 < 5 ~ "41-50",
      YearRemodAdd / 10 <= 6 ~ "51-60",
    ),
    YearRemodAdd=as.factor(YearRemodAdd),
    Exterior2nd=if_else(
      Exterior2nd == Exterior1st, "None", Exterior2nd
    ),
    MasVnrType=if_else(
      is.na(MasVnrType) & is.na(MasVnrArea),
      "None", MasVnrType
    ),
    MasVnrArea=if_else(
      is.na(MasVnrArea), 0, MasVnrArea
    ),
    ExterQual=case_match(
      ExterQual,
      "Ex" ~ 5,
      "Gd" ~ 4,
      "TA" ~ 3,
      "Fa" ~ 2,
      "Po" ~ 1
    ),
    ExterCond=case_match(
      ExterCond,
      "Ex" ~ 5,
      "Gd" ~ 4,
      "TA" ~ 3,
      "Fa" ~ 2,
      "Po" ~ 1
    ),
    BsmtQual=case_when(
      BsmtQual == "Ex" ~ 5,
      BsmtQual == "Gd" ~ 4,
      BsmtQual == "TA" ~ 3,
      BsmtQual == "Fa" ~ 2,
      BsmtQual == "Po" ~ 1,
      is.na(BsmtQual) ~ 0,
    ),
    BsmtCond=case_when(
      BsmtCond == "Ex" ~ 5,
      BsmtCond == "Gd" ~ 4,
      BsmtCond == "TA" ~ 3,
      BsmtCond == "Fa" ~ 2,
      BsmtCond == "Po" ~ 1,
      is.na(BsmtCond) ~ 0,
    ),
    BsmtExposure=case_when(
      BsmtExposure == "Gd" ~ 4,
      BsmtExposure == "Av" ~ 3,
      BsmtExposure == "Mn" ~ 2,
      BsmtExposure == "No" ~ 1,
      is.na(BsmtExposure) ~ 0,
    ),
    BsmtFinType1=case_when(
      BsmtFinType1 == "GLQ" ~ 6,
      BsmtFinType1 == "ALQ" ~ 5,
      BsmtFinType1 == "BLQ" ~ 4,
      BsmtFinType1 == "Rec" ~ 3,
      BsmtFinType1 == "LwQ" ~ 2,
      BsmtFinType1 == "Unf" ~ 1,
      is.na(BsmtFinType1) ~ 0,
    ),
    BsmtFinType2=case_when(
      BsmtFinType2 == "GLQ" ~ 6,
      BsmtFinType2 == "ALQ" ~ 5,
      BsmtFinType2 == "BLQ" ~ 4,
      BsmtFinType2 == "Rec" ~ 3,
      BsmtFinType2 == "LwQ" ~ 2,
      BsmtFinType2 == "Unf" ~ 1,
      is.na(BsmtFinType2) ~ 0,
    ),
    across(
      c(BsmtFinSF1, BsmtFinSF2:TotalBsmtSF),
      \(x) x=if_else(is.na(x), 0, x)
    ),
    HeatingQC=case_match(
      HeatingQC,
      "Ex" ~ 5,
      "Gd" ~ 4,
      "TA" ~ 3,
      "Fa" ~ 2,
      "Po" ~ 1
    ),
    CentralAir=case_match(
      CentralAir,
      "Y" ~ 1,
      "N" ~ 0
    ),
    KitchenQual=case_match(
      KitchenQual,
      "Ex" ~ 5,
      "Gd" ~ 4,
      "TA" ~ 3,
      "Fa" ~ 2,
      "Po" ~ 1
    ),
    Functional=if_else(
      is.na(Functional), "Typ", Functional
    ),
    FireplaceQu=case_when(
      FireplaceQu == "Ex" ~ 5,
      FireplaceQu == "Gd" ~ 4,
      FireplaceQu == "TA" ~ 3,
      FireplaceQu == "Fa" ~ 2,
      FireplaceQu == "Po" ~ 1,
      is.na(FireplaceQu) ~ 0,
    ),
    GarageYrBlt=YrSold - GarageYrBlt,
    GarageYrBlt=case_when(
      GarageYrBlt <= 0 ~ "0",
      GarageYrBlt / 10 < 1 ~ "1-10",
      GarageYrBlt / 10 < 2 ~ "11-20",
      GarageYrBlt / 10 < 3 ~ "21-30",
      GarageYrBlt / 10 < 4 ~ "31-40",
      GarageYrBlt / 10 < 5 ~ "41-50",
      GarageYrBlt / 10 < 6 ~ "51-60",
      GarageYrBlt / 10 < 7 ~ "61-70",
      GarageYrBlt / 10 < 8 ~ "71-80",
      GarageYrBlt / 10 < 9 ~ "81-90",
      GarageYrBlt / 10 < 10 ~ "91-100",
      GarageYrBlt / 10 >= 10 ~ "100+",
    ),
    across(
      c(GarageType:GarageFinish),
      \(x) x=if_else(is.na(GarageType), "None", x)
    ),
    GarageYrBlt=as.factor(GarageYrBlt) |>
      fct_relevel("100+", "None", after=Inf),
    GarageQual=case_when(
      GarageQual == "Ex" ~ 5,
      GarageQual == "Gd" ~ 4,
      GarageQual == "TA" ~ 3,
      GarageQual == "Fa" ~ 2,
      GarageQual == "Po" ~ 1,
      is.na(GarageQual) ~ 0
    ),
    GarageCond=case_when(
      GarageCond == "Ex" ~ 5,
      GarageCond == "Gd" ~ 4,
      GarageCond == "TA" ~ 3,
      GarageCond == "Fa" ~ 2,
      GarageCond == "Po" ~ 1,
      is.na(GarageCond) ~ 0
    ),
    PoolQC=case_when(
      PoolQC == "Ex" ~ 4,
      PoolQC == "Gd" ~ 3,
      PoolQC == "TA" ~ 2,
      PoolQC == "Fa" ~ 1,
      is.na(PoolQC) ~ 0
    ),
    Fence=case_when(
      Fence == "GdPrv" ~ 4,
      Fence == "MnPrv" ~ 3,
      Fence == "GdWo" ~ 2,
      Fence == "MnWw" ~ 1,
      is.na(Fence) ~ 0
    ),
    MiscFeature=if_else(
      MiscVal==0, "None", MiscFeature
    ),
    MiscFeature=if_else(
      is.na(MiscFeature), "Gar2", MiscFeature
    ),
    across(
      c(MoSold, YrSold),
      \(x) x=as.factor(x)
    ),
    across(
      where(is.character),
      \(x) x=as.factor(x) |> fct_infreq()
    ),
    GarageType=GarageType |>
      fct_relevel("None", after=Inf),
  )
train_data <- house_prices_data |>
  filter(From=="train") |>
  select(-From, -Id)
test_data <- house_prices_data |>
  filter(From=="test") |>
  select(-From, -Id, -SalePrice)
test_id <- house_prices_data |>
  filter(From=="test") |>
  select(Id)
to_factor <- c(
  "OverallQual", "OverallCond",
  "ExterQual", "ExterCond",
  "BsmtQual", "BsmtCond", "BsmtExposure",
  "BsmtFinType1", "BsmtFinType2",
  "HeatingQC", "CentralAir",
  "BsmtFullBath", "BsmtHalfBath",
  "FullBath", "HalfBath",
  "BedroomAbvGr", "TotRmsAbvGrd",
  "KitchenAbvGr", "KitchenQual",
  "Fireplaces", "FireplaceQu",
  "GarageCars", "GarageQual", "GarageCond",
  "PoolQC", "Fence"
)
plot_data <- train_data
for(x in to_factor){
  plot_data[[x]] <- as.factor(plot_data[[x]])
}
columns <- test_data |> colnames()
for(x in columns){
  if(is.factor(plot_data[[x]])){
    barPlot <- plot_data |>
      ggplot(aes(x=.data[[x]])) +
      geom_bar(
        aes(y=after_stat(count)),
        color="royalblue", fill="skyblue"
      ) + theme_bw()
    boxPlot <- plot_data |>
      ggplot(aes(y=SalePrice)) +
      geom_boxplot(
        aes(x=.data[[x]]),
        color="royalblue", fill="skyblue"
      ) + scale_y_continuous(
        breaks= seq(0, 800000, by=100000), labels=comma
      ) + theme_bw()
    (barPlot | boxPlot) |> print()
  }else if(is.numeric(plot_data[[x]])){
    IQR <- plot_data |> select({{x}}) |>
      filter(.data[[x]]!=0) |>
      unlist() |> IQR(na.rm=TRUE)
    width <- round(IQR/5, 0) + 1
    histogramPlot <- plot_data |>
      ggplot(aes(x=.data[[x]])) +
      geom_histogram(
        na.rm=TRUE, binwidth=width, center=width/2,
        color="royalblue", fill="skyblue"
      ) + theme_bw()
    pointPlot <- plot_data |>
      ggplot(aes(y=SalePrice)) +
      geom_point(
        aes(x=.data[[x]]), na.rm=TRUE, color="royalblue"
      ) + scale_y_continuous(
        breaks= seq(0, 800000, by=100000), labels=comma
      ) + theme_bw()
    (histogramPlot | pointPlot) |> print()
  }
}
train_data |>
  ggplot(aes(x=SalePrice)) +
  geom_histogram(
    na.rm=TRUE, binwidth=10000, center=5000,
    color="royalblue", fill="skyblue"
  ) + scale_x_continuous(
    breaks= seq(0, 800000, by=100000), labels=comma
  )+ theme_bw()
train_data |>
  ggplot(aes(sample=SalePrice)) +
  geom_qq(color="slateblue") +
  geom_qq_line(color="royalblue", linewidth=1)+
  scale_y_continuous(
    breaks= seq(0, 800000, by=100000), labels=comma
  ) + theme_bw()
train_data <- train_data |>
  mutate(SalePrice=log(SalePrice, base=10))
train_data |>
  ggplot(aes(sample=SalePrice)) +
  geom_qq(color="slateblue") +
  geom_qq_line(color="royalblue", linewidth=1)+
  theme_bw()
Features <- c()
Normality <- c()
for(x in columns){
  if(is.factor(plot_data[[x]])){
    result <- plot_data |>
      group_by(.data[[x]]) |>
      summarise(
        Result=tryCatch({
          if_else(
              shapiro.test(SalePrice)$p.value>=0.05,
              TRUE, FALSE
            )
        },error=\(e){NA})
      )
    Features <- Features |> append(x)
    Normality <- Normality |> append(
      if_else(
        result |> filter(Result==FALSE) |> nrow() == 0,
        TRUE, FALSE
      )
    )
  }else if(is.numeric(plot_data[[x]])){
    Features <- Features |> append(x)
    Normality <- Normality |> append(
      if_else(
        shapiro.test(plot_data[[x]])$p.value>=0.05,
        TRUE, FALSE
      )
    )
  }
}
Features <- Features |> append("SalePrice")
Normality <- Normality |> append(
  if_else(
    shapiro.test(train_data$SalePrice)$p.value>=0.05,
    TRUE, FALSE
  )
)
tibble(Features, Normality) |> print(n=80)
Features <- c()
Homogeneity <- c()
for(x in columns){
  if(is.factor(plot_data[[x]])){
    Features <- Features |> append(x)
    Homogeneity <- Homogeneity |>
      append(
        if_else(
          fligner.test(
            plot_data$SalePrice, plot_data[[x]]
          )$p.value>=0.05,
          TRUE, FALSE
        )
      )
  }
}
tibble(Features, Homogeneity) |> print(n=59)
for(x in columns){
  if(is.factor(plot_data[[x]])){
    cat("x=", x, "\n")
    kruskal.test(
      plot_data$SalePrice, plot_data[[x]]
    ) |> print()
  }else if(is.numeric(plot_data[[x]])){
    cat("x=", x, "\n")
    cor.test(
      plot_data$SalePrice, plot_data[[x]],
      method="kendall"
    ) |> print()
  }
}
train_data <- train_data |>
  select(-Street, -Utilities, -LandSlope, -BsmtFinSF2, -BsmtHalfBath, -PoolQC, -MoSold, -YrSold)
test_data <- test_data |>
  select(-Street, -Utilities, -LandSlope, -BsmtFinSF2, -BsmtHalfBath, -PoolQC, -MoSold, -YrSold)
set.seed(1024)
split_data <- initial_split(train_data)
fold_data <- vfold_cv(train_data, strata=SalePrice)
model <- boost_tree(
  mtry=tune(), trees=1000, min_n=tune(),
  tree_depth=10, learn_rate=0.01) |>
  set_engine("xgboost", nthread=8, counts=FALSE) |>
  set_mode("regression")
recipe <- recipe(SalePrice ~ ., data=split_data) |>
  step_impute_knn(
    MSZoning, LotFrontage, Electrical,
    impute_with=imp_vars(
      MSSubClass, Neighborhood
    )
  ) |>
  step_impute_knn(
    Exterior1st, Exterior2nd, MasVnrType,
    impute_with=imp_vars(
      MasVnrArea, ExterQual, ExterCond
    )
  ) |>
  step_impute_knn(
    BsmtQual, BsmtCond, BsmtExposure, BsmtFinType2, BsmtFullBath,
    impute_with=imp_vars(
      BsmtFinType1, BsmtFinSF1, BsmtUnfSF, TotalBsmtSF
    )
  ) |>
  step_impute_knn(
    GarageYrBlt, GarageFinish, GarageCars, GarageArea, GarageQual, GarageCond,
    impute_with=imp_vars(GarageType)
  ) |>
  step_impute_knn(
    KitchenQual,
    impute_with=imp_vars(KitchenAbvGr)
  ) |>
  step_impute_knn(
    SaleType,
    impute_with=imp_vars(SaleCondition)
  ) |>
  step_dummy(
    all_nominal_predictors(),
    one_hot=TRUE
  )
workflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(model)
tune <- workflow |>
  tune_bayes(
    resamples=fold_data,
    param_info=parameters(
      mtry=mtry_prop(), min_n()
    ),
    metrics=metric_set(rmse, rsq)
  )
tune |> autoplot()
tune |> show_best(metric="rmse")
tune |> show_best(metric="rsq")
params <- tune |> select_best(metric="rmse")
final <- workflow |> finalize_workflow(params)
final |> last_fit(split_data) |>
  collect_metrics()
fit <- final |> fit(train_data)
predict <- fit |> predict(test_data) |>
  rename(SalePrice=.pred) |>
  mutate(SalePrice=10^SalePrice |> round(0))
submission <- test_id |>
  select(Id) |>
  bind_cols(predict)
submission |> write.csv(
  "submission.csv", row.names = FALSE
)
