library(tidyverse)
library(tidymodels)
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
conflicts_prefer(recipes::step())
titanic_train <- read.csv("Assessment/titanic_train.csv")
glimpse(titanic_train)
titanic_train[titanic_train==""] <- NA
titanic_train <- titanic_train |>
  mutate(
    Survived=case_match(
      Survived,
      0 ~ "No",
      1 ~ "Yes"
    ),
    Survived=as.factor(Survived),
    Survived=fct_rev(Survived),
    Pclass=as.factor(Pclass),
    Sex=as.factor(Sex),
    Sex=fct_rev(Sex),
    Age=case_when(
      Age/10 <= 1 ~ "0-10",
      Age/10 <= 2 ~ "10-20",
      Age/10 <= 3 ~ "20-30",
      Age/10 <= 4 ~ "30-40",
      Age/10 <= 5 ~ "40-50",
      Age/10 <= 6 ~ "50-60",
      Age/10 > 6 ~ "60+",
      is.na(Age) ~ NA,
    ),
    Age=as.factor(Age),
    Family=SibSp + Parch,
    Family=case_when(
      Family == 0 ~ "Single",
      Family <= 3 ~ "Small",
      between(Family, 4, 6) ~ "Medium",
      Family >= 7 ~ "Large"
    ),
    Family=as.factor(Family),
    Family=fct_rev(Family) |> fct_relevel("Single"),
    Embarked=case_match(
      Embarked,
      "C" ~ "Cherbourg",
      "Q" ~ "Queenstown",
      "S" ~ "Southampton",
      .default=NA,
    ),
    Embarked=as.factor(Embarked)
  )
titanic_train |>
  ggplot(aes(x=Age, fill=Sex)) +
  geom_bar(position = "dodge") +
  theme_bw()
titanic_train |>
  ggplot(aes(x=Age, group=Survived, fill=Survived)) +
  geom_bar(aes(y=after_stat(prop)), position="dodge") +
  theme_bw()
titanic_train |>
  ggplot(aes(x=Sex, group=Survived, fill=Survived)) +
  geom_bar(aes(y=after_stat(prop)), position="dodge") +
  theme_bw()
titanic_train |>
  ggplot(aes(x=Family, group=Survived, fill=Survived)) +
  geom_bar(aes(y=after_stat(prop)), position="dodge") +
  theme_bw()
titanic_train |>
  ggplot(aes(x=Pclass, group=Survived, fill=Survived)) +
  geom_bar(aes(y=after_stat(prop)), position="dodge") +
  theme_bw()
titanic_train |>
  ggplot(aes(x=Embarked, group=Survived, fill=Survived)) +
  geom_bar(aes(y=after_stat(prop)), position="dodge") +
  theme_bw()
data <- titanic_train |>
  select(Survived, Age, Sex, Family, Pclass, Embarked)
set.seed(1024)
split <- initial_split(data)
folds <- vfold_cv(data, strata=Survived)
recipe <- recipe(Survived ~ ., data=split) |>
  step_impute_knn(
    Age, Embarked,
    impute_with=imp_vars(Sex, Family, Pclass)
  )
dummy <- recipe |>
  step_dummy(
    all_nominal_predictors(),
    one_hot=TRUE
  )
work <- \(model, recipe){
  workflow <- workflow() |>
    add_recipe(recipe) |>
    add_model(model)
  workflow |>
    last_fit(split) |>
    collect_metrics() |>
    print()
  workflow |>
    fit_resamples(folds) |>
    collect_metrics() |>
    print()
}
rand_forest(
  mode="classification", engine="ranger"
) |> work(recipe)
boost_tree(
  mode="classification", engine="xgboost"
) |> work(dummy)
