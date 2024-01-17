library(tidyverse)
library(tidymodels)
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
conflicts_prefer(recipes::step())
titanic_train <- read.csv("Assessment/titanic_train.csv")
titanic_test <- read.csv("Assessment/titanic_test.csv")
titanic_data <- bind_rows(
  list("train"=titanic_train, "test"=titanic_test),
  .id = "From"
)
glimpse(titanic_data)
titanic_data[titanic_data==""] <- NA
titanic_data <- titanic_data |>
  mutate(
    Survived=as.factor(Survived),
    Survived=fct_rev(Survived),
    Pclass=as.factor(Pclass),
    Sex=as.factor(Sex),
    Sex=fct_rev(Sex),
    Family=SibSp + Parch + 1,
    Ticket=as.factor(Ticket),
    Embarked=as.factor(Embarked)
  ) |> group_by(Ticket) |>
  mutate(
    Group=n(),
    Fare=round(Fare/Group, 4)
  ) |> ungroup()
titanic_train <- titanic_data |>
  filter(From=="train") |>
  select(-From)
titanic_test <- titanic_data |>
  filter(From=="test") |>
  select(-From,-Survived)
titanic_train |>
  ggplot(aes(x=Age, fill=Sex)) +
  geom_histogram(
    position = "dodge", binwidth = 10, center = 5, na.rm=TRUE
  ) + theme_bw()
titanic_train |>
  ggplot(aes(x=Age, group=Survived, fill=Survived)) +
  geom_histogram(
    aes(y=after_stat(density)),
    position="dodge", binwidth = 10, center = 5, na.rm=TRUE
  ) + theme_bw()
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
  ggplot(aes(x=Fare, group=Survived, fill=Survived)) +
  geom_histogram(
    aes(y=after_stat(density)),
    position="dodge", binwidth = 25, center = 12.5
  ) + theme_bw()
titanic_train |>
  ggplot(aes(x=Embarked, group=Survived, fill=Survived)) +
  geom_bar(
    aes(y=after_stat(prop)),
    position="dodge", na.rm=TRUE
  ) + theme_bw()
train_data <- titanic_train |>
  select(
    Survived,
    Age, Sex, Family,
    Pclass, Ticket, Fare, Embarked
  )
test_data <- titanic_test |>
  select(
    PassengerId,
    Age, Sex, Family,
    Pclass, Ticket, Fare, Embarked
  )
set.seed(1024)
split_data <- initial_split(train_data)
fold_data <- vfold_cv(train_data, strata=Survived)
model <- rand_forest(
  mode="classification", engine="ranger", trees=1000,
  mtry=tune(), min_n=tune()
)
recipe <- recipe(Survived ~ ., data=split_data) |>
  step_impute_knn(
    Age, Fare, Embarked,
    impute_with=imp_vars(
      Sex, Family, Pclass, Ticket
    )
  )
workflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(model)
grid <- grid_max_entropy(
  mtry(c(1,7)),
  min_n(),
  size=20
)
tune <- workflow |>
  tune_grid(
    resamples=fold_data,
    grid=grid,
    metrics=metric_set(accuracy, roc_auc)
  )
tune |> autoplot()
tune |> show_best(metric="accuracy")
tune |> show_best(metric="roc_auc")
params <- tune |> select_best(metric="roc_auc")
final <- workflow |>
  finalize_workflow(params)
final |> last_fit(split_data) |>
  collect_metrics()
fit <- final |> fit(train_data)
predict <- fit |> predict(test_data) |>
  rename(Survived=.pred_class)
submission <- test_data |>
  select(PassengerId) |>
  bind_cols(predict)
submission |> write.csv(
  "submission.csv", row.names = FALSE
)
