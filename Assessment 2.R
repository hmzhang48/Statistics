library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
tetris <- read_csv("./Assessment/tetris.csv") |>
  select(condition,score) |>
  mutate(condition = factor(condition))
tetris |>
  group_by(condition) |>
  summarise(
    N=length(score),
    min=min(score),
    max=max(score),
    mean=mean(score),
    sd=sd(score),
    median=median(score),
    IQR=IQR(score)
  )
tetris |>
  ggplot(aes(condition, score, fill = condition)) +
  geom_boxplot() +
  guides(fill = "none") +
  ggtitle("Boxplot of auditory and visual") +
  ylab("Score") +
  theme_bw()
tetris |>
  ggplot(aes(score, fill = condition)) +
  geom_histogram(
    aes(, after_stat(count)),
    binwidth = 5, center = 2.5,
    color="black") +
  guides(fill = "none") +
  ggtitle("Histogram of auditory and visual") +
  xlab("Score") + ylab("Count") +
  theme_bw()
tetris |>
  filter(condition == "auditory") |>
  pull(score) |>
  shapiro.test()
tetris |>
  filter(condition == "visual") |>
  pull(score) |>
  shapiro.test()
library('car')
tetris |>
  with(leveneTest(score, condition))
tetris |>
  with(t.test(score ~ condition, paired = FALSE))
tetris |>
  ggplot(aes(condition, score, fill = condition)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(
    fun.min = (
      \(x) {
        mean(x) + (sd(x) / sqrt(length(x)))
      }
    ),
    fun.max = (
      \(x) {
        mean(x) - (sd(x) / sqrt(length(x)))
      }
    ), geom = "errorbar", width = 0.5) +
  guides(fill = "none") +
  ggtitle("Barplot of auditory and visual") +
  xlab("Condition") + ylab("Mean Score") +
  theme_bw()
