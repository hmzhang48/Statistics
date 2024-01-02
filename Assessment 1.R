library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
Scores <- read_csv("./Assessment/SusRtlx.csv") |>
  select(SUS.Score, RTLX.Score)
Scores <- Scores |>
  mutate(
    SUS.Score = if_else(
      SUS.Score < 0 | SUS.Score > 100, NA, SUS.Score
    )
  )
Scores <- Scores |>
  mutate(
    RTLX.Score = if_else(
      RTLX.Score < 0 | RTLX.Score > 126, NA, RTLX.Score
    )
  )
statistics <- \(x) {
  tibble(
    N=length(x),
    min=min(x, na.rm=TRUE),
    max=max(x, na.rm=TRUE),
    mean=mean(x, na.rm=TRUE),
    sd=sd(x, na.rm=TRUE),
    median=median(x, na.rm=TRUE),
    IQR=IQR(x, na.rm=TRUE)
  )
}
Scores$SUS.Score |>
  statistics()
Scores$RTLX.Score |>
  statistics()
Scores |>
  filter(!is.na(SUS.Score)) |>
  ggplot(aes(SUS.Score)) +
  geom_histogram(aes(, after_stat(count)),
    binwidth = 10, center = 5,
    color = "darkblue", fill = "lightblue") +
  ggtitle("Histogram of SUS Score") +
  xlab("SUS Score") + ylab("Count") +
  theme_bw()
Scores |>
  filter(!is.na(RTLX.Score)) |>
  ggplot(aes(RTLX.Score)) +
  geom_histogram(aes(, after_stat(count)),
    binwidth = 5, boundary = 5,
    color = "darkgreen", fill = "lightgreen") +
  ggtitle("Histogram of RTLX Score") +
  xlab("RTLX Score") + ylab("Count") +
  theme_bw()
Scores |>
  filter(!is.na(SUS.Score)) |>
  ggplot(aes(, SUS.Score)) +
  geom_boxplot(
    color = "darkblue", fill= "lightblue") +
  ggtitle("Boxplot of SUS Score") +
  ylab("SUS Score") +
  theme_bw()
Scores |>
  filter(!is.na(RTLX.Score)) |>
  ggplot(aes(, RTLX.Score)) +
  geom_boxplot(
    color = "darkgreen", fill= "lightgreen") +
  ggtitle("Boxplot of RTLX Score") +
  ylab("RTLX Score") +
  theme_bw()
Scores |>
  filter(!is.na(SUS.Score) & !is.na(RTLX.Score)) |>
  ggplot(aes(RTLX.Score, SUS.Score)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm",
    color = "royalblue", fill = "skyblue", alpha = 0.5) +
  ggtitle("Plot of SUS Score and RTLX Score") +
  xlab("RTLX Score") + ylab("SUS Score") +
  theme_bw()
Scores |>
  filter(!is.na(SUS.Score) & !is.na(RTLX.Score)) |>
  with(cor.test(RTLX.Score, SUS.Score))
Scores |>
  filter(!is.na(SUS.Score) & !is.na(RTLX.Score)) |>
  with(lm(SUS.Score ~ RTLX.Score)) |>
  summary()

