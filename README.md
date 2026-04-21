# first-move-advantage-chess-
BSDS 1ST YEAR STUDENT WORKING ON GROUP PROJECT OF DATA ANALYTICS IN R AND PYTHON
# Mayank Kumar Sahu
# ============================================================
# Lichess Chess Analysis - All 4 Research Questions
# ============================================================

library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)

# ------------------------------------------------------------
# Load Data
# ------------------------------------------------------------
df <- read.csv("/Users/mayankkumarsahu/lichess_jan2013_features_clean.csv")

# ------------------------------------------------------------
# Feature Engineering
# ------------------------------------------------------------
df <- df %>%
  mutate(
    EloDiff = WhiteElo - BlackElo,
    AbsEloDiff = abs(EloDiff),
    AvgElo = (WhiteElo + BlackElo) / 2,
    
    WhiteWin = as.integer(Result == "W"),
    Draw = as.integer(Result == "D"),
    
    FavoredWin = case_when(
      EloDiff > 0 ~ as.numeric(Result == "W"),
      EloDiff < 0 ~ as.numeric(Result == "L"),
      TRUE ~ NA_real_
    ),
    
    EloDiffBin = cut(
      AbsEloDiff,
      breaks = c(0,50,100,200,400,Inf),
      labels = c("0-50","50-100","100-200","200-400","400+"),
      right = FALSE
    ),
    
    EloBin = cut(
      AvgElo,
      breaks = c(0,1200,1400,1600,1800,2100,Inf),
      labels = c("<1200","1200-1400","1400-1600",
                 "1600-1800","1800-2100","2100+")
    )
  )

# ============================================================
# Q1 — Do openings reduce the impact of Elo difference?
# ============================================================

top_openings <- df %>%
  count(Opening, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(Opening)

q1 <- df %>%
  filter(Opening %in% top_openings, !is.na(FavoredWin)) %>%
  group_by(Opening) %>%
  summarise(
    Games = n(),
    EloImpact = cor(AbsEloDiff, FavoredWin, use = "complete.obs"),
    FavoredWinRate = mean(FavoredWin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(EloImpact) %>%
  mutate(Opening = fct_reorder(Opening, EloImpact))

scale_factor <- max(q1$EloImpact) / max(q1$FavoredWinRate)

ggplot(q1, aes(x = Opening)) +
  geom_col(aes(y = EloImpact), fill = "#3B82F6", width = 0.7) +
  geom_line(aes(y = FavoredWinRate * scale_factor, group = 1),
            color = "darkgreen", linewidth = 1.2) +
  geom_point(aes(y = FavoredWinRate * scale_factor),
             color = "darkgreen", size = 3) +
  scale_y_continuous(
    name = "Impact of Elo Difference",
    sec.axis = sec_axis(~./scale_factor,
                        name = "Favored Player Win Rate",
                        labels = percent)
  ) +
  labs(
    title = "Q1: Do Certain Openings Reduce the Impact of Elo Difference?",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ============================================================
# Q2 — Are some openings associated with longer or shorter games?
# ============================================================

q2 <- df %>%
  filter(Opening %in% top_openings) %>%
  group_by(Opening) %>%
  summarise(
    AvgMoves = mean(MovesPlayed, na.rm = TRUE),
    Games = n(),
    .groups = "drop"
  ) %>%
  arrange(AvgMoves) %>%
  mutate(Opening = fct_reorder(Opening, AvgMoves))

ggplot(q2, aes(x = Opening, y = AvgMoves)) +
  geom_col(fill = "#E76F51") +
  coord_flip() +
  labs(
    title = "Q2: Game Length by Opening",
    subtitle = "Aggressive openings tend to end faster",
    x = NULL,
    y = "Average Moves Played"
  ) +
  theme_minimal()


# ============================================================
# Q3 — How does game length relate to competitiveness?
# ============================================================

q3 <- df %>%
  filter(!is.na(EloDiffBin)) %>%
  group_by(EloDiffBin) %>%
  summarise(
    AvgMoves = mean(MovesPlayed, na.rm = TRUE),
    Games = n(),
    .groups = "drop"
  )

ggplot(q3, aes(x = EloDiffBin, y = AvgMoves, fill = EloDiffBin)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(AvgMoves,1)),
            vjust = -0.6,
            fontface = "bold") +
  scale_fill_brewer(palette = "Purples") +
  labs(
    title = "Q3: Elo Gap vs Game Length",
    subtitle = "Close Elo → longer games | Big Elo gap → shorter games",
    x = "Absolute Elo Difference",
    y = "Average Moves Played"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ============================================================
# Q4 — Is chess more predictable at higher Elo levels?
# ============================================================

q4 <- df %>%
  filter(!is.na(FavoredWin), !is.na(EloBin)) %>%
  group_by(EloBin) %>%
  summarise(
    WinRate = mean(FavoredWin),
    Games = n(),
    SE = sqrt(WinRate*(1-WinRate)/Games),
    .groups = "drop"
  )

ggplot(q4, aes(x = EloBin, y = WinRate)) +
  geom_col(fill = "#C9184A", width = 0.7) +
  geom_errorbar(aes(
    ymin = WinRate - 1.96*SE,
    ymax = WinRate + 1.96*SE
  ), width = 0.2) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray40") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Q4: Predictability of Chess by Elo Level",
    subtitle = "Higher Elo → stronger players win more consistently",
    x = "Average Elo Level",
    y = "Win Rate of Higher Rated Player"
  ) +
  theme_minimal()


# ============================================================
# Print Summary Tables
# ============================================================

cat("\n===== Q1 Results =====\n")
print(q1)

cat("\n===== Q2 Results =====\n")
print(q2)

cat("\n===== Q3 Results =====\n")
print(q3)

cat("\n===== Q4 Results =====\n")
print(q4)
