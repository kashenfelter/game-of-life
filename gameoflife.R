library(gganimate)
library(dplyr)
library(ggforce)
library(ggplot2)
library(tidyr)

set.seed(101)

n <- 40 # number of rows
m <- 64 # number of columns

df <- data.frame(row = rep(1:n, times  = m), col = rep(1:m, each = n)) %>%
  rowwise() %>%
  mutate(state = ifelse(runif(1) < 0.5, 0, 1))

K <- 200 # number of frames

for (k in 4:K) {
  for (i in 1:nrow(df)) {
    row_i <- df$row[i]
    col_i <- df$col[i]
    state_i <- df[i, k-1]
    state_new <- state_i
    temp <- df %>% filter((row == (row_i-1) | row == row_i | (row == row_i+1)) &
                            (col == (col_i-1) | col == col_i | (col == col_i+1)) &
                            !((row == row_i) & (col == col_i))) %>%
      replace(is.na(.), 0)
    live_neighbours <- sum(temp[, k-1])
    # Any live cell with fewer than two live neighbours dies, as if caused by under-population
    if ((state_i > 0) & (live_neighbours < 2)) {
      state_new <- 0
      # Any live cell with two or three live neighbours lives on to the next generation
    } else if ((state_i > 0) & ((live_neighbours == 2) | (live_neighbours == 3))) {
      state_new <- 1
      # Any live cell with more than three live neighbours dies, as if by over-population
    } else if ((state_i > 0) & (live_neighbours > 3)) {
      state_new <- 0
      # Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction
    } else if ((state_i < 1) & (live_neighbours == 3)) {
      state_new <- 1
    }
    
    df[i, k] <- state_new
    print(k)
  }
}

names(df) <- c("row", "col", sprintf("%03s", 1:(ncol(df)-2)))

df <- df %>%
  gather(frame, state, 3:ncol(.))

p <- ggplot(df[1:(nrow(df)*0.75), ]) +
  geom_tile(aes(col, row, width = 1, height = 1,
                fill = factor(state), frame = frame), colour = "white", size = 0.1) +
  coord_equal() +
  scale_fill_manual(name = NULL, values = c("0" = "lightgray", "1" = "black"), labels = c("dead", "alive")) +
  theme(panel.border = element_blank(), legend.position = "none") +
  theme(panel.background=element_rect(fill = "transparent",colour = NA)) +
  theme(plot.background=element_rect(fill = "transparent",colour = NA)) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(plot.margin=unit(c(0,0,0,0), "null")) +
  theme(panel.margin=unit(c(0,0,0,0), "null")) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  theme(axis.ticks.length=unit(0, "null")) +
  theme(legend.margin=unit(0, "null"))

gg_animate(p, "gif/gameoflife.gif", interval = 0.1, title_frame = FALSE, ani.width=800*0.525, ani.height=500*0.525)
