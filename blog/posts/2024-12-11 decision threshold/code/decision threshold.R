library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(ggtext)
library(stringr)

# Figure 2 ----------------------------------------------------------------
threshold <- 0.5

logistics <- read.csv("~/Documents/Github/personal_website/blog/posts/2024-12-11 decision threshold/code/logistics.csv")

data <- logistics |>
  select(P_BAD1, BAD) |>
  rename(Observed = BAD, Probability = P_BAD1) |>
  group_by(Observed) |>
  sample_n(size = ifelse(first(Observed) == "1", 1, 9), replace = TRUE) |> 
  ungroup()


plot_data <- data |> 
  mutate(decision = ifelse(Probability >= threshold, 1, 0)) |> 
  arrange(Observed) |> 
  mutate(x = row_number(),
         color = case_when(
           Observed == 1 & decision == 1 ~ "TP",
           Observed == 0 & decision == 1 ~ "FP",
           Observed == 1 & decision == 0 ~ "FN",
           Observed == 0 & decision == 0 ~ "TN")
  )


acr_plot_data <- plot_data |> 
  mutate(color = as.factor(Observed)) |>
  pivot_longer(cols = -c(x, decision, color), 
               names_to = "shape", 
               values_to = "y") |> 
  mutate(shape = case_when(
    y == 0 ~ "1",
    y == 1 ~ "0",
    y > 0 & y < 1 ~ "2" ))|>
  select(x, y, decision, shape, color)


acr_plot_data |> 
  ggplot(aes(x = x, y = y, shape = shape, group = x)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(19, 15, 0)) +
  geom_line(aes(color = color), linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("black", "red"),
                     labels = c("Correctly Predicted", "Incorrectly Predicted")) +
  geom_hline(yintercept = threshold, color = "red", linewidth = 1) +
  labs(title = "Model Performance on Test Data",
       subtitle = "with Threshold = 0.5",
       x = "Observations",
       y = "Probability",
       color = "",
       shape = "") +
  guides(shape = "none") +
  scale_x_continuous(breaks = 1:10) +
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 12)
  )




# Figure 1 ----------------------------------------------------------------
data <- read.csv("~/Documents/Github/personal_website/blog/posts/2024-12-11 decision threshold/code/data.csv")

plot_data <- data |> 
  mutate(decision = ifelse(Probability >= threshold, 1, 0)) |> 
  arrange(Observed) |> 
  mutate(x = row_number(),
         color = case_when(
           Observed == 1 & decision == 1 ~ "TP",
           Observed == 0 & decision == 1 ~ "FP",
           Observed == 1 & decision == 0 ~ "FN",
           Observed == 0 & decision == 0 ~ "TN")
  )

calibration_plot_data <- plot_data |> 
  mutate(color = as.factor(Observed)) |>
  pivot_longer(cols = -c(x, decision, color), 
               names_to = "shape", 
               values_to = "y") |> 
  select(x, y, decision, shape, color)


calibration_plot_data |> 
  ggplot(aes(x = x, y = y, shape = shape, group = x)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1),
                     labels = c("True Class Labels", "Predicted Probabilities")) +
  geom_line(linetype = "dashed") +
  labs(title = "Model Performance on Test Data",
       subtitle = "with Probability Outputs",
       x = "Observations",
       y = "Probability",
       color = "",
       shape = "") +
  theme(
    legend.key = element_rect(colour = "black"),
    legend.position =  "bottom" 
  )


# Figure 3 ----------------------------------------------------------------
library(yardstick)

auc_plot_data <- data |> 
  mutate(
    Observed = as.factor(Observed),
    Probability = as.numeric(Probability)
  ) |>
  roc_curve(Observed, Probability) 

auc_plot_data |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#1f78b4") +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  coord_equal()

# Figure 4 ----------------------------------------------------------------
library(patchwork)

labels <- data.frame(color = c("TP", "FP", "TN", "FN"))

generateColorVector <- function(data) {
  
  max_frequency <- max(data$frequency, na.rm = TRUE)
  # Define base colors for True and False outcomes
  base_colors <- c("True" = "#0571b0", "False" = "#ca0020")
  
  data %>%
    mutate(
      # Determine base color based on the category (assuming 'T' in category means True)
      BaseColor = if_else(grepl("T", color), base_colors["True"], base_colors["False"]),
      
      # Create a gradient palette from white to the base color and select the appropriate index
      ColorIndex = as.integer(100 * frequency / max_frequency),
      Color = map2_chr(BaseColor, ColorIndex, ~ {
        color_palette <- colorRampPalette(c("white", .x))(100)
        color_palette[max(1, min(.y, 100))]
      })
    ) %>%
    pull(Color) %>%
    setNames(data$color)
}

create_performance_plot <- function(data, threshold) {
  
  plot_data <- data |> 
    mutate(decision = ifelse(Probability >= threshold, 1, 0)) |> 
    arrange(Observed) |> 
    mutate(x = row_number(),
           color = case_when(
             Observed == 1 & decision == 1 ~ "TP",
             Observed == 0 & decision == 1 ~ "FP",
             Observed == 1 & decision == 0 ~ "FN",
             Observed == 0 & decision == 0 ~ "TN")
    )
  
  
  tt_plot_data <- plot_data |> 
    pivot_longer(cols = -c(x, decision, color), 
                 names_to = "shape", 
                 values_to = "y") |>
    select(x, y, decision, shape, color)
  
  
  cf_plot_data <- plot_data |> 
    group_by(color) |>
    summarise(frequency = n()) |> 
    full_join(labels, by = "color") |> 
    mutate(frequency = ifelse(is.na(frequency), 0, frequency),
           x = case_when(color == "TP" ~ "1",
                         color == "FP" ~ "0",
                         color == "TN" ~ "0",
                         color == "FN" ~ "1"),
           y = case_when(color == "TP" ~ "1",
                         color == "FP" ~ "1",
                         color == "TN" ~ "0",
                         color == "FN" ~ "0")
    )
  
  color_vector <- generateColorVector(cf_plot_data)
  
  tt_p <- tt_plot_data |> 
    ggplot(aes(x = x, y = y, color = color, shape = shape, group = x)) +
    geom_point(size = 2) +
    scale_shape_manual(values = c(19, 1),
                       # no legend displayed
                       guide = "none") +
    scale_color_manual(values = color_vector,
                       guide = "none") +
    geom_line(linetype = "dashed") +
    geom_hline(yintercept = threshold, color = "red") +
    labs(title = "Model Performance on Test Data",
         subtitle = "Decision Threshold and Model's Probability Output",
         x = "Observations",
         y = "Probability",
         color = "",
         shape = "")

  
  cf_p <- cf_plot_data |> 
    # fill is the cross product of the two columns Reference and Prediction
    ggplot(aes(x = x, y = y, fill = color)) +
    geom_tile(color = "white") +
    geom_text(aes(label = frequency), vjust = 1.5, color = "black", size = 5) +
    scale_fill_manual(values = color_vector) +
    labs(title = "",
         subtitle = "Confusion Matrix",
         x = "Actual Label",
         y = "Predicted Label",
         fill = "")+
    theme(
      legend.position = "bottom",
      legend.key = element_rect(colour = "black") 
    )
  
  return(tt_p/cf_p)
}

create_performance_plot(data, 0.1)

# gif ---------------------------------------------------------------------
# === Step 1: Create thresholds from 0 to 1
thresholds1 <- seq(1, 0.25, by = -0.01)
thresholds2 <- seq(0.25, 0, by = -0.003)
thresholds <- c(thresholds1, thresholds2)


# === Step 2: Create directory for temporary frame images ===
dir.create("blog/posts/2024-12-11 decision threshold/code/frames", showWarnings = FALSE)

# === Step 3: Generate and save frames ===
image_paths <- map_chr(seq_along(thresholds), function(i) {
  threshold <- thresholds[i]
  
  # Generate plot
  p <- create_performance_plot(plot_data, threshold)
  
  # Save frame with padded name
  path <- sprintf("blog/posts/2024-12-11 decision threshold/code/frames/frame_%03d.png", i)
  ggsave(path, plot = p, width = 8, height = 6, dpi = 150)
  path
})
# === Step 4: Create GIF using ImageMagick ===
library(magick)

frames <- image_read(image_paths)
gif <- image_animate(frames, fps = 10)
image_write(gif, "blog/posts/2024-12-11 decision threshold/figures/threshold_animation.gif")

# # === Step 5: Clean up temporary frames ===
# file.remove(image_paths)
# unlink("blog/posts/2024-12-11 decision threshold/code/frames", recursive = TRUE)



