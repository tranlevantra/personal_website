library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(purrr)
library(ggtext)
library(stringr)
library(magick)
library(yardstick)
# Read Data ---------------------------------------------------------------
forest <- read.csv("decision_threshold/forest.csv") |>   filter(X_PartInd_ == 2)
logistics <- read.csv("decision_threshold/logistics.csv")
neuroNet <- read.csv("decision_threshold/neuroNet.csv")
svm <- read.csv("decision_threshold/svm.csv")
gam <- read.csv("decision_threshold/gam.csv")
decision_tree <- read.csv("decision_threshold/decisionTree.csv")


data <- logistics |>
  select(P_BAD1, BAD) |>
  rename(Observed = BAD, Probability = P_BAD1) |>
  group_by(Observed) |>
  sample_n(size = ifelse(first(Observed) == "1", 1, 9), replace = TRUE) |> 
  ungroup()

# The classicial threshold is 0.5
  

# data |> write_csv("decision_threshold/data.csv")

data <- read_csv("decision_threshold/data.csv")

labels <- data.frame(color = c("TP", "FP", "TN", "FN"))






# Decision Threshold ------------------------------------------------------
threshold <- 0.5

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
    # Remove unnecessary columns and set names to category
    pull(Color) %>%
    setNames(data$color)
}

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
  # i want to suffle the order of the rows
  # sample_n(size = nrow(.), replace = FALSE) |>

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
         index = color) |> 
  separate(index, into = c("x", "y"), sep = 1) |> 
  mutate(
    x = case_when(
      x == "T" ~ "1",
      x == "F" ~ "0"
    ),
    y = case_when(
      y == "P" ~ "1",
      y == "N" ~ "0"
    )
  )


color_vector <- generateColorVector(cf_plot_data)

calibration_plot_data <- plot_data |> 
  mutate(color = as.factor(Observed)) |>
  pivot_longer(cols = -c(x, decision, color), 
               names_to = "shape", 
               values_to = "y") |> 
  select(x, y, decision, shape, color)

auc_plot_data <- roc_auc(data, truth = as.factor(Observed), estimate = Probability)

auc_plot_data <- data |> 
  mutate(
    Observed = as.factor(Observed),
    Probability = as.numeric(Probability)
  ) |>
  roc_curve(Observed, Probability) 

auc_plot_data

auc_plot_data |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#1f78b4") +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  coord_equal()

calibration_plot_data |> 
  ggplot(aes(x = x, y = y, shape = shape, group = x)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1),
                     labels = c("True Class Labels", "Corresponding Predicted Probabilities")) +
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

acr_plot_data

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


tt_p <- tt_plot_data |> 
  ggplot(aes(x = x, y = y, color = color, shape = shape, group = x)) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(19, 1),
                     labels = c("Reference", "Probability")) +
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
    legend.key = element_rect(colour = "black")  # Adds a border to the legend keys
  )


tt_p/cf_p
  
  


# Function to create performance plot -------------------------------------





# Slider ------------------------------------------------------------------

library(shiny)

sliderInput("threshold", "Decision Threshold:",
            min = 0, max = 1, value = 0.5, step = 0.01)

# Plot with dynamic threshold
renderPlot({
  plot_model_performance(data, input$threshold)
})


# gif ---------------------------------------------------------------------


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
    # Remove unnecessary columns and set names to category
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
           index = color) |> 
    separate(index, into = c("x", "y"), sep = 1) |> 
    mutate(
      x = case_when(
        x == "T" ~ "1",
        x == "F" ~ "0"
      ),
      y = case_when(
        y == "P" ~ "1",
        y == "N" ~ "0"
      )
    )
  
  color_vector <- generateColorVector(cf_plot_data)
  
  tt_p <- tt_plot_data |> 
    ggplot(aes(x = x, y = y, color = color, shape = shape, group = x)) +
    geom_point(size = 2) +
    scale_shape_manual(values = c(19, 1),
                       labels = c("Reference", "Probability")) +
    scale_color_manual(values = color_vector,
                       guide = "none") +
    geom_line(linetype = "dashed") +
    geom_hline(yintercept = threshold, color = "red") +
    labs(title = "Relibility Diagram",
         subtitle = str_c("with <span style='color:red;'>decision threshold</span>"),
         x = "Observations",
         y = "Probability",
         color = "",
         shape = "") +
    theme(
      plot.title = element_markdown(),
      plot.subtitle = element_markdown()
    )
  
  cf_p <- cf_plot_data |> 
    # fill is the cross product of the two columns Reference and Prediction
    ggplot(aes(x = x, y = y, fill = color)) +
    geom_tile(color = "white") +
    geom_text(aes(label = frequency), vjust = 1.5, color = "black", size = 5) +
    scale_fill_manual(values = color_vector) +
    labs(title = "Confusion Matrix",
         x = "Actual Label",
         y = "Predicted Label",
         fill = "")+
    theme(
      legend.key = element_rect(colour = "black")  # Adds a border to the legend keys
    )
  
  
  tt_p/cf_p
}

create_performance_plot(data, 0.5)

# === Step 1: Create thresholds from 0 to 1 in steps of 0.01 ===
thresholds1 <- seq(0, 0.25, by = 0.005)
thresholds2 <- seq(0.25, 1, by = 0.01)

create_performance_plot(data, 0.5)

#Add more thresholds for more frames
thresholds <- c(thresholds1, thresholds2)

# === Step 2: Create directory for temporary frame images ===
dir.create("decision_threshold/frames", showWarnings = FALSE)

# === Step 3: Generate and save frames ===
image_paths <- map_chr(seq_along(thresholds), function(i) {
  threshold <- thresholds[i]
  
  # Generate plot
  p <- create_performance_plot(data, threshold)
  
  # Save frame with padded name
  path <- sprintf("decision_threshold/frames/frame_%03d.png", i)
  ggsave(path, plot = p, width = 8, height = 6, dpi = 150)
  path
})
# === Step 4: Create GIF using ImageMagick ===
# Read all frames
frames <- image_read(image_paths)
gif <- image_animate(frames, fps = 10)
image_write(gif, "decision_threshold/threshold_animation.gif")
# === Step 5: Clean up temporary frames ===
file.remove(image_paths)
unlink("decision_threshold/frames", recursive = TRUE)



