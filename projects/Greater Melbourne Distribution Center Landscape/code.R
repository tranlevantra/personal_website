library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(oz)
library(mapview)



#read excel file
code <- read_excel("/Users/tranlevantra/Desktop/meshblock.xlsx")
code |> filter(STATE_NAME_2021== "Victoria") |> 
  select(MB_CODE_2021, SA1_CODE_2021) -> code


vic_sheets <- c("Table 2", "Table 2.1")

meshblock_count <- map_df(
  vic_sheets,
  ~ read_excel("/Users/tranlevantra/Desktop/Mesh Block Counts, 2021.xlsx", sheet = .x, skip = 6)
)

meshblock_count |> select(MB_CODE_2021, Person) -> meshblock_count

meshblock_count |> str()

SA1_shapefile <- read_sf("/Users/tranlevantra/Desktop/SA1/SA1_2021_AUST_GDA2020.shp")
SA1_shapefile |> filter(STE_NAME21 == "Victoria") |> 
  rename(SA1_CODE_2021 = SA1_CODE21) |>
  select(SA1_CODE_2021) -> SA1_shapefile


SA1_shapefile |> filter(!st_is_empty(geometry)) -> SA1_shapefile
code
meshblock_count
SA1_shapefile

# Join the dataframes
code |> 
  left_join(meshblock_count, by = "MB_CODE_2021") |> 
  group_by(SA1_CODE_2021) |> 
  summarise(Person = sum(Person, na.rm = TRUE)) |> 
  left_join(SA1_shapefile, by = "SA1_CODE_2021") |> 
  st_as_sf() -> SA1_count_shapefile

#using purr to st_sample 
SA1_count_shapefile

SA1_count_shapefile <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/SA1_count_shapefile/SA1_count_shapefile.shp")
points_sf <- read_sf("projects/Greater Melbourne Distribution Center Landscape/resources/SA1_vic_dotcounts/points_sf.shp")

SA1_count_shapefile

SA1_count_shapefile |> 
  mutate(n_dots = floor(Person / 100))  |> 
  filter(n_dots > 0) |> st_sample(size = n_dots)


ggplot() +
  geom_sf(data = SA1_count_shapefile) +
  geom_sf(data = points_sf) +
  theme_minimal() +
  labs(title = "Population Density in Victoria",
       fill = "Population") +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = SA1_count_shapefile, fill = "grey95", color = "white", size = 0.1) +
  geom_sf(data = points_sf, color = "orange", size = 0.3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Population Density in Victoria",
    subtitle = "Each <span style='color:orange'>&#9679;</span> = 100 people",
    caption = "Source: ABS Census Data 2021",
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1)
  )


library(ggplot2)
library(ggtext)  # allows HTML in text elements

ggplot() +
  geom_sf(data = SA1_count_shapefile, fill = "grey95", color = "white", size = 0.1) +
  geom_sf(data = points_sf, color = "orange", size = 0.3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Population Density in Victoria",
    subtitle = "Each <span style='color:orange'>&#9679;</span> = 100 people",
    caption = "Source: ABS Census Data 2021"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 1)
  )

SA1_count_shapefile |>                          # <1>
  mutate(n_dots = floor(Person / 100))  |>     
  filter(n_dots > 0) |>  st_sample(size =SA1_count_shapefile$n_dots) |>  # <2>
  st_sf() -> points_sf


SA1_count_shapefile |> 
  mutate(n_dots = floor(Person / 100))  |>
  filter(n_dots > 0) -> SA1_count_shapefile

st_sample(SA1_count_shapefile, size = SA1_count_shapefile$n_dots) -> points_sf

st_sample(SA1_count_shapefile[1:10,], size = SA1_count_shapefile$n_dots[1:10]) -> points_sf

points_sf |> st_sf()
