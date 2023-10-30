library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(tidyr)
library(warwickplots)
library(viridis)

aubergine <- warwick_palettes$aubergine[1]
dark_text <- "#2e2e2f"
mid_text <-  "#4d4e4f"
light_text <- "#747576"
pale_text <- "#ebebeb"

aubergine_pal <- warwickplots::warwick_palettes$aubergine


# overall -----------------------------------------------------------------

# read in and split out status counts
review <- read_csv("data/review.csv") |>
    filter(Type != "Not taken up" & !Area %in% c("Translation")) |>
    separate_longer_delim(`Status end sprint`, delim = ";") |>
    separate_wider_regex(`Status end sprint`,
                         patterns = c(" *", n = "[0-9]+", " ",
                                      Status = ".*"))

# simplify categories and total within area
review <- review  |>
    mutate(Status = case_when(
        Status %in% c("closed fixed", "closed won't fix", "patch accepted") ~
            "closed",
        Status %in% c("new tests", "proposed patch") ~ "proposed patch",
        Status %in% c("running tests", "wip") ~ "work in progress",
        .default = Status),
        Status = factor(Status, levels = rev(c("discussion", "roadmap",
                                           "work in progress", "proposed patch",
                                           "closed")))) |>
    group_by(Area, Status) |>
    summarize(n = sum(as.numeric(n))) |> ungroup()


# vertical
review |>
    ggplot(aes(fill = Status, y = n, x = fct_infreq(Area, w = n))) +
    geom_col() +
    scale_fill_viridis(discrete = TRUE, option = "plasma") +
    scale_x_discrete(limits = rev) +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    theme_minimal(base_size = 36) +
    theme(axis.text.y = element_text(colour = mid_text, size = rel(1)),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

#ggsave("bug_review.png", path = here::here("figures"), device = "png", dpi = 320)


# horizontal
review |>
    ggplot(aes(fill = Status, y = n, x = fct_infreq(Area, w = n))) +
    geom_col() +
    scale_fill_viridis(discrete = TRUE, option = "plasma") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 36) +
    theme(axis.text.y = element_text(colour = mid_text, size = rel(1)),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

ggsave("bug_review.png", path = here::here("figures"), device = "png",
       width = 20, height = 8.82, units = "in", dpi = 320)

# Documentation -----------------------------------------------------------

area <- unique(review$Area)

for (a in area) {
    review |>
        filter(Area == a) |>
        ggplot(aes(fill = Status, y = n, x = Status)) +
        geom_col() +
        scale_fill_viridis(discrete = TRUE, option = "plasma") +
        labs(x = NULL, y = NULL) +
        guides(fill = "none") +
        theme_minimal(base_size = 36) +
        theme(axis.text.y = element_text(colour = mid_text, size = rel(1)),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())

    ggsave(paste0("bug_review_", a, ".png"), path = here::here("figures"), device = "png",
           width = 9, height = 9, units = "in", dpi = 320)
}
