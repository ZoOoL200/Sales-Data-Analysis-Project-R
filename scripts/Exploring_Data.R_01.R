

# =========================================================
# Exploratory Data Analysis Script
# Project: Retail Sales Analysis
# Author: Ahmed Rabie
# =========================================================

# Load Libraries -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ggtext)
library(patchwork)
library(lubridate)

if (!dir.exists("./figures")) {
  dir.create("./figures")
}

# Paths --------------------------------------------------------------------

data_path <- "./data/processed/data_cleaned.rds"
figures_path <- "./figures/"

if (!dir.exists(figures_path)) {
  dir.create(figures_path)
}


# Import Data --------------------------------------------------------------

data <- readRDS(data_path)

glimpse(data)
head(data)


# KPIs ---------------------------------------------------------------------

#Total Sales
total_sales <- sum(data$sales)
#Average of Sales
average_sales <- mean(data$sales)
#Total Orders
total_orders <- nrow(data)

kpi_table <- tibble(
  Metric = c("Total Sales", "Average Sales", "Total Orders"),
  Value = c(total_sales, average_sales, total_orders)
)

kpi_table


# EDA by States ---------------------------------------------------------

sales_by_state <- data %>%
  group_by(state) %>%
  summarise(
    total_sales = sum(sales),
    total_orders = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

# Plot Top 5 States highest Sales
sales_by_state %>%
  mutate(label_text = paste0(
    round(total_sales / 1000, 1)
    ,
    "K\n (",
    comma(total_orders),
    " orders)"
  )) %>%
  slice_head(n = 5) %>%
  
  ggplot(aes(x = reorder(state , -total_sales), y = total_sales / 1000)) +
  
  geom_col(fill = "#4e83b4", width = 0.7) +
  geom_text(
    aes(label = label_text),
    vjust = -0.3,
    lineheight = 0.9,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    limits = c(0, max(sales_by_state$total_sales / 1000) * 1.1),
    breaks = pretty_breaks(n = 5),
    labels = unit_format(unit = "K", sep = "")
  ) +
  
  labs(
    title = "Market Leadership: Top 5 States by Total Sales",
    subtitle = "California leads the market with $446.3K in total sales and the
    highest volume of 1,946 orders, followed by New York and Texas. 
    The significant order volume in New York compared to Texas suggests 
    a stronger demand for mid-range products in that state.",
    x = NULL,
    y = "Total Sales (K)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 15, t = 5)
    ),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(color = "#474646" , face = "plain"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10 , color = "black"),
    axis.title.y = element_text(
      margin = margin(t = 0,r = 0.25,b = 0,l = 0,"cm"))
  )

# Save plot
ggsave(
  filename = paste0(figures_path, "top_states_sales.png"),
  width = 15,
  height = 11.25,
  units = "cm",
  dpi = 300
)


# EDA by Region -----------------------------------------------------------

#Sales by region
sales_by_region <- data %>% group_by(region) %>%
  summarise(total_sales = sum(sales), total_orders = n()) %>%
  arrange(desc(total_sales))

# Plot regions sales (bars plot)
sales_by_region %>%
  
  mutate(label_text = paste0(
    round(total_sales / 1000, 1)
    ,
    "K\n (",
    comma(total_orders),
    " orders)"
  )) %>%
  slice_head(n = 5) %>%
  
  ggplot(aes(x = reorder(region , -total_sales) , y = total_sales / 1000)) +
  
  geom_col(fill = "#4e83b4", width = 0.7)  +
  geom_text(
    aes(label = label_text),
    vjust = -0.3,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    limits = c(0, max(sales_by_region$total_sales / 1000) * 1.1),
    breaks = pretty_breaks(n = 5),
    labels = unit_format(unit = "K", sep = "")
  ) +
  
  labs(
    title = "Regional Sales Performance & Order Volume",
    subtitle = "The West region leads the market with $710.2K in total sales
      and the highest volume of 3,140 orders, followed closely by the East.
      While the Central region shows moderate performance, the South records
      the lowest figures in both revenue and orders. This data indicates
      a strong correlation between order frequency and total revenue across all
      geographical segments.",
    x = NULL,
    y = "Total Sales (K)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 15, t = 5)
    ),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(color = "#474646" , face = "plain"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10 , color = "black"),
    axis.title.y = element_text(
      margin = margin(t = 0,r = 0.25,b = 0,l = 0,"cm"))
  )

ggsave(
  filename = paste0(figures_path, "sales_by_region.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300
)


# Total Sales during time plot

p1 <- data %>%
  
  mutate(month_year = floor_date(order_date, "month")) %>%
  
  group_by(month_year, region) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE),
            .groups = "drop") %>%
  
  ggplot(aes(x = month_year, y = total_sales / 1000, color = region)) +
  
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
  scale_y_continuous(labels = scales::unit_format(unit = "K", sep = ""),
                     breaks = pretty_breaks(n = 5)) +
  
  scale_color_manual(
    values = c(
      "West"    = "#2E5B88",
      "East"    = "#E67E22",
      "Central" = "#27AE60",
      "South"   = "#C0392B"  
    )
  ) +
  
  labs(title = "Monthly Sales Trends", x = NULL, y = "Total Sales (K)") +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(
          t = 0,
          r = 0.25,
          b = 0,
          l = 0,
          "cm"
        )))




# No.Orders during time plot
p2 <- data %>%
  
  mutate(month_year = floor_date(order_date, "month")) %>%
  
  group_by(month_year, region) %>%
  summarise(total_order = n() , .groups = "drop") %>%
  
  ggplot(aes(x = month_year, y = total_order , color = region)) +
  
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_color_manual(
    values = c(
      "West"    = "#2E5B88",
      "East"    = "#E67E22",
      "Central" = "#27AE60",
      "South"   = "#C0392B"
    )
  ) +
  
  labs(title = "Monthly Order Trends", x = NULL, y = "No.Orders") +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(
          t = 0,
          r = 0.25,
          b = 0,
          l = 0,
          "cm"
        )))


# Plot time series
combined_plot <- p1 / p2

combined_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Market Growth Dynamics: Regional Revenue & Demand Trends",
    subtitle = "Regional trends show a strong seasonal cyclicality with a
        significant growth surge toward late 2018. While the East and West
        frequently swap leads in order volume, the West secured a dominant lead
        in total sales due to intermittent revenue drops in the East. This
        divergence confirms the West's superior Average Order Value (AOV) and
        its more resilient growth trajectory compared to other regions.",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_textbox_simple(
        size = 10,
        color = "grey30",
        margin = margin(b = 15, t = 5)
      )
    )
  ) &
  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_blank()
  ) &
  guides(color = guide_legend(nrow = 1, byrow = TRUE))


ggsave(
  filename = paste0(figures_path, "region_time_plot.png"),
  width = 25,
  height = 20,
  units = "cm",
  dpi = 300
)


# EDA Segments  -----------------------------------------------------------

data %>% group_by(segment) %>%
  summarise(total_sales = sum(sales)) %>%
  mutate(perc = total_sales / sum(total_sales),
         label = scales::percent(perc, accuracy = 0.1)) %>%
  
  ggplot(aes(x = "" , total_sales, fill = segment)) +
  
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4.5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = c("#BFD7EA", "#4F8CC9", "#2F5D9B")) +
  
  labs(title = "Customer Base Distribution by Segment",
       subtitle = "Consumer segment drives over 50% of the total revenue,
        followed by Corporate and Home Office.") +
  
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      face = "bold",
      size = 14,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 20, t = 5),
      lineheight = 1.4
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20) ,
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename =  paste0(figures_path, "segemnt_distribution_plot.png"),
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)


# EDA Category ----------------------------------------------------------

data %>% group_by(category) %>%
  summarise(total_sales = sum(sales), .groups = "drop") %>%
  mutate(perc = total_sales / sum(total_sales),
         label = scales::percent(perc, accuracy = 0.1)) %>%
  
  ggplot(aes(x = "" , total_sales, fill = category)) +
  
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4.5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = c("#BFD7EA", "#4F8CC9", "#2F5D9B")) +
  
  labs(
    title = "Sales Distribution by Category",
    subtitle = "A breakdown of total revenue across primary product
        categories to identify key performance drivers."
  ) +
  
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      face = "bold",
      size = 14,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 20, t = 5),
      lineheight = 1.4
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20) ,
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename =  paste0(figures_path, "category_distribution_plot.png"),
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)


# EDA Sub-category --------------------------------------------------------

sales_by_sub_category <- data %>%
  group_by(sub_category) %>%
  summarise(total_sales = sum(sales) , total_orders = n()) %>%
  arrange(desc(total_orders))

# Plot Top 5 sub-category ordered
sales_by_sub_category %>%
  mutate(label_text = paste0(
    comma(total_orders),
    " orders (",
    round(total_sales / 1000, 1),
    " K)"
  )) %>%
  slice_head(n = 7) %>%
  
  ggplot(aes(y = reorder(sub_category , total_orders) , x = total_sales /
               1000)) +
  
  geom_col(fill = "#4e83b4", width = 0.7)  +
  geom_text(
    aes(label = label_text),
    hjust = -0.01,
    lineheight = 1.1,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    limits = c(0, max(sales_by_sub_category$total_sales / 1000) * 1.1),
    breaks = pretty_breaks(n = 5),
    labels = unit_format(unit = "K", sep = "")
  ) +
  
  labs(
    title = "Market Demand & Revenue Leaders",
    subtitle = "Sub-categories ranked by transaction volume, with bar lengths 
    representing total sales revenue.",
    y = NULL,
    x = "Total Sales (K)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 15, t = 5)
    ),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(color = "#474646" , face = "plain"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10 , color = "black"),
    axis.title.x = element_text(
      margin = margin(t = 0.25,r = 0,b = 0,l = 0,"cm"))
  )

# Save plot
ggsave(
  filename =  paste0(figures_path, "top_7_sub_category_plot.png"),
  width = 30,
  height = 22.5,
  units = "cm",
  dpi = 300
)


# EDA ship mode ------------------------------------------------------------


ship_mode_orders <- data %>% group_by(ship_mode) %>%
  summarise(total_orders = n()) %>%
  arrange(desc(total_orders))

# Plot Top 5 sub-category ordered
ship_mode_orders %>%
  mutate(label_text = paste0(
    comma(total_orders)," orders")) %>%
  
  ggplot(aes(y = reorder(ship_mode , total_orders) , x = total_orders)) +
  
  geom_col(fill = "#4e83b4", width = 0.7)  +
  geom_text(
    aes(label = label_text),
    hjust = -0.1,
    lineheight = 1,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    limits = c(0, max(ship_mode_orders$total_orders) * 1.1),
    breaks = pretty_breaks(n = 5)) +
  
  labs(
    title = "Ship Mode Performance Analysis",
    subtitle = "Standard Class remains the dominant shipping method,
        significantly outpacing other modes in both transaction frequency.",
    y = NULL,
    x = "No.Orders"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox_simple(
      size = 10,
      color = "grey30",
      margin = margin(b = 15, t = 5)
    ),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(color = "#474646" , face = "plain"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10 , color = "black"),
    axis.title.x = element_text(
      margin = margin(t = 0,r = 0.25,b = 0,l = 0,"cm"))
  )

# Save plot
ggsave(
  filename =  paste0(figures_path, "ship_mode_distribution_plot.png"),
  width = 30,
  height = 22.5,
  units = "cm",
  dpi = 300
)


# delivery analysis

data %>%
  mutate(delivery_days = as.numeric(ship_date - order_date)) %>%
  group_by(ship_mode) %>%
  
  ggplot(aes(
    x = reorder(ship_mode, delivery_days, FUN = median),
    y = delivery_days,
    fill = ship_mode
  )) +
  
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1,
              alpha = 0.1,
              color = "#2c3e50") +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Shipping Efficiency: Delivery Lead Time",
    subtitle = "While 'Same Day' shipping consistently meets near-zero
        day targets, 'Standard Class' shows a wider variance with delivery
        times ranging between 4 to 6 days.",
    x = "Shipping Mode",
    y = "Days to Ship",
    fill = "Ship Mode"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox_simple(size = 10, color = "grey40"),
    panel.grid.minor = element_blank(),
    axis.title.y  = element_text(margin =  margin(0, 0.5, 0, 0, "cm")) ,
    axis.title.x  = element_text(margin =  margin(0.5, 0, 0, 0, "cm")),
    axis.text = element_text(
      size = 10 ,
      color = "black" ,
      face = "bold"
    )
  )

# Save plot
ggsave(
  filename =  paste0(figures_path, "delivery_distribution_plot.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300
)
