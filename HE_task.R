library(tidyverse)
library(ggtext)
library(extrafont)
library(geofacet)
library(scales)
library(cowplot)
library(sf)
library(rnaturalearth)
library(viridis)
library(tidytext)


### FIGURE 1: STUDENT COUNT

student_count <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/chart-1.csv", 
                          skip = 13, col_names = TRUE)

student_count_clean <- student_count %>% 
  mutate("Academic_Year" = as.factor(`Academic Year`)) %>%
  gather("Level", "n", 2:4) %>%
  mutate("Level" = as.factor(Level)) %>% 
  filter(Level == "Postgraduate" | Level == "Undergraduate")
  
  
student_count_clean  %>% 
  ggplot(aes(Academic_Year, n, fill = Level)) +
  coord_flip() + 
  labs(title = "UK Higher Education (HE) intake: level of study",
       x = "", 
       y = "", 
       fill = "") +
  geom_col() + 
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = c("#43A193", "#CF720E"),
                    breaks = c("Undergraduate", "Postgraduate")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Noto Sans", size = 24, hjust = 0.5, margin = margin(20,0,20,0)),
        axis.text.x = element_text(family = "Noto Sans", colour = "grey40"),
        axis.text.y = element_text(family = "Noto Sans", colour = "grey40"),
        legend.title = element_text(family = "Noto Sans"),
        legend.text = element_text(family = "Noto Sans", size = 11))

ggsave("Figure_1.png", plot = last_plot(), dpi = 300)

scc_lm <- student_count_clean %>%
  filter(`Level` %in% "Undergraduate") %>%
  rownames_to_column() %>%
  mutate("rowname" = as.numeric(rowname))

model <- lm(n ~ rowname, data = scc_lm)
predict(model, data.frame(rowname = c(20, 21, 22)), interval = "confidence")

scc_lm_2 <- student_count_clean %>%
  filter(`Level` %in% "Postgraduate") %>%
  rownames_to_column() %>%
  mutate("rowname" = as.numeric(rowname))

model2 <- lm(n ~ rowname, data = scc_lm_2)
predict(model2, data.frame(rowname = c(20, 21, 22)), interval = "confidence")
  
### FIGURE 2: DEGREE BREAKDOWN

degree_breakdown <- read_csv("https://www.hesa.ac.uk/data-and-analysis/sb255/figure-1a.csv",
                             skip = 22, col_names = TRUE)

degree_breakdown %>%
  mutate("Academic_Year" = as.factor(`Academic year`),
         "Level" = as.factor(`Level of study`)) %>%
  ggplot(aes(Academic_Year, Number, group = Level, colour = Level)) +
  geom_line(size = 1) +
  geom_point() + 
  labs(title = "First year HE student enrolments (by level of study)", x = "", y = "Number of Students", colour = "") +
  scale_colour_manual(values = c("#43A193", "#094F45", "#CF720E", "#8C5D2A", "#4F0E43")) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Noto Sans", size = 24, hjust= 0.5, margin = margin(20,0,20,0)),
        axis.text.x = element_text(family = "Noto Sans", colour = "grey40"),
        axis.text.y = element_text(family = "Noto Sans", colour = "grey40"),
        legend.title = element_text(family = "Noto Sans"),
        legend.text = element_text(family = "Noto Sans", size = 12))

ggsave("Figure_2.png", plot = last_plot(), dpi = 300)


### FIGURE 3: DEGREE BREAKDOWN #2


degree_breakdown_2 <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/chart-2.csv",
                 skip = 13, col_names = TRUE) 

degree_breakdown_2_clean <- degree_breakdown_2 %>% 
  mutate("Academic_Year" = as.factor(`Academic Year`)) %>%
  gather("Level", "n", 2:6) %>% 
  mutate("Level" = as.factor(Level)) 

degree_breakdown_2_clean %>%
  filter(!Level %in% "Total") %>%
  ggplot(aes(Academic_Year, n, fill = Level)) +
  coord_flip() +
  labs(title = "UK HE intake (first year students): level of study",
                    x = "", 
                    y = "", 
                    fill = "") +
  geom_col() + 
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = c("#98D4CB", "#43A193", "#F4AE62", "#CF720E"),
                    breaks = c("Undergraduate part-time", "Undergraduate full-time", "Postgraduate part-time", "Postgraduate full-time")) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Noto Sans", size = 24, hjust = 0.5, margin = margin(20,0,20,0)),
        axis.text.x = element_text(family = "Noto Sans", colour = "grey40"),
        axis.text.y = element_text(family = "Noto Sans", colour = "grey40"),
        legend.title = element_text(family = "Noto Sans"),
        legend.text = element_text(family = "Noto Sans", size = 11))

ggsave("Figure_3.png", plot = last_plot(), dpi = 300)  

### Figure 4/5/5B: ENROLMENT BY HR PROVIDER (FIRST YEAR FULL TIME)

enrolment <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/table-1-(2018-19).csv",
                      skip = 14, col_names = TRUE)

enrolment_clean <- enrolment %>%
  select(-UKPRN, -`Academic Year`, - `Country of HE provider`) %>%
  filter(!`Category marker` %in% c("Domicile", "Total")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category marker`) %>%
  mutate(
    `Region of HE provider` = case_when(
      `Region of HE provider` == "Yorkshire and The Humber" ~ "Yorkshire & the Humber",
      TRUE ~ `Region of HE provider`)) %>%
  unique()

new_grid <- uk_regions1 %>%
  mutate(
    row = replace(row, name=="South West", 7),
    row = replace(row, name=="South East", 7),
    row = replace(row, name=="Northern Ireland",3)
  ) %>%
  mutate(
    name = case_when(
      name == "Eastern" ~ "East of England",
      TRUE ~ name
    ))

enrolment_clean %>%
  group_by(.dots=c("`Region of HE provider`", "Category")) %>%
  summarise(total = sum(Number)) %>%
  ggplot(aes(Category, total, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = c("#43A193", "#CF720E", "#F4AE62")) + 
  labs(title = "Enrolment by region and sex (for 2018/19 first year full-time students)",
       x = "", y = "") +
  scale_y_continuous(labels = comma) +
  facet_geo(~`Region of HE provider`, grid = new_grid, label = "name") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,30,0)),
        strip.text = element_text(size = 12, family = "Lato"),
        axis.title.x = element_text(size = 9, family = "Lato"),
        axis.title.y = element_text(size = 9, family = "Lato"),
        legend.position = "none")

ggsave("Figure_4.png", plot = last_plot(), dpi = 300)  

enrolment_clean_soton <- enrolment %>%
  select(-UKPRN, -`Academic Year`, - `Country of HE provider`) %>%
  filter(!`Category marker` %in% c("Domicile", "Total")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  filter(`Level of study` %in% "All") %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category marker`) %>%
  filter(`HE provider` %in% "The University of Southampton") %>%
  unique()
  
ggplot(enrolment_clean_soton, aes(x = "", y = Number, fill = Category)) +
  geom_bar(width = , stat = "identity") + 
  coord_polar("y", start=0) +
  labs(title = "The University of Southampton", fill = "", caption = "Female students: 4725 | Male students: 3550 | Other: 0") +
  scale_fill_manual(values = c("#43A193", "#CF720E", "#F4AE62")) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 28, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.text = element_text(family = "Noto Sans", size = 11)
  )

ggsave("Figure_5.png", plot = last_plot(), dpi = 300)  

enrolment_clean_winch <- enrolment %>%
  select(-UKPRN, -`Academic Year`, - `Country of HE provider`) %>%
  filter(!`Category marker` %in% c("Domicile", "Total")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  filter(`Level of study` %in% "All") %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category marker`) %>%
  filter(`HE provider` %in% "The University of Winchester") %>%
  unique()

a <- ggplot(enrolment_clean_winch, aes(x = "", y = Number, fill = Category)) +
  geom_bar(width = , stat = "identity") + 
  coord_polar("y", start=0) +
  labs(title = "Winchester", fill = "") +
  scale_fill_manual(values = c("#43A193", "#CF720E", "#F4AE62")) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 28, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "none",
    legend.text = element_text(family = "Noto Sans", size = 11)
  )

enrolment_clean_bm <- enrolment %>%
  select(-UKPRN, -`Academic Year`, - `Country of HE provider`) %>%
  filter(!`Category marker` %in% c("Domicile", "Total")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  filter(`Level of study` %in% "All") %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category marker`) %>%
  filter(`HE provider` %in% "Bournemouth University") %>%
  unique()

b <- ggplot(enrolment_clean_bm, aes(x = "", y = Number, fill = Category)) +
  geom_bar(width = , stat = "identity") + 
  coord_polar("y", start=0) +
  labs(title = "Bournemouth", fill = "") +
  scale_fill_manual(values = c("#43A193", "#CF720E", "#F4AE62")) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 28, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "none",
    legend.text = element_text(family = "Noto Sans", size = 11)
  )

enrolment_clean_solent <- enrolment %>%
  select(-UKPRN, -`Academic Year`, - `Country of HE provider`) %>%
  filter(!`Category marker` %in% c("Domicile", "Total")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  filter(`Level of study` %in% "All") %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category marker`) %>%
  filter(`HE provider` %in% "Solent University") %>%
  unique()

c <- ggplot(enrolment_clean_solent, aes(x = "", y = Number, fill = Category)) +
  geom_bar(width = , stat = "identity") + 
  coord_polar("y", start=0) +
  labs(title = "Solent", fill = "") +
  scale_fill_manual(values = c("#43A193", "#CF720E", "#F4AE62")) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 28, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "none",
    legend.text = element_text(family = "Noto Sans", size = 11)
  )

plot_grid(a,b,c, nrow = 1)

ggsave("Figure_5b.png", plot = last_plot(), dpi = 300)  

### Figure 6: STUDENT ENROLMENT: PERSONAL CHARACTERISTICS (FIRST YEAR FULL TIME)

enrolment_personal <- read_csv("https://www.hesa.ac.uk/data-and-analysis/sb255/figure-4.csv",
                      skip = 17, col_names = TRUE)

enrolment_personal_clean <- enrolment_personal %>%
  filter(!`Mode of study` %in% c("All", "Part-time")) %>%
  filter(`Academic Year` %in% c("2018/19")) %>%
  filter(`First year marker` %in% "First year") %>%
  filter(`Level of study` %in% "All") %>%
  filter(!`Category Marker` %in% c("Sex", "Total", "Total UK domiciled students")) %>%
  filter(!`Country of HE provider` %in% "All") %>%
  mutate(
    Category = case_when(
      Category == "30 years and over" ~ "30+",
      TRUE ~ Category
    )) %>%
  mutate("Category_Marker" = as.factor(`Category Marker`),
         "Category" = as.factor(Category),
         "Country" = as.factor(`Country of HE provider`)) %>%
  select(-`First year marker`, -`Level of study`, -`Mode of study`, -`Category Marker`, -`Country of HE provider`, -`Academic Year`, -`Percentage`) %>%
  unique()

enrolment_personal_clean %>%
  filter(Category_Marker %in% "Age Group") %>%
  ggplot(aes(Category, Number, fill = Country)) +
  geom_col() +
  scale_fill_manual(values = c("#43A193", "#094F45", "#CF720E", "#8C5D2A")) +
  labs(title = "Age Group", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  )

ggsave("Figure_6a.png", plot = last_plot(), dpi = 300)  

enrolment_personal_clean %>%
  filter(Category_Marker %in% "Disability Status") %>%
  ggplot(aes(Category, Number, fill = Country)) +
  geom_col() +
  scale_fill_manual(values = c("#43A193", "#094F45", "#CF720E", "#8C5D2A")) +
  labs(title = "Disability Status", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  )

ggsave("Figure_6b.png", plot = last_plot(), dpi = 300)  

enrolment_personal_clean %>%
  filter(Category_Marker %in% "Ethnicity") %>%
  ggplot(aes(Category, Number, fill = Country)) +
  geom_col() +
  scale_fill_manual(values = c("#43A193", "#094F45", "#CF720E", "#8C5D2A")) +
  labs(title = "Ethnicity", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  )

ggsave("Figure_6c.png", plot = last_plot(), dpi = 300)  

### FIGURE 7: STUDENT ENROLMENT: PERSONAL CHARACTERISTICS (FIRST YEAR FULL TIME) SOUTHAMPTON

enrolment_personal_he <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/table-3.csv",
                               skip = 18, col_names = TRUE)

enrolment_personal_he %>%
  filter(`HE provider` %in% "The University of Southampton") %>%
  filter(!`Country of HE provider` %in% "All") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Age group` %in% "Total") %>%
  filter(`Academic Year` %in% c("2018/19")) %>%
  ggplot(aes(`Age group`, Number)) +
  geom_col(fill = "#4F0E43") +
  labs(title = "Age Group", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  ) 
  
ggsave("Figure_7a.png", plot = last_plot(), dpi = 300)  

enrolment_personal_he_dis <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/table-4.csv",
                                  skip = 18, col_names = TRUE)

enrolment_personal_he_dis %>%
  filter(`HE provider` %in% "The University of Southampton") %>%
  filter(!`Country of HE provider` %in% "All") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Disability marker` %in% "Total") %>%
  filter(`Academic Year` %in% c("2018/19")) %>%
  ggplot(aes(`Disability marker`, Number)) +
  geom_col(fill = "#4F0E43") +
  labs(title = "Disability Status", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  ) 

ggsave("Figure_7b.png", plot = last_plot(), dpi = 300)  

enrolment_personal_he_eth <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/table-5.csv",
                                      skip = 18, col_names = TRUE)

enrolment_personal_he_eth %>%
  filter(`HE provider` %in% "The University of Southampton") %>%
  filter(!`Country of HE provider` %in% "All") %>%
  filter(!`Region of HE provider` %in% "All") %>%
  filter(!`Ethnicity marker` %in% "Total") %>%
  filter(`Academic Year` %in% c("2018/19")) %>%
  ggplot(aes(`Ethnicity marker`, Number)) +
  geom_col(fill = "#4F0E43") +
  labs(title = "Ethnicity", y = "Number of Students") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  ) 

ggsave("Figure_7c.png", plot = last_plot(), dpi = 300)  

### FIGURE 8: PARTICIPATION CHARACTERISTICS: FIRST YEAR ENGLAND

participation <- read_csv("https://www.hesa.ac.uk/data-and-analysis/sb255/figure-5a.csv",
         skip = 16, col_names = TRUE)

participation_clean <- participation %>%
  filter(`First year marker` %in% "First year") %>%
  filter(!`Country of domicile` %in% "All") %>%
  filter(`Level of study` %in% "All") %>%
  filter(!`Category Marker` %in% "Total") %>%
  mutate(
    Category = case_when(
      Category == "Quintile 1 – most deprived (IMD)" ~ "Quintile 1 (IMD)",
      Category == "Quintile 5 – least deprived (IMD)" ~ "Quintile 5 (IMD)",
      TRUE ~ Category
    )) %>%
  mutate(Category_Marker = as.factor(`Category Marker`),
         Category = as.factor(Category),
         Country = as.factor(`Country of domicile`),
         Year = as.factor(`Academic Year`))

participation_clean %>%
  filter(`Category Marker` %in% "State school marker") %>%
  filter(Country %in% "England") %>%
  ggplot(aes(Year, Number, group = Category)) +
  geom_line(aes(color = Category), size = 0.8) +
  geom_text(aes(label=Number), family = "Noto Sans", hjust=-.5, nudge_y = 500, nudge_x = -.1, size=3.5) + 
  labs(title = "Participation: State School Marker", subtitle = "England (2015/16 to 2018/19)", y = "Number of Students") +
  geom_point(aes(color = Category)) +
  scale_colour_manual(values = c("#4F0E43", "#094F45", "#CF720E")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.subtitle = element_text(size = 16, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 12, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_text(family = "Noto Sans", size = 11),
    legend.text = element_text(family = "Noto Sans", size = 10)
  ) 

ggsave("Figure_8a.png", plot = last_plot(), dpi = 300)  

participation_clean %>%
  filter(`Category Marker` %in% "IMD") %>%
  ggplot(aes(Year, Number, group = Category)) +
  geom_line(aes(color = Category), size = 0.8) +
  labs(title = "Participation: English Deprivation Index (IMD)",
       subtitle = "2015/16 to 2018/19",
       y = "Number of Students",
       caption = "Quintile 1: Most Deprived | Quintile 5: Least Deprived") +
  geom_point(aes(color = Category)) +
  annotate("text", x = 4.3, y = 6045, size = 4, colour = "#43A193", family = "Noto Sans", label = "6045 (-14.66%)") +
  annotate("text", x = 4.3, y = 5700, size = 4, colour = "#094F45", family = "Noto Sans", label = "5700 (-13.51%)") +
  annotate("text", x = 4.3, y = 3475, size = 4, colour = "#4f0909", family = "Noto Sans", label = "3475 (-5.70%)") +
  annotate("text", x = 4.3, y = 2480, size = 4, colour = "#CF720E", family = "Noto Sans", label = "2480 (-8.66%)") +
  annotate("text", x = 4.3, y = 2135, size = 4, colour = "#8C5D2A", family = "Noto Sans", label = "2135 (-2.95%)") +
  annotate("text", x = 4.3, y = 55, size = 4, colour = "#4F0E43", family = "Noto Sans", label = "55 (+57.14%)") +
  scale_colour_manual(values = c("#43A193", "#094F45", "#4f0909", "#CF720E", "#8C5D2A", "#4F0E43")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, family = "Noto Sans", margin = margin(0,10,0,0)),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.subtitle = element_text(size = 16, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 10, family = "Noto Sans", hjust = 0.5, margin = margin(10,0,0,0)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "Noto Sans", size = 10)
  ) 

ggsave("Figure_8b.png", plot = last_plot(), dpi = 300)

### Figure 8 English Universities Non-UK domiciled HE student (2018-2019) FT (EU intake)

domicile <- read_csv("https://www.hesa.ac.uk/data-and-analysis/students/table-28-(2018-19).csv", skip = 13, col_names = TRUE)

domicile.clean <- domicile %>%
  filter(`Level of study` %in% "All") %>% 
  filter(`Mode of study` %in% "Full-time") %>% 
  filter(`Country of HE provider` %in% c("England", "Scotland", "Wales")) %>%
  mutate(
    `Country of domicile` = case_when(
      `Country of domicile` == "Cyprus (European Union)" ~ "Cyprus",
      TRUE ~ `Country of domicile`
    )) %>%
  filter(!`Country of domicile` %in% c("French Guiana", "Egypt", "Angola", "Algeria", "Martinique", "Mayotte", "Guadeloupe", "Åland Islands", "Cyprus not otherwise specified", "Total Other Europe", "European Union not otherwise specified", "Total Other EU", "Cyprus (Non-European Union)", "Europe not otherwise specified")) %>%
  filter(`Region of HE provider` %in% "All") %>%
  filter(!`HE provider` %in% "Total") %>%
  group_by(`Country of domicile`) %>%
  rename(sovereignt = `Country of domicile`) %>%
  summarise(count = sum(Number))

worldmap <- ne_countries(scale = 'medium', type = 'countries',
                         returnclass = 'sf')


world <- left_join(worldmap, domicile.clean, by = "sovereignt")

ggplot(world) + 
  geom_sf(color = NA, aes(fill = count)) +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  scale_fill_viridis(option = "plasma", na.value = "grey70") +
  labs(title = "Full-time UK HE student intake from Europe (2018/19)*", 
       caption = "*Data excludes intake from Russia",
       fill = "Number of students") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 10, family = "Noto Sans", margin = margin(10,0,0,0)),
    legend.text = element_text(family = "Noto Sans", size = 10),
    legend.title = element_text(family = "Noto Sans")
  ) 

ggsave("Figure_9a.png", plot = last_plot(), dpi = 300)

domicile.clean.soton <- domicile %>%
  filter(`Level of study` %in% "All") %>% 
  filter(`Mode of study` %in% "Full-time") %>% 
  filter(`HE provider` %in% "The University of Southampton") %>%
  mutate(
    `Country of domicile` = case_when(
      `Country of domicile` == "Cyprus (European Union)" ~ "Cyprus",
      TRUE ~ `Country of domicile`
    )) %>%
  filter(!`Country of domicile` %in% c("French Guiana", "Egypt", "Angola", "Algeria", "Martinique", "Mayotte", "Guadeloupe", "Åland Islands", "Cyprus not otherwise specified", "Total Other Europe", "European Union not otherwise specified", "Total Other EU", "Cyprus (Non-European Union)", "Europe not otherwise specified")) %>%
  filter(`Region of HE provider` %in% "All") %>%
  filter(!`HE provider` %in% "Total") %>%
  group_by(`Country of domicile`) %>%
  rename(sovereignt = `Country of domicile`) %>%
  summarise(count = sum(Number))

worldmap <- ne_countries(scale = 'medium', type = 'countries',
                         returnclass = 'sf')


world.2 <- left_join(worldmap, domicile.clean.soton, by = "sovereignt")

ggplot(world.2) + 
  geom_sf(color = NA, aes(fill = count)) +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  scale_fill_viridis(option = "plasma", na.value = "grey70") +
  labs(title = "Full-time UoS HE student intake from Europe (2018/19)*", 
       caption = "*Data excludes intake from Russia",
       fill = "Number of students") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 10, family = "Noto Sans", margin = margin(10,0,0,0)),
    legend.text = element_text(family = "Noto Sans", size = 10),
    legend.title = element_text(family = "Noto Sans"))

ggsave("Figure_9b.png", plot = last_plot(), dpi = 300)


### FIGURE 10: DOMICILE PCA

ukprn_data <- read_tsv("ukprn.txt") %>%
  select(`Provider UKPRN`, `Provider short name`, `Russell Group filter`) %>%
  rename(UKPRN = `Provider UKPRN`)

domicile.clean.mv <- domicile %>%
  filter(`Level of study` %in% "All") %>% 
  filter(`Mode of study` %in% "Full-time") %>% 
  filter(`Country of HE provider` %in% "England") %>%
  filter(`Region of domicile` %in% "Geographic region - Other European Union") %>%
  mutate(
    `Country of domicile` = case_when(
      `Country of domicile` == "Cyprus (European Union)" ~ "Cyprus",
      TRUE ~ `Country of domicile`
    )) %>%
  filter(!`Country of domicile` %in% c("French Guiana", "Martinique", "Mayotte", "Guadeloupe", "Åland Islands", "Cyprus not otherwise specified", "Total Other Europe", "European Union not otherwise specified", "Total Other EU", "Cyprus (Non-European Union)", "Europe not otherwise specified")) %>%
  filter(`Region of HE provider` %in% "All") %>%
  filter(!`HE provider` %in% "Total") %>%
  select(-`Region of HE provider`, -`Country of HE provider`, -`Level of study`, -`Mode of study`, -`Academic Year`, -`Region of domicile`) %>%
  pivot_wider(names_from = `Country of domicile`, values_from = Number, values_fill = 0) 

domicile.clean.mv.rg <- left_join(domicile.clean.mv, ukprn_data, by = "UKPRN") %>%
  filter(!is.na(`Russell Group filter`))


pca_rec <- recipe(~., data = domicile.clean.mv.rg) %>%
  update_role(`HE provider`, UKPRN, `Provider short name`, `Russell Group filter`,  new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

tidied_pca %>%
  filter(component %in% paste0("PC", 2:3)) %>%
  group_by(component) %>%
  top_n(12, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Contribution Values (Red: Negative | Blue: Positive)",
    x = "",
    y = "") + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    strip.text.x = element_text(size = 10, family = "Noto Sans", face = "bold"),
    plot.title = element_text(size = 16, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    legend.position = "none")

ggsave("Figure_10a.png", plot = last_plot(), dpi = 300)


juice(pca_prep) %>%
  ggplot(aes(PC4, PC5, label = `Provider short name`)) +
  geom_point(alpha = 0.7, size = 2, aes(colour = `Russell Group filter`)) +
  geom_text(check_overlap = TRUE, family = "Noto Sans", vjust = -0.6, hjust = 1.1)  + 
  labs(title = "Principal Component Analysis",
       x = "Principal Component 2",
       y = "Principal Component 3",
       subtitle = "HE full-time intake from Europe (2018/19)*",
       caption = "*Data excludes intake from Russia",
       colour = "Russell Group") +
  lims(x = c(-8, 5.5)) + 
  scale_colour_manual(values = c("#43A193", "#CF720E")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 10, family = "Noto Sans"),
    axis.title.y = element_text(size = 10, family = "Noto Sans"),
    axis.text.x = element_text(size = 10, family = "Noto Sans"),
    axis.text.y = element_text(size = 10, family = "Noto Sans"),
    plot.title = element_text(size = 24, family = "Noto Sans", face = "bold", hjust = 0.5, margin = margin(10,0,10,0)),
    plot.caption = element_text(size = 10, family = "Noto Sans", margin = margin(10,0,0,0)),
    plot.subtitle = element_text(size = 18, hjust = 0.5, family = "Noto Sans", margin = margin(10,0,0,0)),
    legend.text = element_text(family = "Noto Sans", size = 12),
    legend.title = element_text(family = "Noto Sans", size = 16),
    legend.position = "bottom")

ggsave("Figure_10b.png", plot = last_plot(), dpi = 300)
