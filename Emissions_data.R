require(tidyverse)
require(timetk)
require(readr)
require(dplyr)
require(xts)
require(countrycode)

emissions_df <- read_csv("../Environment_Emissions_by_Sector_E_All_Data_(Normalized).csv")

emissions_df$Year <- year(emissions_df$`Year Code`)

emissions_df %>%
  summarise(across(everything(), n_distinct, .names = "count_{.col}"))

emissions_df_countries <- emissions_df[1:425699,]

emissions_df_agri <- emissions_df_countries %>%
  filter(Item == "Emissions on agricultural land"
         & Unit =="tonnes/capita") %>%
  select(`Year Code`, Area, `Area Code (M49)`, Value) %>%
  rename(Year=`Year Code`,
         Area_Code = `Area Code (M49)`)%>%
  mutate(Area_Code = as.numeric(gsub("^'", "", Area_Code)))

country_with_continents <- read_csv("https://gist.githubusercontent.com/stevewithington/20a69c0b6d2ff846ea5d35e5fc47f26c/raw")
country_with_continents <- country_with_continents  %>%
  mutate(duplicate_chk = duplicated(Country_Number),
         GeoRegion = countrycode(Three_Letter_Country_Code,
                                 origin = "iso3c",
                                 destination = "un.regionsub.name"),
         Continent = countrycode(Three_Letter_Country_Code,
                                 origin = "iso3c",
                                 destination = "continent"))  %>%
  filter(duplicate_chk==FALSE)

# Handling Missing Data
country_with_continents <- country_with_continents %>%
  mutate(
    Continent = case_when(
      is.na(Continent) & grepl("Latin", GeoRegion,ignore.case = TRUE)   ~ "Americas",
      is.na(Continent) & grepl("Africa", GeoRegion,ignore.case = TRUE)  ~ "Africa",
      is.na(Continent) & 
        grepl("Australia|New Zealand|Micronesia|Polynesia|Melanesia", GeoRegion,ignore.case = TRUE)  ~ "Oceania",
      TRUE ~ Continent
    ),
    GeoRegion = ifelse(
      is.na(GeoRegion) & grepl("Asia",Continent) & Country_Name=="Taiwan",
      "Eastern Asia",GeoRegion)
  )

# Joining further information on countries
emissions_df_agri <- emissions_df_agri %>%
  left_join(country_with_continents %>%
              select(Continent,GeoRegion,Country_Number)%>%
              rename(Area_Code=Country_Number),by="Area_Code")

# Cleaning and Modifying Text
emissions_df_agri <- emissions_df_agri %>%
  mutate(
    Area = iconv(Area, from = "", to = "UTF-8", sub = ""),
    Area = trimws(Area)
  )
emissions_df_agri$Continent <- ifelse(emissions_df_agri$Area == "Yugoslav SFR", "Europe", emissions_df_agri$Continent)

emissions_df_agri <- emissions_df_agri %>%
  mutate(
    Continent = case_when(
      is.na(Continent) & grepl("China",Area) ~ "Asia",
      is.na(Continent) & grepl("Belgium-Luxembourg",Area) ~ "Europe",
      is.na(Continent) & grepl("Czechoslovakia",Area) ~ "Europe",
      is.na(Continent) & grepl("Ethiopia PDR",Area) ~ "Africa",
      is.na(Continent) & grepl("Netherlands Antilles",Area) ~ "Americas",
      is.na(Continent) & grepl("Serbia and Montenegro",Area) ~ "Europe",
      is.na(Continent) & grepl("Sudan",Area) ~ "Africa",
      is.na(Continent) & grepl("USSR",Area) ~ "Europe",
      TRUE ~ Continent
    ),
    GeoRegion = case_when(
      is.na(GeoRegion) & grepl("China",Area) ~ "Eastern Asia",
      is.na(GeoRegion) & grepl("Belgium-Luxembourg",Area) ~ "Western Europe",
      is.na(GeoRegion) & grepl("Czechoslovakia",Area) ~ "Eastern Europe",
      is.na(GeoRegion) & grepl("Ethiopia PDR",Area) ~ "Eastern Africa",
      is.na(GeoRegion) & grepl("Netherlands Antilles",Area) ~ "Latin America and the Caribbean",
      is.na(GeoRegion) & grepl("Serbia and Montenegro",Area) ~ "Southern Europe",
      is.na(GeoRegion) & grepl("Sudan",Area) ~ "Northern Africa",
      is.na(GeoRegion) & grepl("USSR",Area) ~ "Eastern Europe",
      TRUE ~ GeoRegion
    )
  )

emissions_df_agri %>%
  plot_time_series(
    .date_var = Year,
    .value = Value,
    .color_var = Area,
    .title = "Per capita Emissions on Agricultural land",
    .smooth = FALSE
  )

ggplot(emissions_df_agri, aes(x = Year, y = Area, fill = Value)) +
  geom_tile(height = 5) +
  scale_fill_gradientn(
    colours = c("Very Low" = "#313695",
      "Low" = "#74add1",
      "Slight Low" = "#abd9e9",
      "Middle" = "#fda",
      "Slight High" = "#fdae61",
      "High" = "#f46d43",
      "Very High" = "#a50026"),
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(1.5, "npc"),
      barheight = 1
    )
  ) +
  labs(
    title = "Per Capita Emissions on Agricultural Land",
    x = "Year",
    y = "Country",
    fill = "Anomaly"
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n=20),
                     expand = c(0,0))+
  theme(
    axis.text.y = element_text(size = 2),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Identify Top 10 countries per continent
top_countries <- emissions_df_agri %>%
  filter(!is.na(Continent)) %>%
  group_by(Continent, Year) %>%
  slice_max(Value, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(Continent, Area)

emissions_agri_top <- emissions_df_agri %>%
  filter(!is.na(Continent)) %>%
  semi_join(top_countries, by = c("Continent", "Area"))

emissions_agri_top_complete <- emissions_agri_top %>%
  group_by(Continent) %>%
  complete(Year = full_seq(Year, 1), Area) %>%
  ungroup() %>%
  group_by(Area) %>%
  fill(Area_Code, Continent, .direction = "downup") %>%
  ungroup()

ggplot(emissions_agri_top_complete, aes(x = Year, y = Area, fill = Value)) +
  geom_tile(width=1,height=1,color="white",linewidth=0.2) +
  scale_fill_gradientn(
    colours = c("Very Low" = "#313695",
                "Low" = "#74add1",
                "Slight Low" = "#abd9e9",
                "Middle" = "#fda",
                "Slight High" = "#fdae61",
                "High" = "#f46d43",
                "Very High" = "#a50026"),
    limits = c(0, 38),
    breaks = scales::breaks_pretty(n = 15),
    guide = guide_colorsteps(
      direction = "horizontal",
      barwidth = unit(0.7, "npc"),
      barheight = unit(1, "lines"),
      show.limits = TRUE
    )
  ) +
  labs(
    title = "Per Capita Emissions on Agricultural Land",
    x = "Year",
    y = "Country",
    fill = "Anomaly"
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n=20),
                     expand = c(0,0))+
  facet_grid(Continent ~ .,scales="free_y")+
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    aspect.ratio = 0.2,
    strip.text.y = element_text(angle = 90,face = "bold"),
    panel.spacing = unit(0.05, "lines")
  )

emissions_xts <- emissions_df_agri %>%
  pivot_wider(id_cols = Year,names_from = Area,values_from = Value)

emissions_xtsobj <- emissions_xts %>%
  mutate(Year = as.Date(paste0(Year,"-01-01")))%>%
  tk_xts(date_var = Year)

plot.xts(emissions_xtsobj)
  