library(pxweb)
library(janitor)
library(tidyverse)

# INPUT
region <- "22"
comp_region <- "00"



# IMPORT
## Combine region(s)
regions <- c(region, comp_region)


## PXWEB query
pxweb_query_list <- 
  list("Region"=regions,
       "Utbildning" =c(
         "00S",
         
         #"1",
         #"15B",
         #"15F",
         #"15G",
         #"15HP",
         #"15S",
         #"15V",
         #"15X",
         
         #"2",
         #"25H",
         #"25K",
         #"25M",
         #"25T",
         #"25X",
         
         #"3",
         #"33H",
         #"35B",
         #"35E",
         #"35F",
         #"35J",
         #"35M",
         #"35P",
         #"35S",
         #"35X",
         #"35V",
         #"35Y",
         
         #"4",
         #"45B",
         #"45D",
         #"45DH",
         #"45DL",
         #"45FM",
         #"45G",
         #"45K",
         #"45Q",
         #"45X",
         
         "5",
         "53A+55Q",
         #"53B",
         "53E",
         #"53F",
         "53I",
         #"53R",
         #"55A",
         "55B",
         "55C",
         "55D",
         "55E",
         "55F",
         #"55G",
         "55H",
         "55I",
         "55J",
         "55K",
         #"55L",
         "55T",
         "55X"
         
         #"6",
         #"63Z",
         #"65J",
         #"65S",
         #"65V",
         #"65X",
         
         #"7",
         #"73B",
         #"73OX",
         #"73T",
         #"75A",
         #"75B",
         #"75D",
         #"75F",
         #"75H",
         #"75HS",
         #"75J",
         #"75L",
         #"75M",
         #"75N",
         #"75O",
         #"75P",
         #"75R",
         #"75S",
         #"75SA",
         #"75SB",
         #"75SD",
         #"75SP",
         #"75SX",
         #"75T",
         #"75V",
         #"75X",
         
         #"8",
         #"83H",
         #"83R",
         #"83T",
         #"85P",
         #"85T",
         #"85X"
       ),
       "SNI2007" = "A-U",
       "KonAlderFodelseland"=c(
         "20-24",
         "25-29",
         "30-34",
         "35-39",
         
         "20-69"
       ),
       "ContentsCode"=c(
         #"000008QQ", # Anställda helt matchade (A)
         "000008QT"), # Totalt antal - justerat (E)
       "Tid"="*"
       
  ) 

### Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906A/RegionInd19E3CKMNy",
            query = pxweb_query_list)


### Convert to data.frame 
df <- cbind(
  as.data.frame(px_data, column.name.type = "text", variable.value.type = "text"),
  as.data.frame(px_data, column.name.type = "text", variable.value.type = "code")[2] #Utb.g.-code
)



# TIDY
## Clean
e3 <- clean_names(df)



# TRANSFORM
## Keep latest year
latest_year <- max(as.integer(e3$ar), na.rm = TRUE)

e3 <- e3 %>%
  filter(as.integer(ar) == latest_year)


## Geography labels
e3 <- e3 %>%
  mutate(
    geografi = case_when(
      region == "Västernorrlands län" ~ "Västernorrland",
      region == "Riket" ~ "Riket",
      TRUE ~ region
    )
  )


## Calculate number aged 20–39
df_20_39 <- e3 %>%
  filter(
    kon_alder_fodelseland %in% c(
      "20–24 år",
      "25–29 år",
      "30–34 år",
      "35–39 år"
    )
  ) %>%
  group_by(
    ar,
    utbildning_2,
    utbildning,
    geografi
  ) %>%
  summarise(
    antal_20_39 = sum(
      totalt_antal_personer_justerat_antal_e,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## Get total number aged 20–69
df_20_69 <- e3 %>%
  filter(
    kon_alder_fodelseland == "20–69 år"
  ) %>%
  transmute(
    ar,
    utbildning_2,
    utbildning,
    geografi,
    antal_20_69 = totalt_antal_personer_justerat_antal_e
  )


## Join numerator and denominator
rmi_atervaxt <- df_20_39 %>%
  left_join(
    df_20_69,
    by = c(
      "ar",
      "utbildning_2",
      "utbildning",
      "geografi"
    )
  ) %>%
  mutate(
    andel = if_else(
      !is.na(antal_20_69) & antal_20_69 > 0,
      antal_20_39 / antal_20_69,
      NA_real_
    ),
    
    andel_pct = andel * 100
  )



## Rensa utbildningsnamnen
rmi_atervaxt <- rmi_atervaxt %>%
  mutate(
    huvudgrupp = str_detect(utbildning_2, "^\\d$"),
    
    utbildning_label = utbildning,
    
    utbildning_label = str_remove(
      utbildning_label,
      regex(
        "\\s*[–-]\\s*gymnasial$",
        ignore_case = TRUE
      )
    ),
    
    utbildning_label = str_replace(
      utbildning_label,
      "^civilingenjörsutbildning",
      "civ.ing."
    ),
    
    utbildning_label = str_replace(
      utbildning_label,
      "^högskoleingenjörsutb",
      "hög.ing."
    ),
    
    utbildning_label = if_else(
      huvudgrupp,
      paste0("Ämne: ", utbildning_label),
      utbildning_label
    )
  )



## Tooltip för Plotly
rmi_atervaxt <- rmi_atervaxt %>%
  mutate(
    tooltip = paste0(
      "<b>", utbildning_label, "</b>",
      "<br>",
      geografi,
      "<br>",
      "Andel 20–39 år: ",
      format(
        round(andel_pct, 1),
        decimal.mark = ",",
        nsmall = 1
      ),
      " %",
      "<br>",
      "20–39 år: ",
      format(
        antal_20_39,
        big.mark = " ",
        scientific = FALSE
      ),
      "<br>",
      "20–69 år: ",
      format(
        antal_20_69,
        big.mark = " ",
        scientific = FALSE
      )
    )
  )



## Referensvärden: samtliga utbildningsgrupper
df_ref <- rmi_atervaxt %>%
  filter(utbildning_2 == "00S")


df_dumbbell <- rmi_atervaxt %>%
  filter(utbildning_2 != "00S")



## Behåll bara utbildningar som finns för båda geografierna
kompletta_utbildningar <- df_dumbbell %>%
  filter(!is.na(andel_pct)) %>%
  distinct(utbildning_2, geografi) %>%
  count(utbildning_2) %>%
  filter(n == 2) %>%
  pull(utbildning_2)


df_dumbbell <- df_dumbbell %>%
  filter(
    utbildning_2 %in% kompletta_utbildningar
  )



## Skapa segmenten till dumbbell-grafen
seg_df <- df_dumbbell %>%
  select(
    utbildning_2,
    utbildning_label,
    geografi,
    andel_pct
  ) %>%
  pivot_wider(
    names_from = geografi,
    values_from = andel_pct
  )



## Sortera - ämne överst
label_levels <- seg_df %>%
  mutate(
    # TRUE för ämnesinriktning/huvudgrupp, t.ex. "5"
    huvudgrupp = str_detect(utbildning_2, "^\\d$"),
    
    # Andra tecknet i koden anger utbildningsnivå
    # t.ex. 33H -> 3, 45D -> 5? OBS: se kommentar nedan
    utbildningsniva = if_else(
      huvudgrupp,
      NA_integer_,
      as.integer(str_sub(utbildning_2, 2, 2))
    )
  ) %>%
  arrange(
    desc(huvudgrupp),   # huvudgruppen först
    utbildningsniva,    # sedan 3, 4, 5 ...
    utbildning_2        # stabil ordning inom samma nivå
  ) %>%
  pull(utbildning_label)

df_dumbbell <- df_dumbbell %>%
  mutate(
    utbildning_label = factor(
      utbildning_label,
      levels = rev(label_levels)
    )
  )

seg_df <- seg_df %>%
  mutate(
    utbildning_label = factor(
      utbildning_label,
      levels = levels(df_dumbbell$utbildning_label)
    )
  )



## Referenslinjer
ref_riket <- df_ref %>%
  filter(geografi == "Riket") %>%
  pull(andel_pct) %>%
  first()


ref_vn <- df_ref %>%
  filter(geografi == "Västernorrland") %>%
  pull(andel_pct) %>%
  first()



# VIZ
## ggplot
p <- ggplot() +
  
  ## Dumbbell segments
  geom_segment(
    data = seg_df,
    aes(
      x = Riket,
      xend = Västernorrland,
      y = utbildning_label,
      yend = utbildning_label
    ),
    color = "#BDBAB6",
    linewidth = 2
  ) +
  
  ## Reference lines - all education groups
  geom_vline(
    xintercept = ref_riket,
    linetype = "dashed",
    color = "#575756",
    alpha = 0.55,
    linewidth = 0.9
  ) +
  
  geom_vline(
    xintercept = ref_vn,
    linetype = "dashed",
    color = "#00AA9E",
    alpha = 0.65,
    linewidth = 0.9
  ) +
  
  ## Geography points
  geom_point(
    data = df_dumbbell,
    aes(
      x = andel_pct,
      y = utbildning_label,
      color = geografi,
      text = tooltip
    ),
    size = 5
  ) +
  
  scale_color_manual(
    name = "Geografi:",
    values = c(
      "Riket" = "#575756",
      "Västernorrland" = "#00AA9E"
    )
  ) +
  
  scale_x_continuous(
    labels = function(x) {
      paste0(round(x, 0), " %")
    }
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_classic() +
  
  theme(
    legend.title = element_text(
      color = "#616161"
    ),
    
    legend.text = element_text(
      color = "#616161"
    ),
    
    legend.position = "bottom",
    
    axis.line.y = element_line(
      color = "#BDBAB6"
    ),
    
    axis.line.x = element_line(
      color = "#BDBAB6"
    ),
    
    axis.ticks.x = element_blank(),
    
    axis.text.x = element_text(
      color = "#616161"
    ),
    
    axis.text.y = element_text(
      color = "#616161"
    ),
    
    axis.ticks.y = element_line(
      color = "#616161"
    ),
    
    axis.title = element_text(
      color = "#616161"
    ),
    
    plot.margin = margin(
      10, 20, 5, 10
    )
  )


## plotly
fig <- ggplotly(
  p,
  tooltip = "text"
) %>%
  layout(
    title = list(
      text = paste0(
        "Återväxt",
        "<br>",
        "<sup style='color:#616161'>",
        "Andel 20–39 år av 20–69 år – ",
        latest_year,
        "</sup>"
      ),
      
      x = 0,
      xanchor = "left",
      
      font = list(
        size = 16,
        color = "#212529"
      )
    ),
    
    margin = list(
      l = 210,
      r = 20,
      t = 75,
      b = 65
    ),
    
    hoverlabel = list(
      bgcolor = "#2b2b2b",
      bordercolor = "#575756",
      
      font = list(
        color = "white",
        size = 12
      )
    ),
    
    legend = list(
      orientation = "h",
      x = 0.25,
      y = -0.25
    )
  ) %>%
  
  config(
    displaylogo = FALSE,
    
    modeBarButtonsToRemove = c(
      "zoom2d",
      "pan2d",
      "select2d",
      "lasso2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian",
      "toggleSpikelines"
    )
  )


fig
