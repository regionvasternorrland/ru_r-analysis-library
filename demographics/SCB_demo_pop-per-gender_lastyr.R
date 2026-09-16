library(pxweb)
library(tidyverse)
library(janitor)
library(plotly)


demo_age_gender <- function(
    geografi,
    language = "SWE"
) {
  
  
  # ARGUMENT CHECKS ----------------------------------------------------
  
  geografi <- as.character(geografi)
  language <- toupper(language)
  
  if (length(geografi) != 1) {
    stop("`geografi` must contain exactly one region or municipality code.")
  }
  
  if (!language %in% c("SWE", "ENG")) {
    stop("`language` must be either 'SWE' or 'ENG'.")
  }
  
  
  
  # IMPORT -------------------------------------------------------------
  
  ## Always download selected geography + Sweden
  regions_to_get <- unique(
    c(geografi, "00")
  )
  
  
  pxweb_query_list <- list(
    
    "Region" = regions_to_get,
    
    "Alder" = c(
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
      "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
      "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
      "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
      "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
      "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
      "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
      "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
      "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
      "90", "91", "92", "93", "94", "95", "96", "97", "98", "99",
      "100+1"
    ),
    
    "Kon" = c(
      "1",
      "2"
    )
  )
  
  
  px_data <- pxweb_get(
    url = paste0(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/",
      "START/BE/BE0101/BE0101A/FolkmangdDecCKM"
    ),
    query = pxweb_query_list
  )
  
  
  
  # DATA FRAME ---------------------------------------------------------
  
  ## Get labels + codes for geography and age
  
  pop_gender <- cbind(
    
    as.data.frame(
      px_data,
      column.name.type = "text",
      variable.value.type = "text"
    ),
    
    as.data.frame(
      px_data,
      column.name.type = "text",
      variable.value.type = "code"
    )[1],
    
    as.data.frame(
      px_data,
      column.name.type = "text",
      variable.value.type = "code"
    )[2]
    
  ) %>%
    clean_names()
  
  
  
  # LATEST YEAR --------------------------------------------------------
  
  ## Find latest available actual year automatically
  
  latest_year <- max(
    as.integer(pop_gender$ar),
    na.rm = TRUE
  )
  
  
  ## Keep only latest year
  
  pop_gender_latest <- pop_gender %>%
    filter(
      as.integer(ar) == latest_year
    )
  
  
  
  # TIDY ---------------------------------------------------------------
  
  pop_gender_latest <- pop_gender_latest %>%
    mutate(
      
      ## Numeric age; "100+1" becomes 100
      age_num = readr::parse_number(alder_2),
      
      ## Cleaner age label
      age_label = if_else(
        age_num == 100,
        "100+",
        as.character(age_num)
      )
    )
  
  
  
  # SELECTED GEOGRAPHY -------------------------------------------------
  
  pop_geo <- pop_gender_latest %>%
    filter(
      region_2 == geografi
    )
  
  
  if (nrow(pop_geo) == 0) {
    stop(
      paste0(
        "No data found for geography code ",
        geografi,
        "."
      )
    )
  }
  
  
  region_name <- pop_geo$region[1]
  
  
  
  # WOMEN 18–44 --------------------------------------------------------
  
  ## Selected geography
  
  geo_18_44 <- pop_gender_latest %>%
    filter(
      region_2 == geografi,
      age_num >= 18,
      age_num <= 44
    )
  
  
  women_18_44 <- geo_18_44 %>%
    filter(
      kon == "kvinnor"
    ) %>%
    summarise(
      n = sum(antal, na.rm = TRUE)
    ) %>%
    pull(n)
  
  
  men_18_44 <- geo_18_44 %>%
    filter(
      kon == "män"
    ) %>%
    summarise(
      n = sum(antal, na.rm = TRUE)
    ) %>%
    pull(n)
  
  
  population_18_44 <- 
    women_18_44 + men_18_44
  
  
  female_share_18_44 <- 
    women_18_44 / population_18_44
  
  
  
  # WOMEN 18–44 – SWEDEN ----------------------------------------------
  
  swe_18_44 <- pop_gender_latest %>%
    filter(
      region_2 == "00",
      age_num >= 18,
      age_num <= 44
    )
  
  
  women_18_44_swe <- swe_18_44 %>%
    filter(
      kon == "kvinnor"
    ) %>%
    summarise(
      n = sum(antal, na.rm = TRUE)
    ) %>%
    pull(n)
  
  
  men_18_44_swe <- swe_18_44 %>%
    filter(
      kon == "män"
    ) %>%
    summarise(
      n = sum(antal, na.rm = TRUE)
    ) %>%
    pull(n)
  
  
  population_18_44_swe <- 
    women_18_44_swe + men_18_44_swe
  
  
  female_share_18_44_swe <- 
    women_18_44_swe / population_18_44_swe
  
  
  
  # DIFFERENCE COMPARED WITH SWEDEN -----------------------------------
  
  ## Percentage-point difference:
  ## positive = higher female share than Sweden
  ## negative = lower female share than Sweden
  
  female_share_diff_pp <- 
    (
      female_share_18_44 -
        female_share_18_44_swe
    ) * 100
  
  
  ## Number of women the geography would have if its
  ## female share 18–44 were identical to Sweden
  
  expected_women_18_44 <- 
    population_18_44 *
    female_share_18_44_swe
  
  
  ## Signed difference:
  ## negative = fewer women than with Swedish sex distribution
  ## positive = more women
  
  female_diff_number <- round(
    women_18_44 -
      expected_women_18_44
  )
  
  
  
  # TEXT ---------------------------------------------------------------
  
  if (language == "SWE") {
    
    title_text <- "Befolkning efter ålder och kön"
    x_text <- "Ålder"
    y_text <- "Folkmängd"
    legend_text <- "Kön:"
    
    men_text <- "män"
    women_text <- "kvinnor"
    
    tooltip_sex <- "Kön"
    tooltip_age <- "Ålder"
    tooltip_pop <- "Folkmängd"
    
  } else {
    
    title_text <- "Population by age and sex"
    x_text <- "Age"
    y_text <- "Population"
    legend_text <- "Sex:"
    
    men_text <- "men"
    women_text <- "women"
    
    tooltip_sex <- "Sex"
    tooltip_age <- "Age"
    tooltip_pop <- "Population"
  }
  
  
  
  # PLOT DATA ----------------------------------------------------------
  
  plot_data <- pop_geo %>%
    mutate(
      kon_plot = case_when(
        kon == "män" ~ men_text,
        kon == "kvinnor" ~ women_text,
        TRUE ~ kon
      )
    )
  
  
  
  # VISUALIZE ----------------------------------------------------------
  
  farg_kon <- c(
    men_text = "#005ca9",
    women_text = "#e8308a"
  )
  
  names(farg_kon) <- c(
    men_text,
    women_text
  )
  
  
  linetype_kon <- c(
    men_text = "dashed",
    women_text = "solid"
  )
  
  names(linetype_kon) <- c(
    men_text,
    women_text
  )
  
  
  plot <- ggplot(
    plot_data,
    aes(
      x = age_num,
      y = antal,
      color = kon_plot,
      group = kon_plot,
      linetype = kon_plot,
      text = paste0(
        "<b>", region_name, "</b>",
        "<br>", tooltip_sex, ": ", kon_plot,
        "<br>", tooltip_age, ": ", age_label,
        "<br>", tooltip_pop, ": ",
        format(
          antal,
          big.mark = " ",
          scientific = FALSE
        )
      )
    )
  ) +
    
    geom_line(
      linewidth = 1
    ) +
    
    scale_linetype_manual(
      name = legend_text,
      values = linetype_kon
    ) +
    
    scale_color_manual(
      name = legend_text,
      values = farg_kon
    ) +
    
    scale_y_continuous(
      labels = function(x) {
        format(
          x,
          big.mark = " ",
          scientific = FALSE
        )
      }
    ) +
    
    scale_x_continuous(
      breaks = seq(
        0,
        100,
        by = 5
      ),
      labels = function(x) {
        ifelse(
          x == 100,
          "100+",
          x
        )
      }
    ) +
    
    labs(
      title = title_text,
      subtitle = sprintf(
        "%s - %s",
        region_name,
        latest_year
      ),
      x = x_text,
      y = y_text
    ) +
    
    theme_classic() +
    
    theme(
      legend.title = element_text(
        color = "#616161"
      ),
      panel.grid.major.y = element_line(
        color = "#ECEBEA"
      ),
      axis.line.y = element_line(
        color = "#BDBAB6"
      ),
      axis.line.x = element_line(
        color = "#BDBAB6"
      ),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        color = "#616161",
        angle = 45,
        vjust = 1,
        hjust = 1
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
      legend.text = element_text(
        color = "#616161"
      ),
      legend.position = "top",
      legend.justification = "left",
      legend.box.just = "left"
    )
  
  
  
  # PLOTLY -------------------------------------------------------------
  
  plotly <- ggplotly(
    plot,
    tooltip = "text"
  ) %>%
    layout(
      
      title = list(
        text = paste0(
          title_text,
          "<br><sup>",
          region_name,
          " - ",
          latest_year,
          "</sup>"
        )
      ),
      
      margin = list(
        l = 80,
        r = 50,
        b = 50,
        t = 100
      ),
      
      legend = list(
        orientation = "h",
        x = 0,
        xanchor = "left",
        y = 1.1,
        yanchor = "top"
      ),
      
      yaxis = list(
        title = list(
          text = plot$labels$y,
          standoff = 8
        ),
        automargin = TRUE
      )
    )
  
  
  
  # RETURN -------------------------------------------------------------
  
  return(
    list(
      
      plot = plotly,
      
      region = region_name,
      region_code = geografi,
      latest_year = latest_year,
      
      women_18_44 = women_18_44,
      men_18_44 = men_18_44,
      population_18_44 = population_18_44,
      
      female_share_18_44 = female_share_18_44,
      female_share_18_44_pct = female_share_18_44 * 100,
      
      female_share_18_44_swe = female_share_18_44_swe,
      female_share_18_44_swe_pct = female_share_18_44_swe * 100,
      
      female_share_diff_pp = female_share_diff_pp,
      
      expected_women_18_44 = round(
        expected_women_18_44
      ),
      
      female_diff_number = female_diff_number,
      
      data = pop_geo
    )
  )
}