library(pxweb)
library(tidyverse)
library(janitor)
library(plotly)


demo_tot_wcomp <- function(
    geografi,
    language = "SWE",
    components = FALSE,
    forecast = TRUE,
    forecast_year = 2040
) {
  
  
  # ARGUMENT CHECKS ----------------------------------------------------
  
  language <- toupper(language)
  
  if (!language %in% c("SWE", "ENG")) {
    stop("`language` must be either 'SWE' or 'ENG'.")
  }
  
  if (!is.logical(components) || length(components) != 1) {
    stop("`components` must be TRUE or FALSE.")
  }
  
  if (!is.logical(forecast) || length(forecast) != 1) {
    stop("`forecast` must be TRUE or FALSE.")
  }
  
  geografi <- as.character(geografi)
  
  f_yr <- as.Date(
    paste0(forecast_year + 1, "-01-01")
  )
  
  
  
  # IMPORT ACTUALS -----------------------------------------------------
  
  ## Old SCB table: 2000–2024
  
  pxweb_query_old <- list(
    "Region" = geografi,
    "Forandringar" = c(
      "110", # folkökning
      "135", # födelseöverskott
      "240", # flyttningsöverskott eget län
      "250", # flyttningsöverskott övriga Sverige
      "260"  # invandringsöverskott
    ),
    "Period" = "hel",
    "Kon" = "1+2"
  )
  
  
  px_data_old <- pxweb_get(
    url = paste0(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/",
      "START/BE/BE0101/BE0101G/BefforandrKvRLK"
    ),
    query = pxweb_query_old
  )
  
  
  bef_old <- 
    as.data.frame(
      px_data_old,
      column.name.type = "text",
      variable.value.type = "text"
    ) %>%
    clean_names()
  
  
  
  ## New CKM table: 2025–
  
  pxweb_query_new <- list(
    "Region" = geografi,
    "Forandringar" = c(
      "110", # folkökning
      "135", # födelseöverskott
      "240", # flyttningsöverskott eget län
      "250", # flyttningsöverskott övriga Sverige
      "260"  # invandringsöverskott
    ),
    "Period" = "hel",
    "Kon" = "TotSa",
    "Tid" = "*"
  )
  
  
  px_data_new <- pxweb_get(
    url = paste0(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/",
      "START/BE/BE0101/BE0101G/BefforandrKvRLKCKM"
    ),
    query = pxweb_query_new
  )
  
  
  bef_new <- 
    as.data.frame(
      px_data_new,
      column.name.type = "text",
      variable.value.type = "text"
    ) %>%
    clean_names() %>%
    rename(
      antal_personer = befolkningsstatistik_antal_personer
    )
  
  
  
  ## Combine old + new actuals
  
  bef <- 
    bind_rows(
      bef_old,
      bef_new
    ) %>%
    mutate(
      ar = as.Date(
        paste0(ar, "-01-01")
      )
    )
  
  
  region_name <- bef$region[1]
  
  
  
  # LATEST ACTUAL YEAR -------------------------------------------------
  
  latest_actual_date <- max(
    bef$ar[!is.na(bef$antal_personer)],
    na.rm = TRUE
  )
  
  
  latest_actual_year <- as.integer(
    format(latest_actual_date, "%Y")
  )
  
  
  
  # POPULATION 2020 ----------------------------------------------------
  #
  # Get population stock for reference year 2020.
  # Population after region, marital status, age and sex.
  
  
  population_2020_query <- list(
    "Region" = geografi,
    "Civilstand" = c(
      "OG",
      "G",
      "SK",
      "ÄNKL"
    ),
    "Alder" = "tot",
    "Kon" = c(
      "1",
      "2"
    ),
    "ContentsCode" = "BE0101N1",
    "Tid" = "2020"
  )
  
  
  population_2020_px <- pxweb_get(
    url = paste0(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/",
      "START/BE/BE0101/BE0101A/BefolkningNy"
    ),
    query = population_2020_query
  )
  
  
  population_2020_df <- 
    as.data.frame(
      population_2020_px,
      column.name.type = "text",
      variable.value.type = "text"
    ) %>%
    clean_names()
  
  
  population_2020 <- sum(
    population_2020_df$folkmangd,
    na.rm = TRUE
  )
  
  
  
  # TOTAL POPULATION CHANGE --------------------------------------------
  
  folkokning <- bef %>%
    filter(
      forandringar == "folkökning",
      !is.na(antal_personer)
    )
  
  
  ## Actual population change from 2020 to latest actual year
  
  population_change_2020 <- folkokning %>%
    filter(
      ar > as.Date("2020-01-01"),
      ar <= latest_actual_date
    ) %>%
    summarise(
      change = sum(
        antal_personer,
        na.rm = TRUE
      )
    ) %>%
    pull(change)
  
  
  ## Latest population
  
  population_latest <- 
    population_2020 + population_change_2020
  
  
  ## Percentage population change since 2020
  
  population_change_2020_pct <- 
    population_change_2020 / population_2020 * 100
  
  
  
  # RETURN HELPER ------------------------------------------------------
  
  make_output <- function(plot_object) {
    
    list(
      plot = plot_object,
      region = region_name,
      region_code = geografi,
      latest_actual_year = latest_actual_year,
      population_latest = population_latest,
      population_2020 = population_2020,
      population_change_2020 = population_change_2020,
      population_change_2020_pct = population_change_2020_pct
    )
  }
  
  
  
  # TOTAL POPULATION CHANGE GRAPH --------------------------------------
  
  if (!components) {
    
    
    if (language == "SWE") {
      
      title_text <- "Befolkningsförändring per år"
      y_text <- "Antal"
      source_text <- "Källa: SCB (SSD)"
      tooltip_value <- "Folkökning"
      year_text <- "År"
      
    } else {
      
      title_text <- "Annual population change"
      y_text <- "Number"
      source_text <- "Source: Statistics Sweden (SCB)"
      tooltip_value <- "Population change"
      year_text <- "Year"
      
    }
    
    
    p <- ggplot(
      folkokning,
      aes(
        x = ar,
        y = antal_personer,
        group = forandringar,
        text = paste0(
          "<b>", region_name, "</b>",
          "<br>", year_text, ": ", format(ar, "%Y"),
          "<br>", tooltip_value, ": ", antal_personer
        )
      )
    ) +
      geom_line(
        color = "#005ca9"
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "grey"
      ) +
      labs(
        x = NULL,
        y = y_text,
        caption = source_text
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
      theme_classic()
    
    
    p <- ggplotly(
      p,
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            title_text,
            "<br>",
            "<sup>",
            region_name,
            "</sup>"
          ),
          x = 0.05,
          xanchor = "left"
        ),
        margin = list(
          l = 60,
          r = 20,
          b = 50,
          t = 70
        )
      )
    
    
    return(
      make_output(p)
    )
  }
  
  
  
  # COMPONENTS ---------------------------------------------------------
  
  fodelseoverskott <- bef %>%
    filter(
      forandringar == "födelseöverskott"
    )
  
  
  flytt_ovr_se <- bef %>%
    filter(
      forandringar == "flyttningsöverskott övriga Sverige"
    )
  
  
  flytt_eget_lan <- bef %>%
    filter(
      forandringar == "flyttningsöverskott eget län"
    )
  
  
  inrikesflyttningsnetto <- 
    bind_rows(
      flytt_ovr_se,
      flytt_eget_lan
    ) %>%
    group_by(
      region,
      ar
    ) %>%
    summarise(
      antal_personer_sum = if (
        all(is.na(antal_personer))
      ) {
        NA_real_
      } else {
        sum(
          antal_personer,
          na.rm = TRUE
        )
      },
      .groups = "drop"
    )
  
  
  invandringsoverskott <- bef %>%
    filter(
      forandringar == "invandringsöverskott"
    )
  
  
  
  # LANGUAGE -----------------------------------------------------------
  
  if (language == "SWE") {
    
    main_title <- "Årlig befolkningsförändring - komponenter"
    
    sub_title <- paste0(
      "Födelseöverskott (lila), ",
      "flyttnetto (röd), ",
      "invandringsnetto (gul) - ",
      region_name
    )
    
    year_label <- "År"
    
    birth_label <- "Födelseöverskott"
    migration_label <- "Inrikesflyttningsnetto"
    immigration_label <- "Invandringsöverskott"
    
    birth_actual <- "Födelseöverskott (utfall)"
    birth_forecast <- "Födelseöverskott (framskrivning)"
    
    migration_actual <- "Inrikesflyttningsnetto (utfall)"
    migration_forecast <- "Inrikesflyttningsnetto (framskrivning)"
    
    immigration_actual <- "Invandringsöverskott (utfall)"
    immigration_forecast <- "Invandringsöverskott (framskrivning)"
    
  } else {
    
    main_title <- "Annual population change - components"
    
    sub_title <- paste0(
      "Natural population change (purple), ",
      "domestic net migration (red), ",
      "net immigration (yellow) - ",
      region_name
    )
    
    year_label <- "Year"
    
    birth_label <- "Natural population change"
    migration_label <- "Domestic net migration"
    immigration_label <- "Net immigration"
    
    birth_actual <- "Natural population change (actual)"
    birth_forecast <- "Natural population change (forecast)"
    
    migration_actual <- "Domestic net migration (actual)"
    migration_forecast <- "Domestic net migration (forecast)"
    
    immigration_actual <- "Net immigration (actual)"
    immigration_forecast <- "Net immigration (forecast)"
  }
  
  
  
  # COMPONENTS WITHOUT FORECAST ----------------------------------------
  
  if (!forecast) {
    
    
    p1 <- ggplot(
      fodelseoverskott,
      aes(
        x = ar,
        y = antal_personer,
        group = 1,
        text = paste0(
          "<b>", region_name, "</b>",
          "<br>", year_label, ": ", format(ar, "%Y"),
          "<br>", birth_label, ": ", antal_personer
        )
      )
    ) +
      geom_line(
        color = "#954b97"
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
      theme_classic()
    
    
    p2 <- ggplot(
      inrikesflyttningsnetto,
      aes(
        x = ar,
        y = antal_personer_sum,
        group = 1,
        text = paste0(
          "<b>", region_name, "</b>",
          "<br>", year_label, ": ", format(ar, "%Y"),
          "<br>", migration_label, ": ",
          antal_personer_sum
        )
      )
    ) +
      geom_line(
        color = "#eb6209"
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
      theme_classic()
    
    
    p3 <- ggplot(
      invandringsoverskott,
      aes(
        x = ar,
        y = antal_personer,
        group = 1,
        text = paste0(
          "<b>", region_name, "</b>",
          "<br>", year_label, ": ", format(ar, "%Y"),
          "<br>", immigration_label, ": ",
          antal_personer
        )
      )
    ) +
      geom_line(
        color = "#ffcc00"
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
      theme_classic()
    
    
    p1 <- ggplotly(
      p1,
      tooltip = "text"
    )
    
    p2 <- ggplotly(
      p2,
      tooltip = "text"
    )
    
    p3 <- ggplotly(
      p3,
      tooltip = "text"
    )
    
    
    result <- subplot(
      p1,
      p2,
      p3,
      nrows = 3,
      shareX = FALSE
    ) %>%
      layout(
        title = list(
          text = paste0(
            main_title,
            "<br>",
            "<sup>",
            sub_title,
            "</sup>"
          ),
          x = 0.05,
          xanchor = "left",
          pad = list(
            t = 10,
            b = 20
          )
        ),
        margin = list(
          t = 70,
          l = 70,
          r = 20,
          b = 50
        ),
        plot_bgcolor = "#ffffff"
      )
    
    
    return(
      make_output(result)
    )
  }
  
  
  
  # FORECAST -----------------------------------------------------------
  
  pxweb_query_forecast <- list(
    "Region" = geografi,
    "ContentsCode" = c(
      "000004KN",
      "000004KQ",
      "000004LN"
    )
  )
  
  
  px_data_forecast <- pxweb_get(
    url = paste0(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/",
      "START/BE/BE0401/BE0401A/BefProgOsiktNetN"
    ),
    query = pxweb_query_forecast
  )
  
  
  bef_forecast <- 
    as.data.frame(
      px_data_forecast,
      column.name.type = "text",
      variable.value.type = "text"
    ) %>%
    clean_names() %>%
    mutate(
      ar = as.Date(
        paste0(ar, "-01-01")
      )
    ) %>%
    filter(
      ar > latest_actual_date,
      ar < f_yr
    )
  
  
  
  # COMBINE ACTUAL + FORECAST ------------------------------------------
  
  fodelse_c <- full_join(
    fodelseoverskott,
    bef_forecast %>%
      select(
        ar,
        fodelseoverskott
      ),
    by = "ar"
  )
  
  
  flytt_c <- full_join(
    inrikesflyttningsnetto,
    bef_forecast %>%
      select(
        ar,
        inrikes_flyttningsnetto
      ),
    by = "ar"
  )
  
  
  invandring_c <- full_join(
    invandringsoverskott,
    bef_forecast %>%
      select(
        ar,
        invandringsnetto
      ),
    by = "ar"
  )
  
  
  
  # VISUALIZE ACTUAL + FORECAST ----------------------------------------
  
  p1 <- ggplot(
    fodelse_c,
    aes(
      x = ar,
      text = paste0(
        "<b>", region_name, "</b>",
        "<br>", year_label, ": ", format(ar, "%Y"),
        "<br>", birth_actual, ": ",
        antal_personer,
        "<br>", birth_forecast, ": ",
        round(fodelseoverskott, 0)
      )
    )
  ) +
    geom_line(
      aes(y = antal_personer),
      group = 1,
      color = "#954b97"
    ) +
    geom_line(
      aes(y = fodelseoverskott),
      group = 1,
      linetype = "dashed",
      color = "#954b97"
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
    theme_classic()
  
  
  
  p2 <- ggplot(
    flytt_c,
    aes(
      x = ar,
      group = 1,
      text = paste0(
        "<b>", region_name, "</b>",
        "<br>", year_label, ": ", format(ar, "%Y"),
        "<br>", migration_actual, ": ",
        antal_personer_sum,
        "<br>", migration_forecast, ": ",
        round(inrikes_flyttningsnetto, 0)
      )
    )
  ) +
    geom_line(
      aes(y = antal_personer_sum),
      color = "#eb6209"
    ) +
    geom_line(
      aes(y = inrikes_flyttningsnetto),
      linetype = "dashed",
      color = "#eb6209"
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
    theme_classic()
  
  
  
  p3 <- ggplot(
    invandring_c,
    aes(
      x = ar,
      text = paste0(
        "<b>", region_name, "</b>",
        "<br>", year_label, ": ", format(ar, "%Y"),
        "<br>", immigration_actual, ": ",
        antal_personer,
        "<br>", immigration_forecast, ": ",
        round(invandringsnetto, 0)
      )
    )
  ) +
    geom_line(
      aes(y = antal_personer),
      group = 1,
      color = "#ffcc00"
    ) +
    geom_line(
      aes(y = invandringsnetto),
      group = 1,
      linetype = "dashed",
      color = "#ffcc00"
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
    theme_classic()
  
  
  
  p1 <- ggplotly(
    p1,
    tooltip = "text"
  )
  
  p2 <- ggplotly(
    p2,
    tooltip = "text"
  )
  
  p3 <- ggplotly(
    p3,
    tooltip = "text"
  )
  
  
  
  result <- subplot(
    p1,
    p2,
    p3,
    nrows = 3,
    shareX = FALSE
  ) %>%
    layout(
      title = list(
        text = paste0(
          main_title,
          "<br>",
          "<sup>",
          sub_title,
          "</sup>"
        ),
        x = 0.05,
        xanchor = "left",
        pad = list(
          t = 10,
          b = 20
        )
      ),
      margin = list(
        t = 70,
        l = 70,
        r = 20,
        b = 50
      ),
      plot_bgcolor = "#ffffff"
    )
  
  
  return(
    make_output(result)
  )
}