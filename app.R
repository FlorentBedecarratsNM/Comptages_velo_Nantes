library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(prophet)
library(cowplot)
library(DT)

# Load ------------------------------------------------------------

# Location of Nantes bike open datasets
url_c <- "https://data.nantesmetropole.fr/explore/dataset/244400404_comptages-velo-nantes-metropole/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
url_h <- "https://data.nantesmetropole.fr/explore/dataset/244400404_comptages-velo-nantes-metropole-historique-jour/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
url_l <- "https://data.nantesmetropole.fr/explore/dataset/244400404_comptages-velo-nantes-metropole-boucles-comptage/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"

load_historic <- function(hist_file = url_h) {
    count_2014_19 <- read_delim(hist_file, skip = 1,delim = ";", 
               col_names = c("id", "date", "name", "count", "anom", "fitted" ))
    save(count_2014_19, file = "data/count_2014_19.rda")
    return(count_2014_19)
}

load_current <- function(current_file = url_c) {
    count_2020 <- read_delim(current_file,
               delim = ";", skip = 1,
               col_names = c("id", "name", "date", 0:23, "anom", "weekday" )) %>%
        mutate(name = ifelse(is.na(name), paste0("Sans nom : ", id), name),
               anom = ifelse(is.na(anom), "fonctionnel", anom),
               anom = recode_factor(anom,
                                    fonctionnel = "Fonctionnel",
                                    Faible = "Faible probabilité d'anomalie",
                                    Forte = "Forte probabilité d'anomalie",
                                    .ordered = TRUE))
    save(count_2020, file = "data/count_2020.rda")
    return(count_2020)
}

prep_feries <- function(file_holidays = "jours_feries.csv") {
    jours_feries <- read_csv(file_holidays) %>%
        filter(annee %in% 2014:2022) %>%
        select(ds = date, holiday = nom_jour_ferie)
}

# Plot anomalies ---------------------------------------------
plot_anom_day <- function(x, n_days = NA, omit_last_day = TRUE) {
    if (omit_last_day) {
        x <- x %>%
            filter(date != max(date))
    }
  # Filter dates
  if (!is.na(n_days)) {
    x <- x %>% 
      filter(date >= (max(date) - n_days))
  }
    x %>% 
        group_by(date) %>% 
        summarise(anom_high = mean(anom == "Forte probabilité d'anomalie"),
                  anom_low = mean(anom == "Faible probabilité d'anomalie")) %>%
        ggplot(aes(x = date)) + 
        geom_area(aes(y = anom_low + anom_high, 
                      fill = "Faible probabilité d'anomalie")) +
        geom_area(aes(y = anom_high, 
                      fill = "Forte probabilité d'anomalie")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b", 
                     minor_breaks = NULL) +
        scale_y_continuous(labels = percent) + 
        theme(axis.title.y = element_blank(),
              axis.title.x= element_blank(),
              legend.position = "bottom") +
        ggtitle("Part des compteurs en anomalie") +
        scale_fill_manual(name="", 
                          values = c("Faible probabilité d'anomalie" = "#fee08b",
                                     "Forte probabilité d'anomalie" = "#f46d43"))
    
}


plot_anom_counter <- function(x, n_counters = 20, n_days = NA,
                              omit_last_day = TRUE) {
    if (omit_last_day) {
        x <- x %>%
            filter(date != max(date))
    }
    # Filter dates
    if (!is.na(n_days)) {
        x <- x %>% 
            filter(date >= (max(date) - n_days))
    }
    # Anomaly rater per counter
    counters <- x %>%
        group_by(name) %>% 
        summarise(high_anom_rate = mean(anom == "Forte probabilité d'anomalie"),
                  anom_rate = mean(anom != "Fonctionnel")) %>%
        arrange(anom_rate) %>%
        ungroup()
    # counter with highest anomalies
    x <- x %>% 
        filter(name %in% counters$name[(nrow(counters)-n_counters):nrow(counters)])
    # Plotting parameters
    timespan <- max(x$date) - min(x$date)
    d_breaks <- ifelse(timespan > 60, "1 month", "1 week")
    d_labels <- ifelse(timespan > 60, "%b", "%d/%m")
    # Plot
    x %>%
        ggplot(aes(x = date, y = factor(name, levels = counters$name, ordered = TRUE),
                   fill = anom)) + 
        geom_tile(colour = "white", size = 0.6) +
        scale_fill_manual(values=c("Fonctionnel" = "#abdda4",
                                   "Faible probabilité d'anomalie" = "#fee08b",
                                   "Forte probabilité d'anomalie" = "#f46d43")) +
        scale_x_date(date_breaks = d_breaks, date_labels = d_labels, 
                     minor_breaks = NULL) +
        theme(axis.title.y = element_blank(),
              axis.title.x= element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              panel.background = element_blank()) +
        ggtitle(label = "Détail des compteurs en anomalie",
                subtitle = paste0("Les ", n_counters, 
                                  " compteurs avec le plus d'erreurs sur ",
                                  timespan, " jours")) 
}

# Tendances ------------------------------------------------------------

# Cette fonction écarte du calcul de tendance les variables dès lors 
# qu'on a trois valeurs nulles (0) consécutives. Le 0 est alors remplacé
# par une valeur manquante. Ceci afin d'éviter de biaiser l'estimation de 
# tendance lors de pannes prolongées. Les valeurs écartées sont bien sûr
# conservées dans les données brutes.
anom_3nuls <- function(x) {
    x %>%
        arrange(ds) %>%
        mutate(y_orig  = y,
               nul_3_consec = ifelse((y == 0 & lead(y) == 0 & lag(y) == 0) |
                                         (y == 0 & lead(y) == 0 & lead(y, 2) == 0) |
                                         (y == 0 & lag(y) == 0 & lag(y, 2) == 0),
                                     1, 0),
               y = ifelse(nul_3_consec == 1, NA, y))
}

# Cette fonction applique l'algorithme prophet aux données de comptage
mouline_prophet <- function(x, 
                            ylim_min = TRUE, 
                            ecarte_3nuls = TRUE,
                            negatives_to_0 = TRUE,
                            jours_feries = prep_feries(),
                            titre_graph = "Variations récurrentes et détection des anomalies") {
    
    if (ecarte_3nuls == TRUE) {
        x <- anom_3nuls(x)
    }
    
    m <- x %>%
        prophet(holidays = jours_feries, yearly.seasonality = TRUE,
                interval.width = 0.90, 
                changepoints =  c('2015-01-01', '2016-01-01', '2017-01-01',
                                  '2018-01-01', '2019-01-01', '2020-01-01',
                                  '2020-03-17', '2020-05-11', '2020-01-01'),
                # n.changepoints = 50,
                changepoint.prior.scale = 0.5,
                changepoint.range = 1)
    
    future <- make_future_dataframe(m, periods = 365) %>%
        filter(ds >= min(x$ds[!is.na(x$y)], na.rm = TRUE), 
               ds <= max(x$ds[!is.na(x$y)], na.rm = TRUE))
    
    forecast <- predict(m, future) %>%
        filter(ds >= min(x$ds, na.rm = TRUE), ds <= max(x$ds, na.rm = TRUE)) %>%
        mutate(ds2 = as_date(ds)) %>% # prophet change le format en POSIXct
        left_join(x, by = c("ds2" = "ds")) %>%
        mutate(anom = ifelse(y > yhat_upper, 1, ifelse(y < yhat_lower, 1, NA)),
               anom_value = ifelse(is.na(anom), NA, y),
               anom_magnitude = ifelse(anom == 1, (y - yhat_upper)/y,
                                       ifelse(anom == -1, (yhat_lower-y)/y, NA)),
               anom = abs(anom),
               anom_magnitude = abs(anom_magnitude),
               `Probabilité\nd'anomalie\n(indice)` = log(anom_magnitude+1))
    
    # dans certains cas, le modèle cumulatif de forecast peut produire
    # des estimations négatives. Il est recommandé de les passer à 0 quand
    # la variable estimée ne peut pas être négative (comme c'est le cas ici)
    # cf. https://github.com/facebook/prophet, issues #1468, #1454, #1214...
    if (negatives_to_0) {
        forecast$yhat = ifelse(forecast$yhat < 0, 0, forecast$yhat)
    }
    
    
    # On enlève le y de forecast
    forecast$y <- NULL
    # On prépare le graph (un peu complexe, mais c'est plus joli comme ça)
    graph <- m$history %>%
        select(ds, y) %>%
        right_join(forecast, by = "ds") %>%
        arrange(ds) 
    # Si la valeur a été remplacée par NA car 3 valeurs nulles (O) consécutifs,
    # on réintègre la valuer initiale, on marque la valeur comme anormale mais
    # on n'inclut pas de "halo" proportionnel à la proba d'erreur.
    if (ecarte_3nuls) {
        graph <- graph %>%
            mutate(y = y_orig,
                   anom = ifelse(nul_3_consec == 1, 1, anom),
                   anom_value = ifelse(nul_3_consec == 1, y_orig, anom_value))
    }
    graph <- graph %>%
        ggplot(aes(x = ymd(paste("2000", month(ds), day(ds), sep = "-")), 
                   y = y, shape = "Mesure")) +
        labs(y = "nombre de passages à vélo") +
        geom_point(size = 0.3, na.rm=TRUE) +
        scale_shape_manual("", values = 19) +
        geom_line(aes(y = yhat, color = "Modèle"),
                  size = 0.2, na.rm = TRUE) +
        geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper,
                        fill = "Marge\nd'erreur"),
                    alpha = 0.5,
                    na.rm = TRUE) +
        scale_fill_manual("", values = "#0072B2") +
        scale_colour_manual("",values="blue") +
        facet_grid(year(ds)~.) +
        geom_point(aes(y = anom_value, alpha = "Anomalie\npossible"),
                   colour = "red", size = 0.3, shape = 16) +
        scale_alpha_manual("",values = 1) +
        geom_point(aes(size = `Probabilité\nd'anomalie\n(indice)`),
                   colour = "red", alpha = 0.2) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "bottom",
              legend.title = element_text(size = 9),
              plot.title = element_text(size = 11, face = "bold")) + #, suppr ')+
        # legend.position = "none") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        guides(shape = guide_legend(order = 1),
               color = guide_legend(order = 2),
               fill = guide_legend(order = 3),
               alpha = guide_legend(order = 4,
                                    override.aes=list(size = 3)),
               size = guide_legend(nrow = 2, order = 5),
               override.aes=list(size = 3))  +
        ggtitle(titre_graph)
    
    if (ylim_min) {
        graph <- graph + ylim(0, NA)
    }
    output <- list(m, forecast, graph)
    names(output) <- c("model", "forecast", "graph")
    return(output)
}

# Une fonction pour calculer la croissance annuelle
cr_an <- function(pr) {
    m_first <- pr[["forecast"]]$trend[1]
    m_last <- pr[["forecast"]]$trend[nrow(pr[["forecast"]])]
    d_first <- pr[["forecast"]]$ds[1]
    d_last <- pr[["forecast"]]$ds[nrow(pr[["forecast"]])]
    duree <- time_length(d_last - d_first, "year")
    c_tot <- round((m_last - m_first)/m_first * 100)
    c_an <- round(((m_last/m_first)^(1/duree)-1)*100,1)
    return(c_an)
}

# Une fonction pour appliquer le modèle aux diférents compteurs
process_boucle <- function(base, nom, ylim_min = FALSE) {
    # Filtre
    db <- base %>%
        filter(name == nom) %>%
        # retirer el dernier jour (décompte partiel)
       # filter(date != max(date)) %>%
        rename(ds = date, y = count)
    # Mouline en pr
    pr <- mouline_prophet(db, ylim_min, 
                          titre_graph = paste0(
                              "Variations récurrentes et détection des anomalies (",
                              nom, ")"))
    decomp <- prophet_plot_components(pr[["model"]], pr[["forecast"]], render_plot = FALSE)
    croiss_an <- cr_an(pr)
    output <- list(pr, decomp, croiss_an)
    names(output) <- c("modele", "composantes", "stat")
    return(output)
}

merge_current_historic <- function(current, historic) {
    current <- current %>%
        select(-weekday) %>%
        mutate(id = as.numeric(id),
               name = recode(name,
                             "Bonduelle vers sud" = "Pont A. Briand vers Sud",
                             "Bonduelle vers Nord" = "Pont A. Briand vers Nord",
                             "pont Anne de Bretagne vers Sud" = "Pont Anne de Bretagne vers Sud",
                             "pont Anne de Bretagne vers Nord" = "Pont Anne de Bretagne vers Nord",
                             "50 Otages Vers Sud" = "Cours des 50 Otages Vers Sud",
                             # Madeleine était cumulé en 2014-2019, on fait pareil ici
                             "Madeleine vers Sud" = "Chaussée de la Madeleine",
                             "Madeleine vers Nord" = "Chaussée de la Madeleine"),
               # idem pour les identifiant de compteur de la Madeleine
               id = ifelse(id == 880, 881, id)) %>%
        pivot_longer(-c("id", "date", "name",  "anom"),
                     names_to = "hour",
                     values_to = "count") %>%
        group_by(id, date, name, anom) %>%
        summarise(count = sum(count, na.rm = TRUE)) %>%
        mutate(anom = ifelse(anom == "Fonctionnel", 0, 1),
               id = as.numeric(id)) %>%
        filter(id %in% historic$id)
    

    current_names <- current %>%
        ungroup() %>%
        select(id, name) %>%
        unique()
    
    historic <- historic %>%
        select(-name) %>%
        left_join(current_names, by = "id")
    
   bind_rows(current, historic) %>%
       filter(!is.na(name))

}


loop_counters <- function(x) {
    # On récupère les noms de chaque compteur
    counters <- unique(x$name)
    # On enlève Tabarly car problème de date
    counters <- counters[counters != "Pont Tabarly vers Sud"]
    counters <- counters[counters != "Bd Malakoff vers Ouest"]
    counters <- counters[counters != "Bd Malakoff vers Est"]
    # On crée une liste vide
    out <- vector(mode = "list", length = length(counters))
    # On prépare les données pour chaque compteur
    for (i in 1:length(out)) {
        print(counters[i])
        out[[i]] <- process_boucle(x, counters[i])
    }
    names(out) <- counters
    return(out)
}


update_trends <- function() {
    count_2020 <- load_current()
    count_2014_19 <- load_historic()
    trends <- count_2020 %>%
        merge_current_historic(count_2014_19) %>%
        loop_counters()
    save(trends, file = "data/trends.rda")
    return(trends)
}

# Interface utilisateurs -------------------------------------------------------

## UI --------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage(
    "Comptages vélo à Nantes",
    tabPanel(
      "Anomalies",
      # Sidebar with a slider input for number of bins 
      fluidRow(
        column(4,
               checkboxInput("removeLastDay", "Omettre le dernier jour",
                             value = TRUE, width = NULL),
               uiOutput("slider_days"),
               uiOutput("slider_counters"),
               actionButton("update_source", "Mise à jour des données")),
        column(8,
               plotOutput("anom_hist_plot"))
      ),
      fluidRow(
        column(12,
               plotOutput("anom_detail_plot"))
      )
       # Sidebar with a slider input for number of bins 
      # sidebarLayout(
      #   sidebarPanel(
      #     checkboxInput("removeLastDay", "Omettre le dernier jour", 
      #                   value = TRUE, width = NULL),
      #     uiOutput("slider_days"),
      #     uiOutput("slider_counters"),
      #     actionButton("update_source", "Mise à jour des données")
      #   ),
      #   
      #   # Show a plot of the generated distribution
      #   mainPanel(
      #     plotOutput("anom_hist_plot", height = "200px"),
      #     plotOutput("anom_detail_plot")
      #   )
      # )
    ),
    tabPanel(
      "Tendances",
      sidebarPanel(
        uiOutput("select_counter"),
        actionButton("update_trends", "Mise à jour des analyses (patienter 10 min.)")
      ),
      mainPanel(
        plotOutput("trend_tab", height = "800px")
      )
    ),
    tabPanel(
      "Indicateurs",
      column(3,
             wellPanel(
               h4("1) Paramétrer"),
               dateRangeInput('dateRange1',
                              label = 'Période de départ (P1)',
                              start = "2014-01-01", end = Sys.Date(),
                              min = "2014-01-01", max = Sys.Date(),
                               format = "dd/mm/yyyy",
                              separator = " - ",  # format = "dd/mm/yyyy",
                              startview = 'year', language = 'fr', weekstart = 1
               ),
               dateRangeInput('dateRange2',
                              label = "Période d'arrivée (P2)",
                              start = "2014-01-01", end = Sys.Date(),
                              min = "2014-01-01", max = Sys.Date(),
                              format = "dd/mm/yyyy",
                              separator = " - ",  # format = "dd/mm/yyyy",
                              startview = 'year', language = 'fr', weekstart = 1
               ),
               radioButtons("avgMiss", "Traitement des données manquante",
                            choices = list("Les omettre" = "omit", 
                                           "Les remplacer par des projections" = "replace")),
               radioButtons("avgAnom", "Traitement des anomalies probables",
                            choices = list("Les omettre" = "omit", 
                                           "Les remplacer par des projections" = "replace",
                                           "Les conserver en l'état" = "keep")),
               actionButton("filter_counters", "Valider les paramètres")
             )),
      column(7,
             h4("2) Sélectionner les compteurs à inclure (classés par fiabilité sur la période)"),
             actionButton("gen_results", "Générer les résultats"),
             DTOutput("filt_count")
      ),
      column(2,
             h4("3) Résultats"),
             htmlOutput("p1_avg"),
             htmlOutput("p2_avg"),
             htmlOutput("var_p1p2"),
             htmlOutput("pitch")
             
      )
    )
    
  )
)

## Serveur ---------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
   count_current <- reactiveValues(df = NULL)
    # Load current years' file (download if not present)
    if (file.exists("data/count_2020.rda")) {
      load("data/count_2020.rda")
      count_current$df <- count_2020
    } else {
        count_current$df <- load_current()
    }

    # Load historic data (download if not present)
    if (file.exists("data/count_2014_19.rda")) {
        load("data/count_2014_19.rda")
    } else {
        count_2014_19 <- load_historic()
    }

    # Load holidays (download if not present)
    jf <- "https://etalab.github.io/jours-feries-france-data/csv/jours_feries_metropole.csv"
    if (!file.exists("jours_feries.csv")) {
        download.file(url = jf, "jours_feries.csv")
    }
    
    # Charge la donnée sur les tendances
    if (file.exists("data/trends.rda")) {
        load("data/trends.rda")
    } else {
        trends <- update_trends()
    }
    
# On crée un fichier count avec toutes les valeurs
    
    for (i in 1:length(trends)) {
        temp <- tibble(
            date = trends[[i]][["modele"]][["forecast"]][["ds"]],
            compteur = trends[[i]][["modele"]][["forecast"]][["name"]],
            comptage_releve = trends[[i]][["modele"]][["forecast"]][["y_orig"]],
            anomalie = trends[[i]][["modele"]][["forecast"]][["anom"]],
            comptage_ajuste = round(trends[[i]][["modele"]][["forecast"]][["yhat"]]))
        if (i == 1) {
            count <- temp
        } else {
            count <- bind_rows(count, temp)
        }
    }
    
    output$trend_tab <- renderPlot({
        boucle <- trends[[input$select_counter]]
        graph <- boucle$modele$graph 
        j <- length(boucle$composantes)
        trend <- boucle$composantes[[1]] +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        weekly <- boucle$composantes[[j-1]] +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        yearly <- boucle$composantes[[j]] +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        comp <- plot_grid(weekly, yearly, trend, nrow = 1)
        plot_grid(comp, graph, ncol = 1, rel_heights = c(1,6))
    })
    
    observeEvent(input$update_source, {
        showModal(modalDialog("Veuillez patienter, l'application télécharge les données les plus récentes",
                              footer=NULL))
        count_current$df <- load_current()
        removeModal()
    })
    
    observeEvent(input$update_trends, {
        trends <- update_trends()
    })

    output$anom_hist_plot <- renderPlot({
    #    count_2020 <- prep_current("bikecount_currentyear.csv",
    #                               omit_last_day = input$removeLastDay) 
        count_current$df %>%
            plot_anom_day(omit_last_day = input$removeLastDay,
                          n_days = input$slider_days)
        })
    
    output$slider_days <- renderUI({
        maxdate <- max(count_current$df$date)
        mindate <- min(count_current$df$date)
        
        sliderInput("slider_days", min   = 1, 
                    label = "Nombre de jours à inclure dans le détail des compteurs",
                    max   = as.numeric(maxdate-mindate),
                    value = 200)
    })
    
    output$slider_counters <- renderUI({
        nb_counters <- length(unique(count_current$df$name))
        
        sliderInput("slider_counters", min   = 1, 
                    label = "Nombre de boucles à inclure dans le détail des compteurs",
                    max   = nb_counters,
                    value = 20,
                    step = 1, 
                    round = TRUE)
    })
    output$select_counter <- renderUI({
        list_boucles <- sort(names(trends))
        selectInput("select_counter",  
                    label = "Compteur",
                    choices = as_list(list_boucles))
    })
    output$anom_detail_plot <- renderPlot({
        count_current$df %>%
            plot_anom_counter(
                n_counters = input$slider_counters, 
                n_days = input$slider_days,
                omit_last_day = input$removeLastDay)
        })
    observeEvent(input$filter_counters, {
      filt_count <<- count %>%
        group_by(compteur) %>%
        summarise(miss_p1 = sum(is.na(comptage_releve) & date >= input$dateRange1[1] & date <= input$dateRange1[2]),
                  anom_p1 = sum(anomalie == 1 & date >= input$dateRange1[1] & date <= input$dateRange1[2], na.rm = TRUE),
                  miss_p2 = sum(is.na(comptage_releve) & date >= input$dateRange2[1] & date <= input$dateRange2[2]),
                  anom_p2 = sum(anomalie == 1 & date >= input$dateRange2[1] & date <= input$dateRange2[2], na.rm = TRUE)) %>%
        arrange(desc(max(miss_p1 + anom_p1, miss_p2 + anom_p2))) %>%
        rename(Compteur = compteur, `Manquantes\n(P1)` = miss_p1, `Anomalies\n(P1)` = anom_p1,
               `Manquantes\n(P2)` = miss_p2, `Anomalies\n(P2)` = anom_p2)
      output$filt_count <- renderDT(filt_count, options = list(dom = "t", pageLength = 30, autoWidth = TRUE),
                                    rownames = FALSE) 
    })
      
    

    observeEvent(input$gen_results, {
      selected <- filt_count[input$filt_count_rows_selected,1]
      p1_avg_tb <- count %>%
        filter(compteur %in% selected$Compteur & date >= input$dateRange1[1] & date <= input$dateRange1[2]) %>%
        mutate(count = comptage_releve,
               count = ifelse(input$avgMiss == "replace" & is.na(count), comptage_ajuste, count),
               count = ifelse(input$avgAnom == "replace" & !is.na(anomalie), comptage_ajuste, count),
               count = ifelse(input$avgAnom == "omit" & !is.na(anomalie), NA, count)) %>%
        summarise(mean = round(mean(count, na.rm = TRUE), 1))
      save(p1_avg_tb, file = "p1_avg_tb .rda") 
       p1_avg <- p1_avg_tb[[1]]

      p2_avg_tb <- count %>%
        filter(compteur %in% selected$Compteur & date >= input$dateRange2[1] & date <= input$dateRange2[2]) %>%
        mutate(count = comptage_releve,
               count = ifelse(input$avgMiss == "replace" & is.na(count), comptage_ajuste, count),
               count = ifelse(input$avgAnom == "replace" & !is.na(anomalie), comptage_ajuste, count),
               count = ifelse(input$avgAnom == "omit" & !is.na(anomalie), NA, count)) %>%
        summarise(mean = round(mean(count, na.rm = TRUE), 1))
      p2_avg <- p2_avg_tb[[1]]

      var_p1p2 = ifelse(p2_avg >= p1_avg,
                        paste0("+", round((p2_avg -p1_avg)/p1_avg*100, 1), "%"),
                        paste0(round((p2_avg -p1_avg)/p1_avg*100, 1), "%"))

      pitch <- paste0("Variation de moyennes de comptages entre la période allant du ",
                      input$dateRange1[1], " au ",  input$dateRange1[2],
                      " et la période allant du ",
                      input$dateRange2[1], " au ", input$dateRange2[2],
                      ". Calculs effectués sur ", length(input$filt_count_rows_selected),
                      " compteurs exploitables. Les données manquantes ont été ",
                      ifelse(input$avgMiss != "replace", "omises. ",
                             "remplacées par des projections. "),
                      "Les données identifiées commme de probables anomalies on été ",
                      ifelse(input$avgAnom == "omit", "omises.",
                             ifelse(input$avgAnom == "keep", "conservées.",
                                    "remplacées par des projections.")))

      output$p1_avg <- renderUI({
        HTML(paste("Moyenne sur P1", p1_avg, sep = "<br/>"))
      })
      output$p2_avg  <- renderUI({
        HTML(paste("Moyenne sur P2", p2_avg, sep = "<br/>"))
      })
      output$var_p1p2 <- renderUI({
        HTML(paste("Variation entre P1 et P2", var_p1p2, sep = "<br/>"))
      })
      output$pitch <- renderUI({
        HTML(paste("Précision méthodologique :", pitch, sep = "<br/>"))
     })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
