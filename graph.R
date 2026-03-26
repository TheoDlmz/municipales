if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               stringr, data.table, stringdist, gridExtra)

couleurs = c("LEXG" = "#bb0000",
             "LFI" = "#cc2443",
             "LCOM" = "#dd0000",
             "LSOC" = "#FF8080",
             # "LRDG" = "#ffd1dc",
             "LDVG" = "#ffc0c0",
             "LUG" = "#cc6666",
             "LVEC" = "#00c000",
             "LECO" = "#77ff77",
             "LDIV" = "#8c8c8c",
             "LREG" = "#DCBFA3",
             # "LGJ" = "#ffff00",
             "LREN" = "#ffeb00",
             "LMDM" = "#ff9900",
             "LUDI" = "#00FFFF",
             "LHOR" = "#66ccff",
             "LUC" = "#F3D79A",
             "LDVC" = "#FAC577",
             "LLR" = "#0066cc",
             "LUD" = "#82A2C6",
             "LDVD" = "#adc1fd",
             "LDLF" = "#0082C4",
             "LRN" = "#0D378A",
             "LUDR" = "#3333cc",
             "LDSV" = "#0000ff",
             
             "LEXD" = "#404040",
             "LREC" = "#1a1a1a",
             "LUXD" = "#666699",
             
             "LNC" = "#dddddd")

df_resultats = readRDS(file = "C:/Users/tdelema/Documents/projects/municipales/resultats.rds")

# show_boxes -> TRUE si on veut numéroter les sièges en bas
# n_legend : nombre de lignes de la légende
# distribution : FALSE si on veut représenter juste le transparent
plot_fusion = function(ville = "Belley", distribution = T,show_boxes = F,
                       # legend_order = NULL,
                       nom_liste = "", departement = "", lab = T, col = "",
                       legend = "", n_legend = 1){
  # On filtre les données
  liste_villes = unique(df_resultats$commune)
  ville = liste_villes[amatch(str_to_lower(ville), str_to_lower(liste_villes), maxDist = Inf)]  # Fuzzy match pour les fautes sur les villes
 
   if (departement != ""){
    df = subset(df_resultats, commune == ville & dep == departement)
  } else {
    df = subset(df_resultats, commune == ville)
  }
  
  if(nrow(df) < 2){stop("Cette ville n'a pas de second tour ou n'existe pas")}
  
  df = df %>% 
    filter(fusion == 1) %>%
    filter(nb_tours_present == 2)
  
  if(nrow(df) < 2){stop("Pas de fusion dans cette ville")}
  
  if (length(unique(df$tete_liste_t2)) > 1 & nom_liste == ""){
    
    print("Plusieurs fusions dans cette commune, sélection d'une au hasard")
    df = df %>%
      filter(prop_index_fusion == min(prop_index_fusion))%>%
      ungroup()
    
  } else if (length(unique(df$tete_liste_t2)) > 1 & nom_liste != ""){
    df = df %>%
      filter(liste == nom_liste)
  }
  
  # Création du tableau de données
  
  # Nombre de sièges au total
  nb_sieges_total = unique(df$length_list)
  data = data.frame()
  
  for (simulation in 1:nb_sieges_total){
    data = bind_rows(data, df %>% 
                       filter(num_candidat_t2 <= simulation) %>% 
                       group_by(tete_liste_t1) %>% 
                       summarise(x=n()) %>%
                       pivot_wider(names_from = tete_liste_t1, values_from = x))
  }
  
  data = data[, order(names(data))]

  # On va rajouter les noms de listes et les nuances
  liste_correspondante <- setNames(df$liste_ext, df$tete_liste_t1)[colnames(data)]
  colnames(data) <- liste_correspondante
  data = rbind(0, data)
  
  # Replace NA with 0
  for(j in 1:ncol(data)){
    data[[j]][is.na(data[[j]])] = 0}
  
  data$nb_sieges <- as.numeric(row.names(data))-1
  
  # On colore en fonction des nuances, si elles existent
  if (all(is.na(unique(df$nuance)))){
    scores = df %>% group_by(tete_liste_t1) %>%
      summarise(score = na.omit(unique(t_voix_exprimes)),
                nuance = "NC",
                liste = unique(liste_ext)) %>%
      mutate(score = score/sum(score)) %>%
      arrange(tete_liste_t1)
  } else {
    scores = df %>% 
      group_by(tete_liste_t1) %>% 
      summarise(score = na.omit(unique(t_voix_exprimes)), 
                nuance = na.omit(unique(nuance)),
                liste = unique(liste_ext)) %>%
      mutate(score = score/sum(score)) %>%
      arrange(tete_liste_t1)
  }
  
  if (any(col == "")){
    scores = scores %>%
      mutate(col = couleurs[nuance])
  } else {
    scores = scores %>%
      mutate(col = recode(liste, !!!col))
  }
  scores = scores %>%
    select(tete_liste_t1, liste, nuance, col, score) %>%
    arrange(tete_liste_t1)
  

  
  data_long <- tidyr::pivot_longer(data, cols = -nb_sieges, names_to = "column", values_to = "value") %>%
    rename(iteration = nb_sieges,
           liste = column) %>%
    left_join(
      expand.grid(iteration = 0:max(.$iteration), liste = scores$liste) %>%
        left_join(scores, by = "liste") %>%
        mutate(
          lb = floor(score * iteration),
          up = ceiling(score * iteration)
        ) %>%
        select(iteration, liste, lb, up, score),
      by = c("iteration", "liste")
    )%>%
    left_join(scores %>% select(liste, nuance, col)) %>%
    rename(column = liste,
           nb_sieges = iteration)
  if (!length(unique(data_long$col)) == length(unique(data_long$column))){
    library(scales)
    
    cols_default <- hue_pal()(length(unique(data_long$column)))
    
    data_long <- data_long %>%
      mutate(column = as.factor(column)) %>%
      group_by(column) %>%
      mutate(col = cols_default[as.numeric(column)]) %>%
      ungroup()
    } 
  vec_orders = data_long %>%
    group_by(column) %>%
    arrange(value)%>%
    mutate(increase = value > lag(value)) %>%
    ungroup() %>%
    filter(!is.na(increase)) %>%
    filter(increase) %>% 
    select(nb_sieges, col)
  vec_orders = rbind(data.frame(nb_sieges = 0, col = "grey"), vec_orders)%>%
    arrange(nb_sieges)
  # data_long = merge(data_long, 
  #                   scores, by.x = c("column"), by.y = c("liste"), all = T)
  
  print(unique(data_long$column))

  if (any(legend != "")){
    data_long = data_long %>%
      mutate(column = recode(column, !!!legend))
    scores = scores %>%
      mutate(liste = recode(liste, !!!legend))
   
  } else {
    data_long = data_long %>% 
      arrange(column) %>% 
      mutate(column = paste0(str_to_sentence(column), " (", nuance, ")"))
    scores = scores  %>%
      mutate(liste = paste0(str_to_sentence(liste), " (", nuance, ")"))
    }
  # Représentation du graphique
  # plot <- ggplot() + 
  #   labs(x = "Nombre de sièges de la liste finale",
  #        y = "Nombre de sièges obtenus par chaque sous-liste",
  #        color = "Liste : ", title = paste0("Ville : ", ville))+
  #   theme_bw()+
  #   theme(legend.position="bottom", legend.direction="vertical", plot.title = element_text(hjust = 0.5)) +
  #   scale_y_continuous(expand=c(0,0))+
  #   scale_x_continuous(expand=c(0,0))+
  #   geom_step(data = data_long, linewidth = 1, aes(x = nb_sieges, y = value, color = column))
  cols <- unique(data_long[, c("column", "col")])
  cols <- setNames(cols$col, cols$column)
  margin <- 0.3

  # data_long_step <- data_long %>%
  #   arrange(column, nb_sieges) %>%
  #   group_by(column) %>%
  #   mutate(next_x = lead(nb_sieges)) %>%
  #   filter(!is.na(next_x)) %>%
  #   mutate(
  #     x_start = nb_sieges,
  #     x_end   = next_x - margin
  #   ) %>%
  #   uncount(2) %>%
  #   mutate(
  #     nb_sieges = ifelse(row_number() %% 2 == 1, x_start, x_end)
  #   ) %>%
  #   ungroup()
  data_long_step <- data_long %>%
    arrange(column, nb_sieges) %>%
    group_by(column) %>%
    mutate(xend = lead(nb_sieges)) %>%
    filter(!is.na(xend)) %>%
    tidyr::uncount(2) %>%
    mutate(nb_sieges = ifelse(row_number() %% 2 == 1, nb_sieges, xend)) %>%
    ungroup()
  data_long_step_2 <- data_long_step
  data_long_step = data_long_step %>%
    mutate(nb_sieges = ifelse(nb_sieges != 0, nb_sieges - margin, nb_sieges))
  data_long_step_2 = data_long_step_2 %>%
    group_by(column, nb_sieges) %>%
    # mutate(lb = ifelse(row_number() == 1, lb + 2*0.3, lb))%>%
    mutate(nb_sieges = ifelse(nb_sieges != 0, nb_sieges + margin, nb_sieges)) #%>%
    # ungroup()
  data_long_step_3 <- bind_rows(data_long_step, data_long_step_2) %>%
    group_by(nb_sieges, column)# %>%
    # filter(
    #   !near(nb_sieges %% 1, margin) | xend == floor(nb_sieges) + 1
    # ) %>%
    # ungroup()
  
  margin <- 0.3
  
  rectangles <- data_long %>%
    arrange(column, nb_sieges) %>%
    group_by(column) %>%
    mutate(
      x_next = lead(nb_sieges),
      lb_next = lead(lb),
      up_next = lead(up)
    ) %>%
    filter(!is.na(x_next)) %>%
    tidyr::uncount(2, .id = "rect_type") %>%
    mutate(
      xmin = case_when(
        rect_type == 1 ~ nb_sieges + margin,
        rect_type == 2 ~ x_next - margin
      ),
      xmax = case_when(
        rect_type == 1 ~ x_next - margin,
        rect_type == 2 ~ x_next + margin
      ),
      ymin = case_when(
        rect_type == 1 ~ lb,
        rect_type == 2 ~ lb_next
      ),
      ymax = case_when(
        rect_type == 1 ~ up,
        rect_type == 2 ~ up_next
      )
    ) %>%
    ungroup()%>%
    group_by(nb_sieges, column) %>%
    arrange(rect_type) %>%
    mutate(ymin = ifelse(ymin != lag(ymin) & rect_type == 2, lag(ymin), ymin))
    # mutate()
  legend_order <- scores %>%
    arrange(desc(score)) %>%
    pull(liste)
  
  data_long$column <- factor(data_long$column, levels = legend_order)
  rectangles$column <- factor(rectangles$column, levels = legend_order)
  
  # data_long_step_3 =  dplyr::bind_rows(data_long_step, data_long_step_2) %>%
  #   arrange(column, nb_sieges)
  #%>%
    # group_by(column, nb_sieges) %>%
    # arrange(nb_sieges, lb) %>%
    # mutate(nb_sieges = case_when(nb_sieges == 0 ~ nb_sieges,
    #                              row_number() == 1 ~ nb_sieges - margin,
    #                              TRUE ~ nb_sieges + margin,
    #                              ))
  plot = ggplot() + 
    labs(x = expression("Nombre de sièges " * italic('k')),
         y = "Part de chaque sous-liste",
         color = "",
         fill = "",
         # title = paste0("Ville : ", ville)
         ) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +#, limits = c(-1,3)
    # scale_fill_manual(values = cols)+
    
    geom_rect(
      data = rectangles,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin - 0.6,
        ymax = ymax + 0.6,
        fill = column
      ),
      alpha = 0.3 + (1 - distribution) * 0.2
    )
   

  if (distribution){
    plot =  plot +
      geom_step(
        data = data_long,
        aes(x = nb_sieges, y = value, color = column, group = column),
        linewidth = 1
      )+geom_rect(
        data = vec_orders,
        aes(
          xmin = nb_sieges,
          xmax = nb_sieges + 1,
          ymin = -1.85,
          ymax = -0.15
        ),
        fill = vec_orders$col,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    if (show_boxes){
      plot = plot +  
        geom_text(
          data = vec_orders,
          aes(
            x = nb_sieges + 0.5,
            y = -1,
            label = nb_sieges
          ),
          size = 3,
          inherit.aes = FALSE,
          show.legend = FALSE
        )+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    } 
  }
  
  plot = plot +
    theme(
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    ) +
    guides(
      color = guide_legend(nrow = n_legend),
      fill  = guide_legend(nrow = n_legend)
    ) +
    scale_color_manual(values = cols, breaks = legend_order) +
    scale_fill_manual(values = cols, breaks = legend_order)
 
    
    
  # # On ajoute les lignes pointillées
  # if (length(unique(data_long$col)) == length(unique(data_long$column))){
  #   plot <- plot +
  #     geom_abline(data = scores, linewidth = 1,
  #                 aes(slope = score, color = liste, intercept = 0), 
  #                 linetype = "dashed", show.legend = T) +
  #     scale_color_manual(values = unique(data_long$col))
  #   
  # } else {
  #   plot <- plot +
  #     geom_abline(data = scores, linewidth = 1,
  #                 aes(slope = score, color = liste, intercept = 0), 
  #                 linetype = "dashed", show.legend = T)
  # }
  plot
  return(list("graph" = plot, "data" = data_long %>%
                rename("Nb sieges sur la fusion (xaxis)" = nb_sieges,
                       "Nb sieges obtenus (yaxis)" = value,
                       "Couleur" = col)))
}

# Exemple d'utilisation
# 1/ Pour print le nom des listes et une v1 du graphique 
plot_fusion("Toulouse") 
# 2/ Pour personnaliser les couleurs et le nom des listes

plot_fusion("Reims",
            col = c("LES RÉMOIS AU COEUR - Liste d'Union de la Droite, du Centre et des Indépendants" 
                    = "#adc1fd",
                    "POUR REIMS UNE NOUVELLE ÈRE AVEC ANNE-SOPHIE FRIGOUT" = "#404040"
            ),
            legend = c("LES RÉMOIS AU COEUR - Liste d'Union de la Droite, du Centre et des Indépendants" 
                       = "Lang (DVD)",
                       "POUR REIMS UNE NOUVELLE ÈRE AVEC ANNE-SOPHIE FRIGOUT" = "Frigout (RN)"
            )
)
