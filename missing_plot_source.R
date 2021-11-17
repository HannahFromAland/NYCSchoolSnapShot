library(tidyverse)
library(patchwork)
library(ggnewscale)
library(gridExtra)

missing_plot <- function(dataset,percentage = TRUE) {
  
  missing_patterns <- data.frame(is.na(dataset)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  missing_patterns$row_name <- row.names(missing_patterns)
  
  test <- pivot_longer(missing_patterns,-c(row_name,count),names_to = "variable",values_to = "missing")
  
  na_row <- test %>% 
    mutate(is_mis = case_when(
      missing == TRUE ~ 1,
      missing == FALSE ~ 0
    )) %>% 
    group_by(variable) %>% 
    summarise(n = sum(is_mis)) %>% 
    arrange(desc(n))
  
  test$variable <- factor(test$variable,level = na_row$variable)
  
  test <-group_by(test,row_name) %>% 
    mutate(n = sum(missing)) %>% 
    mutate(complete = case_when(
      n == 0 ~ "yes",
      TRUE ~ "no"
    )
    )
  
  mid_x <- filter(na_row, row.names(na_row) == round(median(as.numeric(row.names(na_row))),0))%>%  .[1, ]
  mid_x <- mid_x$variable
  
  complete <- filter(test,complete == "yes" & variable == mid_x)
  
  na_col <- missing_patterns %>% select(row_name,count) %>% 
    arrange(desc(count))
  test$row_name <- factor(test$row_name,levels = na_col$row_name)
  
  p <- ggplot(test, aes(x = variable, y = fct_rev(row_name), fill = missing)) +
    geom_tile(color = "white") + 
    xlab("variable")+
    ylab("missing pattern")+
    scale_fill_manual(values=c("grey", "#C4C0C0","#A68ED6",NA)) +
    theme(legend.position="none") +
    scale_color_viridis_c(option = "D") +
    new_scale_color()+
    geom_tile(data = test, aes(x=variable, y = fct_rev(row_name), fill = complete),alpha = 0.5) + 
    scale_color_viridis_c(option = "A") +
    geom_text(data = complete, aes(label = "complete cases"),nudge_x = -0.5)
  
  # if percentage == TRUE
  
  row_mis <- missing_patterns %>% 
    select(row_name,count) %>% 
    mutate( per = count / sum(count) *100)
  row_mis$row_name <- factor(row_mis$row_name,levels = na_col$row_name)
  if(percentage) {
    p_col <- ggplot(data = row_mis, aes(x = fct_rev(row_name), y = per)) +
      geom_bar(stat="identity",fill = "lightblue") +
      theme_bw()+
      ylab("% rows")+
      xlab("")+
      scale_y_continuous(limit= c(0,100),breaks = seq(0, 100, 25))+
      coord_flip()
  } else {
    p_col <- ggplot(data = row_mis, aes(x = fct_rev(row_name), y = count)) +
      geom_bar(stat="identity",fill = "lightblue") +
      theme_bw()+
      ylab("number of rows")+
      xlab("")+
      coord_flip()
  }

    
  
  col_mis <- as.data.frame(sapply(dataset, function(x) sum(is.na(x)))) %>% setNames(c("sum"))
  col_mis$variable <- row.names(col_mis)
  
  n <- sum(row_mis$count)
  
  col_mis <- mutate(col_mis,per = sum/n *100)
  
  col_mis$variable <- factor(col_mis$variable,level = na_row$variable)
  
  if (percentage) {
    p_row <- 
      ggplot(data = col_mis, aes(x = variable, y = per)) +
      geom_bar(stat="identity",fill = "lightblue") +
      theme_bw()+
      ylab("% rows")+
      xlab("")+
      scale_y_continuous(limit= c(0,100),breaks = seq(0, 100, 25))
  } else {
    p_row <-     ggplot(data = col_mis, aes(x = variable, y = sum)) +
      geom_bar(stat="identity",fill = "lightblue") +
      theme_bw()+
      ylab("number of rows")+
      xlab("")
  }
  
  p_blank <- ggplot() +geom_blank()+
    theme_void()
  
  final <- p_row + p_blank + p + p_col + plot_layout(widths = c(4, 1),heights = c(1,4))
  
  print(final)
  }

