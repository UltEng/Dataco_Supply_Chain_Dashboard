function(input, output) { 
  
  
  
  output$pie1 <- renderEcharts4r({
    segm_chart <- scm %>%
      group_by(get(input$variable1)) %>%
      summarise(n = n()) %>%
      rename(
        "Total Data" = n
      )
    
  pie1 <- segm_chart %>%
    e_charts(get(input$variable1)) %>%
    e_pie(`Total Data`, radius = c("50%", "70%")) %>%
    e_title(paste(str_to_title(str_replace(input$variable1, "_", " "),locale ='en'), "Composition"),left = "center") %>%
    e_tooltip() %>%
    e_labels(show = TRUE,
              formatter = "{b} \n {d}%",
              position = "outside",
              fontSize = 10
      ) %>%
    e_legend(orient = "vertical",
               left = "right",
               top = "50px")
    
  })
  output$bar1 <- renderEcharts4r({
    order_cat  <- scm %>%
      group_by(get(input$variable2)) %>%
      summarise(n = n()) %>%
      rename(
        "total_order" = n
      ) %>%
      top_n(5) %>%
      arrange(desc(total_order))
    
    bar1 <- order_cat %>%
      e_charts(get(input$variable2)) %>%
      e_bar(total_order, 
            name = "total_order", 
            legend = FALSE
      ) %>%
      e_labels(position = "top"
      ) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(axisTick = FALSE, fontSize = 6, axisLabel = list(interval=0)) %>%
      e_tooltip() %>%
      e_color("#163ec2") %>%
      e_title(paste("Top 5", str_to_title(str_replace(input$variable2, "_", " "),locale ='en'), "by Order Count"),
              left = "center")
    
  })
  
 
  output$line1 <- renderEcharts4r({
    sum_sales <- scm %>%
      mutate(date = lubridate::floor_date(order_date, input$variable5)) %>%
      group_by(date)%>%
      summarise(total_sales = sum(sales))

    line1 <- sum_sales %>%
      e_charts(date) %>%
      e_line(total_sales) %>%
      e_legend(orient = "horizontal",
               bottom = "bottom"
      ) %>%
      e_tooltip(trigger = 'axis') %>%
      e_title(paste(str_to_title(str_replace(input$variable5, "_", " "),locale ='en'), "Sales Trend"),
              left = "center")
    
  }) 
  
  output$bar3 <- renderEcharts4r({
    ship_date_duration_dist  <- scm %>%
      group_by(total_shipping_days) %>%
      summarise(n = n()) %>%
      rename(
        "total" = n
      )
    
    bar3 <- ship_date_duration_dist %>%
      e_charts(total_shipping_days) %>%
      e_bar(total, 
            name = "total", 
            legend = FALSE
      ) %>%
      e_labels(position = "top"
      ) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(axisTick = FALSE) %>%
      e_tooltip() %>%
      e_color("#163ec2") %>%
      e_title(text = "Ship Duration Distribution",
              left = "center")
    
  }) 
  
  
  output$hist1 <- renderEcharts4r({
    sales_dist <- scm
    
    hist1 <- sales_dist %>%
      e_charts() %>%
      e_histogram(serie = sales, name = "histogram") %>%
      e_density(
        serie = sales,
        areaStyle = list(opacity = .4), 
        smooth = TRUE, 
        name = "density", 
        y_index = 1
      ) %>%
      e_y_axis(index = 1, show = FALSE) %>%
      e_title(text = "Sales Distribution",
              left = "center") %>%
      e_legend(top = "9%") %>%
      e_tooltip(trigger = "axis") %>%
      e_hide_grid_lines(which = "x")
    
  }) 

  
  output$bar4 <- renderEcharts4r({
    sales_cat <- scm %>%
      group_by(get(input$variable3)) %>%
      summarise(total_sales = sum(sales)) %>%
      top_n(5) %>%
      arrange(desc(total_sales))
    
    bar4 <- sales_cat %>%
      e_charts(get(input$variable3)) %>%
      e_bar(total_sales, 
            name = "total_sales", 
            legend = FALSE
      ) %>%
      e_labels(position = "top"
      ) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(axisTick = FALSE, fontSize = 6, axisLabel = list(interval=0)) %>%
      e_tooltip() %>%
      e_color("#163ec2") %>%
      e_title(paste("Top 5", str_to_title(str_replace(input$variable3, "_", " "),locale ='en'), "with most sales"),
              left = "center")
    
    
  }) 
  
  output$violin1 <- renderPlotly({
    violin_chart <- scm  %>%
      plot_ly(
        x = ~(get(input$variable4)),
        y = ~sales,    
        split = ~(get(input$variable4)),
        type = 'violin',
        spanmode = "hard",
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    violin1 <- violin_chart %>%
      layout(
        title = paste("<b>Sales distribution by", str_to_title(str_replace(input$variable4, "_", " <b>"),locale ='en')),
        font = list(
          family = "sans-serif",
          size = 13,
          color = 'black'),
        separators = ", ",
        xaxis = list(
          title = ''),
        yaxis = list(
          title = "",
          zeroline = F,
          tickformat = ". ",
          tickprefix = "$"
        )
      )
    
  })
  
  
  output$bar5 <- renderEcharts4r({
    sales_country <- scm %>%
      group_by(order_country) %>%
      summarise(total_sales = sum(sales)) %>%
      top_n(5) %>%
      arrange(desc(total_sales))
    
    bar5 <- sales_country %>%
      e_charts(order_country) %>%
      e_bar(total_sales, 
            name = "total_sales", 
            legend = FALSE
      ) %>%
      e_labels(position = "top"
      ) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(axisTick = FALSE) %>%
      e_tooltip() %>%
      e_color("#163ec2") %>%
      e_title(text = "Top 5 Country with Most Sales",
              left = "center")
    
    
  }) 
  
  output$pie2 <- renderEcharts4r({
    ship <- scm %>%
      filter(delivery_status == "Late delivery") %>%
      group_by(get(input$variable6)) %>%
      summarise(n = n()) %>%
      rename(
        "total_late_delivery" = n
      )
    
    pie2 <- ship %>%
      e_charts(get(input$variable6)) %>%
      e_pie(`total_late_delivery`, radius = c("50%", "70%")) %>%
      e_title(paste("Total Late Delivery by", str_to_title(str_replace(input$variable6, "_", " "),locale ='en')),
              left = "center") %>%
      e_tooltip() %>%
      e_labels(show = TRUE,
               formatter = "{b} \n {d}%",
               position = "outside",
               fontSize = 10
      ) %>%
      e_legend(orient = "vertical",
               left = "right",
               top = "50px")
    
  })
  
  output$bar6 <- renderEcharts4r({
    ship <- scm %>%
      filter(delivery_status == "Late delivery") %>%
      group_by(get(input$variable6)) %>%
      summarise(n = n()) %>%
      rename(
        "total_late_delivery" = n
      )  %>%
      arrange(total_late_delivery)
    
    bar6 <- ship %>%
      e_charts(get(input$variable6)) %>%
      e_bar(total_late_delivery, 
            name = "total_late_delivery", 
            legend = FALSE
      ) %>%
      e_labels(position = "top"
      ) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(axisTick = FALSE, fontSize = 6, axisLabel = list(interval=0)) %>%
      e_tooltip() %>%
      e_color("#163ec2") %>%
      e_title(paste("Total Late Delivery by", str_to_title(str_replace(input$variable6, "_", " "),locale ='en')),
              left = "center")
    
  }) 
    
    output$bar7 <- renderEcharts4r({
      ship <- scm %>%
        filter(delivery_status == "Late delivery") %>%
        group_by(get(input$variable7)) %>%
        summarise(n = n()) %>%
        top_n(10) %>%
        rename(
          "total_late_delivery" = n
        )  %>%
        arrange(total_late_delivery)
      
      bar7 <- ship %>%
        e_charts(get(input$variable7)) %>%
        e_bar(total_late_delivery, 
              name = "total_late_delivery", 
              legend = FALSE
        ) %>%
        e_labels(position = "right", 
                 formatter = htmlwidgets::JS("
                        function (params) {
                          return (parseFloat(params.value[0]).toLocaleString('en-US'));
                        }
                      ")
        ) %>%
        e_title(paste("Top 10", str_to_title(str_replace(input$variable7, "_", " "),locale ='en'), "with most late delivery"),
                left = "center") %>%
        e_flip_coords() %>%
        e_y_axis(axisTick = FALSE, fontSize = 4, axisLabel = list(width=150, interval=0, overflow='truncate')) %>%
        e_x_axis(show=FALSE) %>%
        e_grid(containLabel=TRUE) %>%
        e_tooltip() %>%
        e_color("#163ec2")
    
  }) 
  
  
  output$table_data <- renderDataTable({
    datatable(data = scm,
              options = list(scrollX = TRUE)
    )
  })
}
