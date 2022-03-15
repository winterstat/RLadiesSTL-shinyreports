# Shiny App: Server side

# Define server logic
shinyServer(function(input, output) {
    
    # Get pretty names to use in plot and table (using values set up in global.R)
    pretty_names <- reactive({

        ylab <- names(char_names)[char_names == input$yaxis]
        xlab <- names(char_names)[char_names == input$xaxis]
        grouplab <- names(group_names)[group_names == input$groupvar]
        
        c(ylab, xlab, grouplab)
    })
    
    
    # Set up data frame that contains only the variables selected
    selected_data <- reactive({
        penguins %>%
            select(input$xaxis, input$yaxis, input$groupvar) %>%
            drop_na()
    })
    
    # Render scatterplot of the two selected characteristics, grouped
    # by the selected grouping variable
    output$plot <- renderPlot({
        
        # Get pretty names for labels/titles
        ylab <- pretty_names()[1]
        xlab <- pretty_names()[2]
        grouplab <- pretty_names()[3]
        
        colors <- c("darkorange","purple","cyan4")
        
        
        # Create plot
        selected_data() %>%
            ggplot(aes_string(x = input$xaxis, y = input$yaxis)) +
            # Add scatter points
            geom_point(aes_string(colour = input$groupvar,
                                  shape = input$groupvar),
                       size = 3) +
            # Include fit line
            geom_smooth(aes_string(colour = input$groupvar), 
                        method = "lm", se = FALSE) +
            scale_colour_manual(values = colors) +
            # Add pretty labels
            labs(x = xlab, y = ylab) +
            guides(colour = guide_legend(grouplab),
                   shape = guide_legend(grouplab))
        
    })
    
    # Set up nested Data Table layout
    table_layout <- reactive({
        
        # Get pretty names
        ylab <- pretty_names()[1]
        xlab <- pretty_names()[2]
        grouplab <- pretty_names()[3]
        
        # Set up Data Table layout
        sketch <- htmltools::withTags(table(
            class = 'display',
            thead(
                tr(
                    th(rowspan = 2, grouplab),
                    th(rowspan = 2, "Cases"),
                    th(colspan = 2, xlab),
                    th(colspan = 2, ylab)
                ),
                tr(
                    lapply(rep(c('Mean', 'SD'), 2), th)
                )
            )
        ))
        
        sketch
    })
    
    # Render Data Table of the two selected characteristics, grouped
    # by the selected grouping variable
    output$table <- DT::renderDataTable({
        
        # Summarize relevant values
        selected_data() %>%
            group_by(.data[[input$groupvar]]) %>%
            summarize(
                cases = n(),
                xvar_mean = mean(.data[[input$xaxis]], na.rm = T),
                xvar_sd = sd(.data[[input$xaxis]], na.rm = T),
                yvar_mean = mean(.data[[input$yaxis]], na.rm = T),
                yvar_sd = sd(.data[[input$yaxis]], na.rm = T)
            ) %>%
            # Create Data Table
            DT::datatable(options = list(dom = 't'), # only show table
                          rownames = FALSE, # don't include rownames
                          container = table_layout() # use custom layout defined above
            ) %>%
            # Round summary stats to 2 decimals
            DT::formatRound(c("xvar_mean", "xvar_sd", "yvar_mean", "yvar_sd"), 2)
    })
    
    # # Generate dynamic report
    # output$report <- downloadHandler(
    #     # Set up report name and file type using user input
    #     filename = function() {
    #         paste('my-penguin-report', sep = '.', switch(
    #             input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #         ))
    #     },
    # 
    #     # Define content of the report
    #     content = function(file) {
    #         shiny::withProgress(
    #             message = paste0("Creating your report!"),
    #             value = 0,
    #             {
    #                 #if(input$format != "Word") {
    #                     src <- normalizePath('report.Rmd')
    #                     src2 <- normalizePath('penguins.png')
    # 
    #                     shiny::incProgress(1/10)
    # 
    #                     # temporarily switch to the temp dir, in case you do not have write
    #                     # permission to the current working directory
    # 
    #                     shiny::incProgress(2/10)
    # 
    #                     owd <- setwd(tempdir())
    #                     on.exit(setwd(owd))
    #                     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #                     file.copy(src2, 'penguins.png', overwrite = TRUE)
    # 
    #                     shiny::incProgress(6/10, detail = "This may take a second...")
    # 
    #                     out <- render(input = 'report.Rmd',
    #                                   output_format = switch(input$format,
    #                                                          PDF = pdf_document(),
    #                                                          HTML = html_document(),
    #                                                          Word = word_document()
    #                                   ),
    #                                   params = list(data = selected_data(), xaxis = input$xaxis,
    #                                                 yaxis = input$yaxis, groupvar = input$groupvar,
    #                                                 prettynames = pretty_names(),
    #                                                 tablelayout = table_layout(),
    #                                                 q1 = input$q1, q2 = input$q2, q3 = input$q3,
    #                                                 key = key))
    #                     file.rename(out, file)
    #                     shiny::incProgress(8/10)
    #                 # } else {
    #                 # src <- normalizePath('report_officedown.Rmd')
    #                 # src2 <- normalizePath('penguins.png')
    #                 # src3 <- normalizePath('template.docx')
    #                 # 
    #                 # shiny::incProgress(1/10)
    #                 # 
    #                 # # temporarily switch to the temp dir, in case you do not have write
    #                 # # permission to the current working directory
    #                 # 
    #                 # shiny::incProgress(2/10)
    #                 # 
    #                 # owd <- setwd(tempdir())
    #                 # on.exit(setwd(owd))
    #                 # file.copy(src, 'report_officedown.Rmd', overwrite = TRUE)
    #                 # file.copy(src2, 'penguins.png', overwrite = TRUE)
    #                 # file.copy(src3, 'template.docx', overwrite = TRUE)
    #                 # 
    #                 # shiny::incProgress(6/10, detail = "This may take a second...")
    #                 # #library(rmarkdown)
    #                 # 
    #                 # out <- render(input = 'report_officedown.Rmd',
    #                 #               params = list(data = selected_data(), xaxis = input$xaxis,
    #                 #                             yaxis = input$yaxis, groupvar = input$groupvar,
    #                 #                             prettynames = pretty_names(),
    #                 #                             tablelayout = table_layout(),
    #                 #                             q1 = input$q1, q2 = input$q2, q3 = input$q3,
    #                 #                             key = key,
    #                 #                             flextab = table_function()))
    #                 # file.rename(out, file)
    #                 # shiny::incProgress(8/10)
    #                 # }
    # 
    # 
    #             }) # close out of progress bar
    #     }
    # ) # close out of downloadhandler
    # 
    # 
    # 
    # # Scatter plot as reactive object for officer
    # scatter_function <- reactive({
    #     # Get pretty names for labels/titles
    #     ylab <- pretty_names()[1]
    #     xlab <- pretty_names()[2]
    #     grouplab <- pretty_names()[3]
    # 
    #     colors <- c("darkorange","purple","cyan4")
    # 
    # 
    #     # Create plot
    #     selected_data() %>%
    #         ggplot(aes_string(x = input$xaxis, y = input$yaxis)) +
    #         # Add scatter points
    #         geom_point(aes_string(colour = input$groupvar,
    #                               shape = input$groupvar),
    #                    size = 3) +
    #         # Include fit line
    #         geom_smooth(aes_string(colour = input$groupvar),
    #                     method = "lm", se = FALSE) +
    #         scale_colour_manual(values = colors) +
    #         # Add pretty labels
    #         labs(x = xlab, y = ylab) +
    #         guides(colour = guide_legend(grouplab),
    #                shape = guide_legend(grouplab))
    # })
    # # 
    # # # Table as reactive object for officer
    # table_function <- reactive({
    #     # Summarize relevant values
    #     table_data <- selected_data() %>%
    #         group_by(.data[[input$groupvar]]) %>%
    #         summarize(
    #             cases = n(),
    #             xvar_mean = mean(.data[[input$xaxis]], na.rm = T),
    #             xvar_sd = sd(.data[[input$xaxis]], na.rm = T),
    #             yvar_mean = mean(.data[[input$yaxis]], na.rm = T),
    #             yvar_sd = sd(.data[[input$yaxis]], na.rm = T)
    #         )
    # 
    #     col_keys <- c(input$groupvar, colnames(table_data[2:6]))
    #     col_vals <- c(pretty_names()[3], "Cases", rep(c("Mean", "SD"), 2))
    # 
    #     ft <- flextable::flextable(table_data) %>%
    #         set_header_df(mapping = data.frame(keys = col_keys,
    #                                            values = col_vals,
    #                                            stringsAsFactors = FALSE),
    #                       key = "keys") %>%
    #         add_header_row(colwidths = c(2,2,2),
    #                        values = c(NA, pretty_names()[2], pretty_names()[1])) %>%
    #         colformat_double(digits = 2) %>%
    #         set_table_properties(layout = "autofit", width = 1)
    # 
    #     theme_vanilla(ft)
    # })
    # 
    # # If you'd like to have the flextable in your Shiny app instead, 
    # # use renderUI with htmltools_value(); and display in ui.R with uiOutput("flextab")
    # output$flextab <- renderUI({
    #     table_function() %>%
    #         htmltools_value()
    # })
    # 
    # # Generate dynamic report using officer package
    # output$officerreport <- downloadHandler(
    #     # Set up report name and file type using user input
    #     filename = function() {
    #         paste('my-penguin-report.docx'
    #         )
    #     },
    # 
    #     # Define content of the report
    #     content = function(file) {
    #         shiny::withProgress(
    #             message = paste0("Creating your report!"),
    #             value = 0, {
    # 
    #                 shiny::incProgress(1/10)
    # 
    #                 # Get plot and table objects
    #                 scatter <- scatter_function()
    #                 table <- table_function()
    # 
    #                 # Get quiz response info (this could also be a separate function)
    #                 correct1 <- if(input$q1 == key[[1]]) {"correct"} else {"incorrect"}
    #                 correct2 <- if(input$q2 == key[[2]]) {"correct"} else {"incorrect"}
    #                 correct3 <- if(input$q3 == key[[3]]) {"correct"} else {"incorrect"}
    # 
    #                 score <- sum(c(correct1, correct2, correct3) == "correct")
    # 
    #                 correct_fp <- fp_text_lite(color = "#70AD47", bold = TRUE)
    #                 incorrect_fp <- fp_text_lite(color = "#C32900", bold = TRUE)
    # 
    #                 # Format quiz result text for each question
    #                 if(correct1 == "correct") {
    #                     q1_text1 <- ftext("correct", correct_fp)
    #                     q1_par <- fpar(answer_p1, input$q1, answer_p2, q1_text1, answer_p3)
    # 
    #                 } else {
    #                     q1_text1 <- ftext("incorrect", incorrect_fp)
    #                     q1_par <- fpar(answer_p1, input$q1, answer_p2, q1_text1, answer_p3, answer_p4, key[[1]], answer_p3)
    #                 }
    # 
    #                 if(correct2 == "correct") {
    #                     q2_text1 <- ftext("correct", correct_fp)
    #                     q2_par <- fpar(answer_p1, input$q2, answer_p2, q1_text1, answer_p3)
    # 
    #                 } else {
    #                     q2_text1 <- ftext("incorrect", incorrect_fp)
    #                     q2_par <- fpar(answer_p1, input$q2, answer_p2, q1_text1, answer_p3, answer_p4, key[[2]], answer_p3)
    #                 }
    # 
    #                 if(correct3 == "correct") {
    #                     q3_text1 <- ftext("correct", correct_fp)
    #                     q3_par <- fpar(answer_p1, input$q3, answer_p2, q1_text1, answer_p3)
    # 
    #                 } else {
    #                     q3_text1 <- ftext("incorrect", incorrect_fp)
    #                     q3_par <- fpar(answer_p1, input$q3, answer_p2, q1_text1, answer_p3, answer_p4, key[[3]], answer_p3)
    #                 }
    # 
    #                 # Set up text to accompany scatter plot
    #                 scatter_text <- paste0("The scatterplot below shows the relationship between ",
    #                                        pretty_names()[2], " and ", pretty_names()[1], " for each ",
    #                                        pretty_names()[3], " observed in the 'penguins' data frame. ",
    #                                        "Linear fit lines have been included for each group, making it",
    #                                        "easier to see if the relationship is positive or negative, strong or weak.")
    # 
    #                 # Set up text to accompany table
    #                 table_text <- paste0("The table below shows the the mean and standard deviation (SD) of ",
    #                                        pretty_names()[2], " and ", pretty_names()[1], " for each ",
    #                                        pretty_names()[3], " observed in the 'penguins' data frame. ",
    #                                        "In addition, the table also includes the number of penguins within each group.")
    # 
    #                 # Set up text to accompany quiz
    #                 quiz_text <- "Below, you'll find your overall quiz score, as well as an overview of your answers (and correct answers if you answered incorrectly) to the quiz questions."
    #                 quiz_score <- paste0("Score: ", score, "/3")
    # 
    #                 shiny::incProgress(2/10)
    # 
    #                 # Build document
    #                 doc <- read_docx(path = "template.docx") %>%
    #                     body_add_par("Penguin Report", style = "Title") %>%
    #                     body_add_par(format(Sys.Date(), format="%d %B, %Y")) %>%
    #                     body_add_par("") %>%
    #                     body_add_par("Your Penguin Plot", style = "heading 1") %>%
    #                     body_add_par(scatter_text, style = "Normal") %>%
    #                     body_add_par("") %>%
    #                     body_add_gg(value = scatter, height = 4, width = 6) %>%
    #                     body_add_par("Your Penguin Table", style = "heading 1") %>%
    #                     body_add_par(table_text, style = "Normal") %>%
    #                     body_add_par("") %>%
    #                     body_add_flextable(table) %>%
    #                     body_add_par("Your Quiz Results", style = "heading 1") %>%
    #                     body_add_par(quiz_text, style = "Normal") %>%
    #                     body_add_par("") %>%
    #                     body_add_par(quiz_score, style = "heading 2") %>%
    #                     body_add_par("") %>%
    #                     body_add_par(q1_text, style = "bold") %>%
    #                     body_add_fpar(q1_par) %>%
    #                     body_add_par("") %>%
    #                     body_add_par(q2_text, style = "bold") %>%
    #                     body_add_fpar(q2_par) %>%
    #                     body_add_par("") %>%
    #                     body_add_par(q3_text, style = "bold") %>%
    #                     body_add_fpar(q3_par)
    # 
    #                 shiny::incProgress(6/10, detail = "This may take a second...")
    # 
    #                 # Return complete document and save to file
    #                 print(doc, target = file)
    #                 shiny::incProgress(8/10)
    # 
    #            }) # end progress bar
    #    } # end content function
    # ) # end download handler
})
