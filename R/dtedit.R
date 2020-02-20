#' Function to create a DataTable with Add, Edit, and Delete buttons.
#'
#' This object will maintain data state. However, in order of the data to persist
#' between Shiny instances, data needs to be saved to some external format (e.g.
#' database or R data file). The callback functions provide a mechanism for this
#' function to interact with a permanent data storage scheme. The callback
#' functions are called when the user adds, updates, or deletes a row from the
#' data table. The callback must accept two parameters: \code{data} and \code{row}.
#' For inserting and updating, the \code{data} object is the current state of
#' data table including any additions or updates. The \code{row} parameter indicates
#' which row from \code{data} was modified (or added). For deletions, however,
#' the \code{data} represents the data table just before deleting the specified
#' row. That is, if \code{callback.delete} returns a \code{data.frame}, that will
#' be the new data table; otherwise this function will remove row \code{row} from
#' \code{data} and that will become the current data table.
#'
#' The callback functions may throw errors (see e.g. \code{stop}) if there are
#' problems with data. That is, if data validation checks indicate data problems
#' before inserting or updating a row the function may throw an error. Note that
#' the error message will be presented to the user so providing messages
#' meaningful to the user is recommended. Moreover, if an error is thrown, the
#' modal dialog is not dismissed and the user can further edit the data and
#' retry the insertion or update.
#'
#' Callback functions may return a \code{data.frame}. When a \code{data.frame} is
#' returned that will become the current state of the data table. If anything
#' else is returned then the internal \code{data.frame} will be used.
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param name the name of the UI output. That is, put \code{uiOutput(name)} where
#'        you want the DataTable in \code{ui.R}. When using more that one \code{dtedit}
#'        within a Shiny application the name must be unique.
#' @param thedata a data frame to view and edit.
#' @param view.cols character vector with the column names to show in the DataTable.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.cols character vector with the column names the user can edit/add.
#'        This can be a subset of the full \code{data.frame}.
#' @param edit.label.cols character vector with the labels to use on the edit
#'        and add dialogs. The length and order of \code{code.cols.labels} must
#'        correspond to \code{edit.cols}.
#' @param input.types a character vector where the name corresponds to a column
#'        in \code{edit.cols} and the value is the input type. Possible values
#'        are \code{dateInput}, \code{selectInput}, \code{numericInput},
#'        \code{textInput}, \code{textAreaInput}, or \code{passwordInput}.
#'        The most common case where this parameter is desirable is when a text
#'        area is required instead of a simple text input.
#' @param input.choices a list of character vectors. The names of each element in the list must
#'        correpsond to a column name in the data. The value, a character vector, are the options
#'        presented to the user for data entry.
#' @param selectize Whether to use selectize.js or not. See \code{\link{selectInput}} for more info.
#' @param defaultPageLength number of rows to show in the data table by default.
#' @param modal.size the size of the modal dialog. See \code{\link{modalDialog}}.
#' @param text.width width of text inputs.
#' @param textarea.width the width of text area inputs.
#' @param textarea.height the height of text area inputs.
#' @param date.width the width of data inputs
#' @param numeric.width the width of numeric inputs.
#' @param select.width the width of drop down inputs.
#' @param title.delete the title of the dialog box for deleting a row.
#' @param title.edit the title of the dialog box for editing a row.
#' @param title.add the title of the dialog box for inserting a new row.
#' @param label.delete the label of the delete button.
#' @param label.edit the label of the edit button.
#' @param label.add the label of the add button.
#' @param label.copy the label of the copy button.
#' @param show.delete whether to show/enable the delete button.
#' @param show.update whether to show/enable the update button.
#' @param show.insert whether to show/enable the insert button.
#' @param show.copy whether to show/enablre the copy button.
#' @param callback.delete a function called when the user deletes a row. This function should
#'        return an updated data.frame.
#' @param callback.update a function called when the user updates a row. This function should
#'        return an updated data.frame.
#' @param callback.insert a function called when the user inserts a new row. This function should
#'        return an updated data.frame.
#' @param click.time.threshold This is to prevent duplicate entries usually by double clicking the
#'        save or update buttons. If the user clicks the save button again within this amount of
#'        time (in seconds), the subsequent click will be ignored. Set to zero to disable this
#'        feature. For developers, a message is printed using the warning function.
#' @param datatable.options options passed to \code{\link{DT::renderDataTable}}.
#'        See \link{https://rstudio.github.io/DT/options.html} for more information.
#' @export
dtedit <- function(input, output, name, thedata, user_name, ship_2, data_type,
				   view.cols = names(thedata),
				   edit.cols = names(thedata),
				   edit.label.cols = edit.cols,
				   input.types,
				   ship_to,
				   show = T,
				   input.choices = NULL,
				   selectize = TRUE,
				   modal.size = 'm',
				   text.width = '100%',
				   textarea.width = '570px',
				   textarea.height = '200px',
				   date.width = '100px',
				   numeric.width = '120px',
				   select.width = '100%',
				   defaultPageLength = 10,
				   title.delete = 'Delete',
				   title.edit = 'Quote Lane',
				   title.add = 'New',
				   label.delete = 'Delete Quote',
				   label.edit = 'Quote Lane',
				   label.add = 'Edit Quote',
				   label.copy = 'Copy',
				   show.delete = F,
				   show.update = TRUE,
				   show.insert = F,
				   callback.delete = function(data, row) { },
				   callback.update = function(data, olddata, row) { },
				   callback.insert = function(data, row) { },
				   click.time.threshold = 2, # in seconds
				   datatable.options = list(pageLength=defaultPageLength)
)


{

	# Some basic parameter checking
	if(!is.data.frame(thedata) | ncol(thedata) < 1) {
		stop('Must provide a data frame with at least one column.')
	} else if(length(edit.cols) != length(edit.label.cols)) {
		stop('edit.cols and edit.label.cols must be the same length.')
	} else if(!all(view.cols %in% names(thedata))) {
		stop('Not all view.cols are in the data.')
	} else if(!all(edit.cols %in% names(thedata))) {
		stop('Not all edit.cols are in the data.')
	}

	DataTableName <- paste0(name, 'dt')

	dt.proxy <- DT::dataTableProxy(DataTableName)

	Data_2 <- paste0(name, 'dt2')

	dt.proxy2 <- DT::dataTableProxy(Data_2)


	selectInputMultiple <- function(...) {
		shiny::selectInput(multiple = TRUE, selectize = selectize, ...)
	}

	result <- shiny::reactiveValues()
	result$thedata <- thedata
	result$view.cols <- view.cols
	result$edit.cols <- edit.cols


	valid.input.types <- c('dateInput', 'selectInput', 'numericInput',
						   'textInput', 'textAreaInput', 'passwordInput', 'selectInputMultiple')
	inputTypes <- sapply(thedata[,edit.cols], FUN=function(x) {
		switch(class(x),
			   list = 'selectInputMultiple',
			   character = 'textInput',
			   Date = 'dateInput',
			   factor = 'selectInput',
			   integer = 'numericInput',
			   numeric = 'numericInput')
	})
	if(!missing(input.types)) {
		if(!all(names(input.types) %in% edit.cols)) {
			stop('input.types column not a valid editting column: ',
				 paste0(names(input.types)[!names(input.types) %in% edit.cols]))
		}
		if(!all(input.types %in% valid.input.types)) {
			stop(paste0('input.types must only contain values of: ',
						paste0(valid.input.types, collapse = ', ')))
		}
		inputTypes[names(input.types)] <- input.types
	}

	# Convert any list columns to characters before displaying
	for(i in 1:ncol(thedata)) {
		if(nrow(thedata) == 0) {
			thedata[,i] <- character()
		} else if(is.list(thedata[,i])) {
			thedata[,i] <- sapply(thedata[,i], FUN = function(x) { paste0(x, collapse = ', ') })
		}
	}


	output[[DataTableName]] <- DT::renderDataTable({
	  submission <- dbGetQuery(cn, "SELECT * FROM submission;")
	  submission <- submission %>%
	  filter(user %in% user_name)

	  thedata <-  thedata %>%
	  filter(`Lane Code` %in% setdiff(`Lane Code`, submission$`Lane Code`))

	  result$thedata <- thedata

	  thedata[,view.cols]
	}, options = datatable.options, server=TRUE, selection='single', rownames=FALSE)


	output[[Data_2]] <- DT::renderDataTable({
	  submission <- dbGetQuery(cn, "SELECT * FROM submission;")
	  submission <- submission %>%
	    filter(user %in% user_name,
	           grepl(data_type, `Lane Code`))

	  table2 <-  thedata %>%
	    filter(`Lane Code` %in% intersect(`Lane Code`, submission$`Lane Code`))

	  table2_join <- left_join(submission, table2, by = c('Lane Code' = 'Lane Code')) %>%
	    select('Lane Code', 'Ship From', ship_2, "Total Shipments/Year","Avg.(Kg)/Shipment", "quote", "est_transit") %>%
	    filter(!is.na(`Ship From`))

	  names(table2_join)[names(table2_join) == "est_transit"] <- "Est. Transit(days)"
	  names(table2_join)[names(table2_join) == "quote"] <- "Quote Amount"
	  names(table2_join)[names(table2_join) == "Lane Code"] <- "Lane Code"
	  table2_join[,6] <- sapply(table2_join[,6], function(x) paste0("  $",round(x,2)))

	  table2_join
	}, options = list(
	  columnDefs = list(list(className = 'dt-right', targets = 5))), server=TRUE, selection='single', rownames=FALSE)


	output[[paste0(name, '_message')]] <- shiny::renderText('')

	updateData <- function(proxy, data, ...) {
		# Convert any list columns to characters before displaying
		for(i in 1:ncol(data)) {
			if(is.list(data[,i])) {
				data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
			}
		}
		DT::replaceData(proxy, data, ...)
	}

	##### Insert functions #####################################################

	observeEvent(input[[paste0(name, '_add')]], {
		if(!is.null(therow)) {
			shiny::showModal(addModal())
		}
	})

	insert.click <- NA

	observeEvent(input[[paste0(name, '_insert')]], {
		if(!is.na(insert.click)) {
			lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
			if(lastclick < click.time.threshold) {
				warning(paste0('Double click detected. Ignoring insert call for ', name, '.'))
				return()
			}
		}
		insert.click <<- Sys.time()

		newdata <- result$thedata
		row <- nrow(newdata) + 1
		newdata[row,] <- NA
		for(i in edit.cols) {
			if(inputTypes[i] %in% c('selectInputMultiple')) {
				newdata[[i]][row] <- list(input[[paste0(name, '_add_', i)]])
			} else {
				newdata[row,i] <- input[[paste0(name, '_add_', i)]]
			}
		}
		tryCatch({
			callback.data <- callback.insert(data = newdata, row = row)
			if(!is.null(callback.data) & is.data.frame(callback.data)) {
				result$thedata <- callback.data
			} else {
				result$thedata <- newdata
			}
			updateData(dt.proxy,
						result$thedata[,view.cols],
						rownames = FALSE)
			shiny::removeModal()
			return(TRUE)
		}, error = function(e) {
		 	output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
		 	return(FALSE)
		})
	})

	addModal <- function(row, values) {
		output[[paste0(name, '_message')]] <- shiny::renderText('')
		fields <- getFields('_add_', values)
		shiny::modalDialog(title = title.add,
					shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
					fields,
					footer = shiny::column(shiny::modalButton('Cancel'),
									shiny::actionButton(paste0(name, '_insert'), 'Save'),
									width=12),
					size = modal.size
		)
	}


	##### Update functions #####################################################

	observeEvent(input[[paste0(name, '_edit')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(editModal(row))
			}
		}
	})

	update.click <- NA

	observeEvent(input[[paste0(name, '_update')]], {
		if(!is.na(update.click)) {
			lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
			if(lastclick < click.time.threshold) {
				warning(paste0('Double click detected. Ignoring update call for ', name, '.'))
				return()
			}
		}
		update.click <- Sys.time()

		row <- input[[paste0(name, 'dt_rows_selected')]]

		if(!is.null(row)) {
			if(row > 0) {
				newdata <- data.frame(`Lane Code` = result$thedata[row,]$`Lane Code`,
				                      peak_season = input$peak_season,
				                      PSquote = input$quote1, quote = input$quote2,
				                      PStransit = input$est_transit1,
				                      transit = input$est_transit2, notes = input$notes,
				                      user = user_name, date_time = Sys.time())
				dplyr::db_insert_into(cn, table = "submission", values = newdata)
				submission <- DBI::dbGetQuery(cn, "SELECT * FROM submission;")
				submission_filter <- submission %>%
				  filter(user %in% user_name)

				newdata1 <- thedata %>%
				  filter(`Lane Code` %in% setdiff(`Lane Code`, submission_filter$`Lane Code`))

				submission <- dbGetQuery(cn, "SELECT * FROM submission;")
				submission <- submission %>%
				  filter(user %in% user_name,
				         grepl(data_type, `Lane Code`))

				table2 <-  thedata %>%
				  filter(`Lane Code` %in% intersect(`Lane Code`, submission$`Lane Code`))

				table2_join <- left_join(submission, table2, by = c('Lane Code' = 'Lane Code')) %>%
				  select('Lane Code', 'Ship From', ship_2, "Total Shipments/Year","Avg.(Kg)/Shipment", "quote", "est_transit") %>%
				  filter(!is.na(`Ship From`))

				names(table2_join)[names(table2_join) == "est_transit"] <- "Est. Transit(days)"
				names(table2_join)[names(table2_join) == "quote"] <- "Quote Amount"
				names(table2_join)[names(table2_join) == "Lane Code"] <- "Lane Code"
				table2_join[,6] <- sapply(table2_join[,6], function(x) paste0("  $",round(x,2)))

				table2_join

				}
				tryCatch({
					callback.data <- callback.update(data = newdata1, row = row)
					if(!is.null(callback.data) & is.data.frame(callback.data)) {
						result$thedata <- callback.data
					} else {
						result$thedata <- newdata1
					}
					updateData(dt.proxy2,
					           table2_join,
					           rownames = FALSE)
					updateData(dt.proxy,
								result$thedata[,view.cols],
								rownames = FALSE)
					shiny::removeModal()
					return(TRUE)
				}, error = function(e) {
					output[[paste0(name, '_message')]] <<- shiny::renderText(geterrmessage())
					return(FALSE)
				})
		}
		return(FALSE)
	})

	editModal <- function(row) {
		output[[paste0(name, '_message')]] <- renderText('')
		#fields <- getFields('_edit_', values=result$thedata[row,])


		###### Month Display Table ######
		display_table <- result$thedata[row,]
		display_table <- display_table %>%
		  select(jan_count, jan_avg_kg,feb_count, feb_avg_kg,mar_count, mar_avg_kg,apr_count, apr_avg_kg,may_count, may_avg_kg,jun_count, jun_avg_kg,jul_count, jul_avg_kg,aug_count, aug_avg_kg,sep_count, sep_avg_kg,oct_count, oct_avg_kg,nov_count, nov_avg_kg,dec_count, dec_avg_kg) %>%
		  gather() %>%
		  mutate(Month = case_when(grepl("jan", key) ~ "January",grepl("feb", key) ~ "February",grepl("mar", key) ~ "March",grepl("apr", key) ~ "April",grepl("may", key) ~ "May",grepl("jun", key) ~ "June",grepl("jul", key) ~ "July",grepl("aug", key) ~ "August",grepl("sep", key) ~ "September",grepl("oct", key) ~ "October",grepl("nov", key) ~ "November",grepl("dec", key) ~ "December")) %>%
		  mutate(Month = fct_relevel(Month, c("January",
		                                      "February","March","April",
		                                      "May","June","July","August","September",
		                                      "October","November","December"))) %>%
		  filter(!is.na(value)) %>%
		  group_by(key, Month) %>%
		  summarize(value = value)

		  display_table <- spread(display_table, key, value)

		  display_table1 <- display_table %>%
		  replace(is.na(.), 0) %>%
		  mutate("Total Shipments" = round(as.numeric(rowSums(.[grep("count", names(.))])), digits = 0),
		         "Avg.(Kg)/Shipment" = round(rowSums(.[grep("avg", names(.))])/rowSums(.[grep("count", names(.))])),2) %>%
		  select(Month, "Total Shipments", "Avg.(Kg)/Shipment") %>%
		  filter(!is.na("Avg.(Kg)/Shipment"), "Total Shipments" != 0)


		  ###### Average Weight per shipment/Year ######

year_avg <-  result$thedata[row,] %>%
		    mutate(year_avg = round(rowSums(.[grep("total", names(.))])/rowSums(.[grep("count", names(.))]),2),
		           year_count = as.numeric(rowSums(.[grep("count", names(.))])))

      ####### Back to the Modal Dialog

		shiny::modalDialog(title = title.edit,
		                   HTML("<font size = '+1.4'><p style='text-align:left;'>"),
		                   strong("Lane: "), result$thedata[row,]$'Lane',
		                   HTML("<span style='float:right;'>"), strong("Mode: "), result$thedata[row,]$Mode,
		                   HTML("</span></p>"),
		                   strong("Ship From: "), result$thedata[row,]$`Ship From`,
		                   HTML("<br>"),
		                   strong(ship_to), result$thedata[row,]$`Ship To`,
		                   HTML("</font><br><br>"),
			shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
			shiny::div(tags$head(tags$style(type="text/css", "#inline label{ display: table-cell;
			                           text-align: center; vertical-align: middle; }
                                  #inline .form-group { display: table-row;}"))),
			  shinyBS::bsCollapsePanel("Historical Shipment Information by Month", HTML("<center>"),renderTable(display_table1, digits = 0, na = 0)), HTML("</center>"),
                     HTML("<u><font size='+1.8'>"),strong("Please Quote from 2019 Figures below:"),HTML("</u></font><br>"),
                     HTML("<font size='+.8'>"), HTML("<br>"),
                     strong("Number of shipments: "), HTML("<u>"), strong(year_avg$year_count), HTML("</u>"),
                     HTML("<br>"),
                     strong("Average Weight Per shipment: "), HTML("<u>"), strong(round(year_avg$year_avg,2)),
                     strong("Kg's"), HTML("</u>"), HTML("</font>"),
                     HTML("<hr>"),
			               tags$div(id = "inline", checkboxGroupInput(inputId = 'peak_season',
			                                     label = "Define Peak Season(s): ", choices = c("1st Quarter","2nd Quarter","3rd Quarter","4th Quarter")), multiple = T, width = '800px'),
			              HTML("<u>"), strong("Peak Season Quote:"), HTML("</u>"),
			              tags$div(id = "inline", numericInput(inputId = 'quote1',
			                                                             label = "*Cost per Shipment: $  ", value = 0.00), step=".01"),
			               tags$div(id = "inline", numericInput(inputId = 'est_transit1',
			                                                             label = "Approx. Transit Time(Days): ", value = 0)),
			              HTML("<br><u>"),strong("Non-Peak Season Quote:"),HTML("</u>"),
              		  tags$div(id = "inline", numericInput(inputId = 'quote2',
              			                                     label = "*Cost per Shipment: $  ", value = 0.00), step=".01"),
              			tags$div(id = "inline", numericInput(inputId = 'est_transit2',
              			                                     label = "Approx. Transit Time(Days): ", value = 0)), HTML("<br>"),
			              textAreaInput("notes", label = "Additional Notes:", width = textarea.width, height = textarea.height),
			              "*Please include all Assessorials in total quoted cost."
			   ,
			footer = column(shiny::modalButton('Cancel'),
							shiny::actionButton(paste0(name, '_update'), 'Submit'),
							width=12),
			size = modal.size
		)
	}

	##### Delete functions #####################################################

	observeEvent(input[[paste0(name, '_remove')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				shiny::showModal(deleteModal(row))
			}
		}
	})

	observeEvent(input[[paste0(name, '_delete')]], {
		row <- input[[paste0(name, 'dt_rows_selected')]]
		if(!is.null(row)) {
			if(row > 0) {
				newdata <- callback.delete(data = result$thedata, row = row)
				if(!is.null(newdata) & is.data.frame(newdata)) {
					result$thedata <- newdata
				} else {
					result$thedata <- result$thedata[-row,]
				}
				updateData(dt.proxy,
							result$thedata[,view.cols],
							rownames = FALSE)
				shiny::removeModal()
				return(TRUE)
			}
		}
		return(FALSE)
	})

	deleteModal <- function(row) {
		fields <- list()
		for(i in view.cols) {
			fields[[i]] <- div(paste0(i, ' = ', result$thedata[row,i]))
		}
		shiny::modalDialog(title = title.delete,
					shiny::p('Are you sure you want to delete this record?'),
					fields,
					footer = shiny::column(modalButton('Cancel'),
									shiny::actionButton(paste0(name, '_delete'), 'Delete'),
									width=12),
					size = modal.size
		)
	}

	##### Build the UI for the DataTable and buttons ###########################

	output[[name]] <- shiny::renderUI({
		shiny::div(
		  HTML("<u>"), h4("Lanes to Quote"), HTML("</u>"),
			if(show.update) { shiny::actionButton(paste0(name, '_edit'), label.edit) },
			if(show.insert) { shiny::actionButton(paste0(name, '_add'), label.add) },
			if(show.delete) { shiny::actionButton(paste0(name, '_remove'), label.delete) },
			shiny::br(), shiny::br(), DT::dataTableOutput(DataTableName), HTML("<hr>"), hr(),
			if (show == T) {
			  div(HTML("<u>"),
			  h4("Submitted Quotes"),
			  HTML("</u>"),
			  h5("**See 'Submitted Quotes' Tab above to edit or delete submitted quotes"),
  			DT::dataTableOutput(Data_2))
			  } else {
			    NULL
			    },
			shiny::br(), shiny::br(),
			shiny::br(), shiny::br()
		)
	})


	return(result)
}
