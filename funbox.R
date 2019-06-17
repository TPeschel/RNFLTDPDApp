output.table.percentiles <-
	function( table.name, data, output ) {
		
		output[[ table.name ]] <-
			rhandsontable::renderRHandsontable( {
				rhandsontable(
					data,
					useTypes = T,
					rowHeaders = F,
					stretchH = "all" ) %>%
					hot_validate_numeric( cols = "CNT", min = .001, max = 99.999, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "CNT", allowInvalid = F ) %>%
					hot_validate_numeric( cols = "SDS", min = -4.5, max = +4.5, allowInvalid = F ) %>%
					hot_col( format = "+0.000", col = "SDS", allowInvalid = F ) %>%
					hot_cols(
						renderer = "
						function ( instance, td, row, col, prop, value, cellProperties ) {
						Handsontable.renderers.NumericRenderer.apply( this, arguments );
						if ( row == 0 || row == instance.countRows( ) - 1 ) {
						td.style.background = '#f0f0e0';
						td.style.color = 'black';
						} else {
						td.style.background = 'lighgray';
						td.style.color = 'black';
						}
						}"
					)
			} )
	}

output.table.visitor <-
	function( table.name, data, output ) {
		
		output[[ table.name ]] <-
			rhandsontable::renderRHandsontable( {
				print( data )
				rhandsontable(
					data,
					useTypes = T,
					rowHeaders = F,
					stretchH = "all" ) %>%
					hot_validate_numeric( cols = "RADIUS.DIFFERENCE", min = -100, max = 100, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "RADIUS.DIFFERENCE", allowInvalid = F ) %>%
					hot_validate_numeric( cols = "ANGLE", min = 0, max = 360, allowInvalid = F ) %>%
					hot_col( format = "0.000", col = "ANGLE", allowInvalid = F ) #%>%
					# hot_cols(
					# 	renderer = "
					# 	function ( instance, td, row, col, prop, value, cellProperties ) {
					# 	Handsontable.renderers.NumericRenderer.apply( this, arguments );
					# 	if ( row == 0 || row == instance.countRows( ) - 1 ) {
					# 	td.style.background = '#f0f0e0';
					# 	td.style.color = 'black';
					# 	} else {
					# 	td.style.background = 'lighgray';
					# 	td.style.color = 'black';
					# 	}
					# 	}"
					# )
			} )
	}


tableOptions <-
	list(
		paging = TRUE,
		pageLength = 10,
		searching = TRUE,
		fixedColumns = TRUE,
		autoWidth = FALSE,
		ordering = TRUE,
		dom = 'Bpfrtip', 
		buttons = c('copy', 'excel', 'csv', 'print', 'colvis' )
	)
