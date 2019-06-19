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

default.color.vector <-
	c( "darkblue", "yellow", "green", "yellow", "darkred" )

clrscl <-
	function( col.vec = default.color.vector, len = length( col.vec ) ) {
		
		if( length( col.vec ) == len ) {
			list( 
				c( seq( 0, 1, length.out = len ) ), 
				c( col.vec )
			)
		}
		else {
			list(
				c( seq( 0, 1, length.out = len ) ),
				colorRampPalette( col.vec )( len )
			)
		}
	}

clrscl.monochrome <-
	function( value.between.0.and.zero, col.vec = default.color.vector ) {
		vl <- 1 + 100 * value.between.0.and.zero
		cl <- colorRampPalette( col.vec )( 101 )
		list( 
			c( 0, 1 ),
			c( cl[ vl ], cl[ vl ] )
		)
	}
#clrscl.monochrome( .4, default.color.vector)
clrspal <-
	function( length = 5, col.vec = default.color.vector ) {
		colorRampPalette( col.vec )( length )
	}

percentiles.color.palette <-
	function( perc.vec, col.vec = default.color.vector ) {
		
		cp <- colorRampPalette( col.vec )( 101 )
		
		cp[ 1 + perc.vec ]
	}
