#' Univariate plots
#'
#' Plot univariate variable for a quick visualization.
#'
#' @param df data frame object.
#' @param var variable of interest.
#'
#' @return a ggplot object.
#'
#' @export
#'

ggunivariate = function(df, vartype) {
	var = colnames(df)
	df = (df
		|> rename(.x_var=var)
	)
	if (any(vartype %in% c("factor", "character"))) {
		df = (df
			|> group_by(.x_var)
			|> count(name = ".nxx")
			|> ungroup()
			|> mutate(prop = .nxx/sum(.nxx))
			|> arrange(desc(prop))
			|> mutate(pos = n():1, .gg=GREEN3)
		)
		p1 = (ggplot(df, aes(x = reorder(.x_var, pos), y=prop, label = scales::percent(prop), fill=.gg))
			+ scale_fill_identity()
			+ geom_col(position="dodge")
			+ scale_y_continuous(expand = c(0, 0), limits=c(0, max(df$prop, na.rm=TRUE)+0.15))
			+ scale_x_discrete(expand = c(0, 0))
			+ geom_text(position=position_dodge(width=0.9), hjust=5, size=4)
			+ base_theme()
			+ coord_flip()
			+ theme(axis.ticks.x = element_blank()
				, axis.text.x = element_blank()
				, axis.line.x = element_blank()
				, axis.title.x = element_blank()
				, axis.title.y = element_blank()
				, legend.position="none"
			)
		)
	} else {
		p1 = (ggplot(df, aes(x=.x_var))
		  + geom_histogram(aes(y = after_stat(density), fill=GREEN3))
		  + scale_fill_identity()
		  + geom_density(size = 0.8, linetype=2, colour=RED3)
		  + scale_y_continuous(expand = c(0, 0))
		  + scale_x_continuous(expand = c(0, 0))
		  + base_theme()
		  + theme(
				axis.title.x = element_blank()
				, axis.title.y = element_blank()
				, legend.position="none"
		  )
		  
		)	
	}
	p1 = (plotly::ggplotly(p1, tooltip="none") 
		|> plotly::style(hoverinfo = "all", textposition = "right")
	)
	return(p1)
}


GRAY1 <- "#231F20"
GRAY2 <- "#414040"
GRAY3 <- "#555655"
GRAY4 <- "#646369"
GRAY5 <- "#76787B"
GRAY6 <- "#828282"
GRAY7 <- "#929497"
GRAY8 <- "#A6A6A5"
GRAY9 <- "#BFBEBE"
BLUE1 <- "#174A7E"
BLUE2 <- "#4A81BF"
BLUE3 <- "#94B2D7"
BLUE4 <- "#94AFC5"
BLUE5 <- "#22435e"
BLUE6 <- "#95B3D7"
RED1 <- "#C3514E"
RED2 <- "#E6BAB7"
RED3 <- "#800000"
GREEN1 <- "#0C8040"
GREEN2 <- "#9ABB59"
GREEN3 <- "#31859C"
GREEN4 <- "#4BACC6"
GREEN5 <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"

base_theme <- function() {
  theme_minimal(base_size = 12, base_family = "Helvetica") +
    theme(
      panel.grid.major = element_blank()
		, panel.grid.minor = element_blank()
		, axis.line = element_line(size = .1, color = GRAY9)
		, axis.text = element_text(size=12, color = "black")
		, axis.ticks.x = element_line(size = 0.5, color = GRAY9)
		, axis.ticks.y = element_line(size = 0.5, color = GRAY9)
		, axis.title = element_text(color = GRAY3)
		, axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt"))
		, axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt"))
		, plot.subtitle = element_text(color = GRAY4, size= 8)
		, plot.title = element_text(color = GRAY4, size= 12)
		, plot.title.position = "plot" # This aligns the plot title to the very left edge
		, plot.caption = element_text(hjust = 0, color = GRAY6)
		, plot.caption.position = "plot"
		, plot.margin = margin(.5,.5,.5,.5,"cm")
		, strip.text = element_text(color = GRAY7)) 
}

