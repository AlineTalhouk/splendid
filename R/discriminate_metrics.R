#' Polytomous discrimination index (PDI)
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @noRd
pdi <- function(C) 
{
	# pending	
}

#' Discrimination plot: boxplots of the predicted probabilities for each
#' outcome category according to each observed outcome category
#' @references http://onlinelibrary.wiley.com/doi/10.1002/sim.5321/abstract
#' @noRd
discrimination.plot <- function(x, pred.probs)
{
	require(ggplot2)

	df <- data.frame(trueClass = x, pred.probs)

	# turn into long-form for plotting
	df.long <- reshape2::melt(df, variable.name = "class", value.name = "prob", id.vars = "trueClass")
	
	# create prevailance (base-line) class proportion table
	df.prevalence <- df.long %>%
		group_by(trueClass) %>% 
		summarise(classCount = length(trueClass)) %>%
		mutate(totalCount = sum(classCount)) %>%
		mutate(prevalence = classCount / totalCount)

	# discrimination plot
	p <- ggplot(df.long, aes(x = class, y = prob, fill = class)) + geom_boxplot(alpha = 0.6) + 
			facet_wrap(~trueClass) + 
			xlab("Outcome category") + ylab("Risk of observed outcome") + theme_bw() + 
			geom_hline(data = df.prevalence, aes(yintercept = prevalence), colour = "lightgrey")
	return(p)
}
