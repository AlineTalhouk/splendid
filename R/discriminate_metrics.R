# dchiu911: please update metrics below into evaluations.R as necessary.
# 			discrimination plot may not be necessary. Inputs required
#			include observed labels (x)  and predicted probabilities.

#' Multi-class Log/cross-entropy Loss 
#' @references https://cran.r-project.org/web/packages/MLmetrics/MLmetrics.pdf 
#' @noRd
logloss <- function(x, pred.probs)
{
    if (is.matrix(x) == FALSE) {
        x <- model.matrix(~0 + ., data.frame(as.character(x)))
    }
    eps <- 1e-15
    N <- nrow(pred.probs)
    pred.probs <- pmax(pmin(pred.probs, 1 - eps), eps)
    MultiLogLoss <- (-1/N) * sum(x * log(pred.probs))
    return(MultiLogLoss)
}

#' AUC/M-index: Multiple Class Area under ROC Curve
#' @references http://link.springer.com/article/10.1023/A:1010920819831
#' @noRd
auc <- function(x, pred.probs)
{
	# ui-constructor for multicap class
	mcap.construct <- HandTill2001::multcap(response = x, predicted = as.matrix(pred.probs))	
	
	# multi-class auc metric	
	auc.out <- HandTill2001::auc(mcap.construct)

	return(auc.out)
}

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
