## Create simple Fagan nomograms as ggplot objects
##   Based on Perl web-implementation (https://araw.mede.uic.edu/cgi-bin/testcalc.pl)
##   Authors: AM. Chekroud* & A. Schwartz (* adam dot chekroud at yale . edu)
##   December 2016

nomogrammer <- function(Prevalence,
                        Sens = NULL,
                        Spec = NULL,
                        LabelSize = (14/5)){
  
  ## Function inputs:
  # Prevalence (prior probability) as a number between 0 and 1
  # Either
  # Sens & Spec
  # model sensitivity and specificity as a number between 0 and 1
  # Or
  # Likelihood ratios
  # Positive and Negative LRs (numeric)
  
  ## Function options:
  # Detail: If true, will overlay key statistics onto the plot
  # NullLine: If true, will add a line from prior prob through LR = 1
  # LabelSize: Tweak this number to change the label sizes
  # Verbose: Print out relevant metrics in the console
  
  ## Function returns:
  # ggplot object
  
  
  
  ######################################
  ########## Libraries & Functions #####
  ######################################
  
  ## Libraries
  require(ggplot2)
  require(scales)
  
  
  ######################################
  ########## Calculations     ##########
  ######################################
  
  ## Checking inputs
  
  ## Prevalence
  # needs to exist
  if(missing(Prevalence)){
    stop("Prevalence is missing")
  }
  # needs to be numeric
  if(!is.numeric(Prevalence)){stop("Prevalence should be numeric")}
  # needs to be a prob not a percent
  if((Prevalence > 1) | (Prevalence <= 0)){stop("Prevalence should be a probability (did you give a %?)")}
  
  # Did user give sens & spec?
  if(missing(Sens) | missing(Spec)){
    sensspec <- FALSE
  } else{ sensspec <- TRUE}
  # if yes, make sure they are numbers
  if(sensspec == TRUE){
    if(!is.numeric(Sens)){stop("Sensitivity should be numeric")}
    if(!is.numeric(Spec)){stop("Specificity should be numeric")}
    # numbers that are probabilities not percentages
    if((Sens > 1) | (Sens <= 0)){stop("Sensitivity should be a probability (did you give a %?)")}
    if((Spec > 1) | (Spec <= 0)){stop("Specificity should be a probability (did you give a %?)")}
  }
  
  
  
  ## If sens/spec provided, we calculate posterior probabilities & odds using sens & spec
  ##  otherwise, if plr and nlr provided, we calculate posteriors using them
  ##  if neither exist, then return an error
  if(sensspec == TRUE){
    prior_prob  <- Prevalence
    prior_odds  <- odds(prior_prob)
    sensitivity <- Sens
    specificity <- Spec
    PLR <- sensitivity/(1-specificity)
    NLR <- (1-sensitivity)/specificity
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos  <- post_odds_pos/(1+post_odds_pos)
    post_prob_neg  <- post_odds_neg/(1+post_odds_neg)
  } else{
    stop("Couldn't find sens & spec, or positive & negative likelihood ratios")
  }
  
  
  
  ######################################
  ########## Plotting (prep)  ##########
  ######################################
  
  
  ## Set common theme preferences up front
  theme_set(theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_text(angle = 0),
                    axis.title.y.right = element_text(angle = 0),
                    axis.line = element_blank(),
                    panel.grid = element_blank(),
                    legend.position = "none"
              )
  )
  
  ## Setting up the points of interest along the y-axes
  
  # Select probabilities of interest (nb as percentages)
  ticks_prob    <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30,
                     40, 50, 60, 70, 80, 90, 95, 99)
  # Convert % to odds
  ticks_odds    <- odds(ticks_prob/100)
  # Convert % to logodds 
  ticks_logodds <- logodds(ticks_prob/100)
  
  # Select the likelihood ratios of interest (for the middle y-axis)
  ticks_lrs     <- sort(c(10^(-3:3), 2*(10^(-3:2)), 5*(10^(-3:2))))
  # Log10 them since plot is in logodds space
  ticks_log_lrs <- log10(ticks_lrs)
  
  ## Fixing particular x-coordinates
  left     <- 0
  right    <- 1
  middle   <- 0.5
  midright <- 0.75
  
  ## Lay out the four key plot points
  ##  (the start and finish of the positive and negative lines)
  
  # Initially these are expressed as probabilities
  df <- data.frame(x=c(left, right, left, right), 
                   y=c(prior_prob, post_prob_pos, prior_prob, post_prob_neg), 
                   line = c("pos", "pos", "neg", "neg"))
  
  adj_min      <- range(ticks_logodds)[1]
  adj_max      <- range(ticks_logodds)[2]
  adj_diff     <- adj_max - adj_min
  scale_factor <- abs(adj_min) - adj_diff/2
  #df$lo_y <- ifelse(df$x==left,(10/adj_diff)*logodds(1-df$y)-1,logodds(df$y))
  
  # Convert probabilities to logodds for plotting
  df$lo_y  <- ifelse(df$x==left,logodds(1-df$y)-scale_factor,logodds(df$y))
  # zero         <- data.frame(x = c(left,right),
  #                            y = c(0,0),
  #                            line = c('pos','pos'),
  #                            lo_y = c(-scale_factor,0))
  
  rescale   <- range(ticks_logodds) + abs(adj_min) - adj_diff/2
  rescale_x_breaks  <- ticks_logodds + abs(adj_min) - adj_diff/2  
  
  ######################################
  ########## Plot             ##########
  ######################################
  
  p <- ggplot(df) +
    geom_line(aes(x = x, y = lo_y, color = line), size = 1) +
    geom_vline(xintercept = middle) +
    annotate(geom = "text",
             x = rep(middle+.075, length(ticks_log_lrs)),
             y = (ticks_log_lrs-scale_factor)/2,
             label = ticks_lrs,
             size = rel(LabelSize)) +
    annotate(geom="point",
             x = rep(middle, length(ticks_log_lrs)),
             y = (ticks_log_lrs-scale_factor)/2,
             size = 1) +
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0),
                       limits = rescale,
                       breaks = -rescale_x_breaks,
                       labels = ticks_prob,
                       name = "prior \n prob.",
                       sec.axis = sec_axis(trans = ~.,
                                           name = "posterior \n prob.",
                                           labels = ticks_prob,
                                           breaks = ticks_logodds))
  
  ## Optional overlay text: prevalence, PLR/NLR, and posterior probabilities
  detailedAnnotation <- paste(
    paste0("prevalence = ", p2percent(prior_prob)),
    paste("PLR =", signif(PLR, 3),", NLR =", signif(NLR, 3)),
    paste("post. pos =", p2percent(post_prob_pos),
          ", neg =", p2percent(post_prob_neg)),
    sep = "\n")
  
  

  
  
  return(p)
  
}

