heatscatter_liuqin <- function(
  df,                   # input dataframe.
  mod,                  # mod-variable (y-axis)
  obs,                  # obs-variable (x-axis)
  moist         = TRUE, # limit data to "moist" days (TRUE), "dry" days (FALSE)
                        # or include "all" days (NULL)
  swap.vars     = FALSE,# swap the obs and mod variables
  text.size     = 12,   # size (pts) of text elements on graph
  point.size    = 1.5,  # the size of points on the graph
  axis.font.size= 1,    # scaling factor for the font size for the axes
  main.title    = NULL, # the main title centered above graph
  mod.title     = NULL, # title for the mod variable (y-axis)
  obs.title     = NULL, # title for the obs variable (x-axis)
  mod.lim       = NULL, # mod-axis range (y-axis)
  obs.lim       = NULL, # obs-axis range (x-axis)
  plot.metrics  = TRUE, # shoud the metrics be written on the graph?
  metric.pos    = "bottomright", # or "topleft" - position of metric labels
  metric.size   = 1,    # scaling factor for the font size of the metrics
  plot.linmod   = TRUE, # plot regression line
  border        = TRUE, # add a border around the graph
  n.tickmarks   = 6,    # approximate number of tickmarks on axes.
  forceorigin   = TRUE, # force the (0,0) as origin of graph
  tag           = NULL, # tag in top-left corner above the image, e.g. (a), etc.
  timesnewroman = TRUE, # Use Times New Roman font. If false, use the
                        # Liberation Serif font (as replacement font for Linux).
                        # If NULL, use the default font.
  dpi           = 600,  # resolution of the jpeg image
  filnam        = paste(mod,"_vs_",obs,sep=""),
  ...
  ) {
  # Read in required packages.
  require(ggplot2)
  require(dplyr)
  require(LSD)
  require(ggthemes)
  require(RColorBrewer)
  # LSD.heatscatter.R also needs to be sourced!
  ###############
  # Prepare data.
  ###############
  # Moist days are filtered out if moist=TRUE (default),
  # dry days are filtered out if moist=FALSE, and
  # all days are included if moist=NULL.
  if (!is.null(moist)) {
    if (moist) {
      df <- df %>% dplyr::filter(moist==TRUE)
    } else {
      df <- df %>% dplyr::filter(moist==FALSE)
    }
  }

  # NB This is dependent on moist input variable (are data restricted to moist days or not?)
  # calculate bias for nn_act and nn_pot,
  # cut soilm into 10 euqivally sized intervals and label values accordingly (1,2,...,10),
  # calculate the ratio of nn_act / obs, and
  # calculate the ratio of nn_pot / obs.
  df_stats <- df %>%
    mutate(bias_act = nn_act - obs,
           bias_pot = nn_pot - obs,
           soilm_bin = cut(soilm, 10),
           ratio_act = nn_act / obs,
           ratio_pot = nn_pot / obs
  )

  # Extract selected columns (given by mod and obs parameters) from df (discarding all the rest).
  if (!swap.vars) {
    df <- df %>%
      as_tibble() %>%
      ungroup() %>%
      dplyr::select(mod=all_of(mod), obs=all_of(obs)) %>%
      tidyr::drop_na(mod, obs)
  } else {
    df <- df %>%
      as_tibble() %>%
      ungroup() %>%
      dplyr::select(mod=all_of(obs), obs=all_of(mod)) %>%
      tidyr::drop_na(mod, obs)
  }

  ####################
  # Calculate metrics. 
  ####################
  # Get linear regression (coefficients).
  linmod <- lm( mod ~ obs, data=df )

  # Construct metrics table using the 'yardstick' library.
  df_metrics <- df %>%
    yardstick::metrics(mod, obs) %>%
    dplyr::bind_rows(tibble(
      .metric = "n",
      .estimator = "standard",
      .estimate = summarise(df, numb=n()) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "slope",
      .estimator = "standard",
      .estimate = coef(linmod)[2]
    )) %>%
    #dplyr::bind_rows(tibble(
    #  .metric = "nse",
    #  .estimator = "standard",
    #  .estimate = hydroGOF::NSE(obs, mod, na.rm=TRUE)
    #)) %>%
    dplyr::bind_rows(tibble(
      .metric = "mean_obs",
      .estimator = "standard",
      .estimate = summarise(df, mean=mean(obs, na.rm=TRUE)) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "prmse",
      .estimator = "standard",
      .estimate =
        dplyr::filter(., .metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "pmae",
      .estimator = "standard",
      .estimate =
        dplyr::filter(., .metric=="mae") %>% dplyr::select(.estimate) %>% unlist() /
        dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "bias",
      .estimator = "standard",
      .estimate = dplyr::summarise(df, mean((mod-obs), na.rm=TRUE    )) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "pbias",
      .estimator = "standard",
      .estimate = dplyr::summarise(df, mean((mod-obs)/obs, na.rm=TRUE)) %>% unlist()
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "cor",
      .estimator = "standard",
      .estimate = cor(df$mod, df$obs, method = "pearson")
    )) %>%
    dplyr::bind_rows(tibble(
      .metric = "cor_p",
      .estimator = "standard",
      .estimate = cor.test(df$mod, df$obs, method = "pearson")$p.value
    ))

  # Extract metrics estimates into variables.
  rsq_val <- df_metrics %>% dplyr::filter(.metric=="rsq") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  rmse_val <- df_metrics %>% dplyr::filter(.metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  mae_val <- df_metrics %>% dplyr::filter(.metric=="mae") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  bias_val <- df_metrics %>% dplyr::filter(.metric=="bias") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  slope_val <- df_metrics %>% dplyr::filter(.metric=="slope") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  n_val <- df_metrics %>% dplyr::filter(.metric=="n") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  cor_val <- df_metrics %>% dplyr::filter(.metric=="cor") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  cor_p_val <- df_metrics %>% dplyr::filter(.metric=="cor_p") %>% dplyr::select(.estimate) %>% unlist() %>% unname()

  # Arrange metric values in a vector for returning.
  results <- tibble( rsq = rsq_val, rmse = rmse_val, mae = mae_val, bias = bias_val, slope = slope_val, n = n_val )

  ##################################
  # Construct the heatscatter graph.
  ##################################

  # Pretty number formats of metrics values for labels in graphs.
  rsq_lab <- format( rsq_val, digits = 2 )
  rmse_lab <- format( rmse_val, digits = 3 )
  mae_lab <- format( mae_val, digits = 3 )
  bias_lab <- format( bias_val, digits = 3 )
  slope_lab <- format( slope_val, digits = 3 )
  n_lab <- format( n_val, digits = 3 )
  cor_lab <- format( cor_val, digits = 3 )
  cor_p_lab <- format( cor_p_val, digits = 3 )

  # x- and y-axis range
  if (!swap.vars) {
      xlim <- obs.lim
      ylim <- mod.lim
  } else {
      xlim <- mod.lim
      ylim <- obs.lim
  }
  gg <- heatscatter(
    x=df$obs,
    y=df$mod,
    xlim=xlim,
    ylim=ylim,
    cexplot=point.size,
    main="",
    ggplot=TRUE)

  if (!is.null(timesnewroman)) {
    if (timesnewroman) {
      gg <- gg + theme_classic(base_size=text.size,base_family = "Times New Roman")
    } else {
      gg <- gg + theme_classic(base_size=text.size,base_family = "LiberationSerif")
    }
  } else {
    gg <- gg + theme_classic(base_size=text.size)
  }

  gg <- gg +
    geom_abline(intercept=0, slope=1, linetype="dotted") +
    xlab(ifelse(!swap.vars,obs.title,mod.title)) +
    ylab(ifelse(!swap.vars,mod.title,obs.title)) +
    scale_x_continuous(limits=xlim,n.breaks=n.tickmarks) +
    scale_y_continuous( limits=ylim,n.breaks=n.tickmarks) +
    theme(
      aspect.ratio=1,
      plot.margin = unit(c(5,5,5,5),"mm"),
      axis.title.x = element_text(margin=margin(3,0,0,0,unit="mm")),
      axis.title.y = element_text(margin=margin(0,3,0,0,unit="mm")),
      axis.text.x = element_text(size = rel(axis.font.size)),
      axis.text.y = element_text(size = rel(axis.font.size)),
      plot.title = element_text(hjust = 0.5)
    )
  if (border) {
    gg <- gg + theme(
      axis.line = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        size = 0.5
      )
    )
  }
  if (forceorigin) {
    gg <- gg + expand_limits(x = 0, y = 0)
  }
  if (plot.linmod) {
    gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)
  }
  if (!is.null(main.title)) {
    gg <- gg + labs(title=main.title)
  }
  if (!is.null(tag)) {
    gg <- gg + labs(tag=tag)
  }
  # separate theme for the returned plot (gg), pdf plot (gg.pdf) and jpeg plot (gg.jpeg).
  gg.pdf <- gg
  gg.jpeg <- gg +
    theme(
      text = element_text(size=text.size*(dpi/100)), # *3: 300dpi, *2: 200dpi
      # since ticks scale with the text size... correct them
      axis.ticks = element_line(size=0.5),
      axis.ticks.length=unit(1,"mm")
    )
  # metric display processing
  if (plot.metrics) {
    xr <- range(layer_scales(gg)$x$range$range,xlim)
    dxr=diff(xr)
    yr <- range(layer_scales(gg)$y$range$range,ylim)
    dyr=diff(yr)
    xm <- ifelse(metric.pos=="topleft",0,max(0,(0.75-(metric.size-1)/5)))
    ym <- ifelse(metric.pos=="topleft",1,0.2*(1+(metric.size-1)/2))
    metric_labels <- data.frame(
      x = rep(xr[1]+dxr*xm,5),
      y = seq(from=yr[1]+dyr*ym,by=-dyr*0.05*(1+(metric.size-1)/2),length.out=5),
      labels = c(
        paste("italic(N) == ",n_lab,sep=""),
        paste("slope == ",slope_lab,sep=""),
        paste("italic(R)^2 == ",rsq_lab,sep=""),
        paste("RMSE == ",rmse_lab,sep=""),
        paste("bias == ",bias_lab,sep="")
      )
    )
    gg <- gg + geom_label(
        data=metric_labels,
        aes(x=x,y=y,label=labels),
        inherit.aes=FALSE,
        parse=TRUE,
        show.legend=FALSE,
        label.size=0,
        vjust="center",
        hjust="left",
        family=ifelse(!is.null(timesnewroman),
               (ifelse(timesnewroman,"Times New Roman","LiberationSerif")),
               ""),
        size=text.size/.pt*metric.size
      )
    gg.pdf <- gg.pdf + geom_label(
        data=metric_labels,
        aes(x=x,y=y,label=labels),
        inherit.aes=FALSE,
        parse=TRUE,
        show.legend=FALSE,
        label.size=0,
        vjust="center",
        hjust="left",
        family=ifelse(!is.null(timesnewroman),
               (ifelse(timesnewroman,"Times New Roman","LiberationSerif")),
               ""),
        size=text.size/.pt*2/3*metric.size # 2/3 is a scaling factor thar works for pdf
      )
    gg.jpeg <- gg.jpeg + geom_label(
        data=metric_labels,
        aes(x=x,y=y,label=labels),
        inherit.aes=FALSE,
        parse=TRUE,
        show.legend=FALSE,
        label.size=0,
        vjust="center",
        hjust="left",
        family=ifelse(!is.null(timesnewroman),
               (ifelse(timesnewroman,"Times New Roman","LiberationSerif")),
               ""),
        size=text.size/.pt*2/3*metric.size*(dpi/100) # 2/3*(dpi/100) is a scaling factor that works for jpeg
      )
  }

  # print to file
  if (!is.na(filnam)) {
    if(swap.vars) {
      filnam <- paste(filnam,"_vars_swapped",sep="")
    }
    ggsave(filename=paste(filnam,".pdf",sep=""),plot=gg.pdf, device="pdf", width=5,height=5,units="in")
    ggsave(filename=paste(filnam,".jpg",sep=""),plot=gg.jpeg,device="jpeg",width=5,height=5,units="in",dpi=dpi)
  }
    
  # Return the data as a list.
  return(list(df_metrics=df_metrics, gg=gg, linmod=linmod, results=results, df_stats=df_stats))
}
