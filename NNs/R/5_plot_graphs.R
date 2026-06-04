plot_graphs <- function() {
  ########################
  # below was added by eoh
  
  # Make the necessary heatscatter plots.
  library(ggplot2)
  source("./R/heatscatter_liuqin.R")
  ######### fonts ############
  ## Try this on windwos if there is problem with font
  #library(extrafont)
  #font_import() #only do this one time - it takes a while
  #loadfonts(device="win")
  #windowsFonts(Times=windowsFont("TT Times New Roman"))
  ## The following three lines I had to execute in R on Linux for the
  ## Liberation Serif font (a replacement for Times New Roman font):
  library(showtext)
  #font_add("LiberationSerif", regular = "LiberationSerif-Regular.ttf",bold="LiberationSerif-Bold.ttf",italic="LiberationSerif-Italic.ttf",bolditalic="LiberationSerif-BoldItalic.ttf")
  font_add("Times New Roman", "timesbd.ttf") # font_add('Times New Roman', regular='C:/Windows/Fonts/timesbd.ttf') 
  showtext_auto()
  ######### fonts ############
  ##########
  # nn_pot_vs_nn_act 
  ##########
  nn_pot_vs_nn_act <- heatscatter_liuqin(
    df=df_all,      # The input dataframe.
    mod="nn_pot",   # The column-name of the datafram corresponding to
                    # the mod-variable.
    obs="nn_act",   # The column-name of the datafram corresponding to
                    # the obs-variable.
    moist=TRUE,     # Limit data to "moist" days (TRUE), "dry" days (FALSE)
                    # or include "all" days (NULL).
    swap.vars=FALSE,# Swap the obs and mod variables
    text.size=12,   # Font size.
    point.size=0.5, # The size of points on the graph.
    axis.font.size=1,  # Scaling factor for the font size for the axes.
    ## To add mathematical symbols, superscript and subscript see help for 
    ## plotmath in R, i.e. ?plotmath   or   help(plotmath).
    main.title=expression(paste("NN"["act"]," moist day")),
    mod.title=expression(paste("Predicted ","F"["rd"],sep="")),
    obs.title=expression(paste("Observed ","F"["rd"]),sep=""),
    mod.lim=c(0,1), # Limits for the mod-variable.
    obs.lim=c(0,1), # Limits for the obs-variable.
    plot.metrics=TRUE,        # Should the metrics be included as text on graph?
    metric.pos="bottomright", # Position of metrics: "bottomrignt" or "topleft".
    metric.size=1,            # Scaling factor for the font size of the metrics.
    plot.linmod=TRUE,         # plot regression line
    border=TRUE,         # Add border aroud graph (TRUE) or not (FALSE).
    n.tickmarks=6,       # Apporximate number of tickmarks on axes.
    forceorigin=TRUE,    # Force the origin (0,0) to be included in the graph.
    tag="(a)",           # A tag for the image or NULL to discard.
    timesnewroman=TRUE,  # TRUE=Timew New Roman, FALSE=Liberation Serif,
                         # NULL=default font used.
    dpi=300              # Resolution of jpeg image.
    #filnam=NA           # Output file name (without extension). Default name
                         # is composed of the name of the mod variable, followed
                         # by "_vs_" followed by the name of the obs variable. If
                         # swap.vars is TRUE then the string "_vars_swapped" is
                         # appended to the output file name. pdf and jpg files are
                         # created in the working  directory. If set to NA
                         # no output is created.
  )
  plot(nn_pot_vs_nn_act$gg)
  ##########
  # nn_pot_vs_obs
  ##########
  nn_pot_vs_obs <- heatscatter_liuqin(
    df=df_all,      # The input dataframe.
    mod="nn_pot",   # The column-name of the datafram corresponding to
                    # the mod-variable.
    obs="obs",      # The column-name of the datafram corresponding to
                    # the obs-variable.
    moist=TRUE,     # Limit data to "moist" days (TRUE), "dry" days (FALSE)
                    # or include "all" days (NULL).
    swap.vars=FALSE,# Swap the obs and mod variables
    text.size=12,   # Font size.
    point.size=0.5, # The size of points on the graph.
    axis.font.size=1,  # Scaling factor for the font size for the axes.
    ## To add mathematical symbols, superscript and subscript see help for 
    ## plotmath in R, i.e. ?plotmath   or   help(plotmath).
    main.title=expression(paste("NN"["act"]," moist day")),
    mod.title=expression(paste("Predicted ","F"["rd"],sep="")),
    obs.title=expression(paste("Observed ","F"["rd"]),sep=""),
    mod.lim=c(0,1), # Limits for the mod-variable.
    obs.lim=c(0,1), # Limits for the obs-variable.
    plot.metrics=TRUE,        # Should the metrics be included as text on graph?
    metric.pos="bottomright", # Position of metrics: "bottomrignt" or "topleft".
    metric.size=1,            # Scaling factor for the font size of the metrics.
    plot.linmod=TRUE,         # plot regression line
    border=TRUE,         # Add border aroud graph (TRUE) or not (FALSE).
    n.tickmarks=6,       # Apporximate number of tickmarks on axes.
    forceorigin=TRUE,    # Force the origin (0,0) to be included in the graph.
    tag="(a)",           # A tag for the image or NULL to discard.
    timesnewroman=TRUE,  # TRUE=Timew New Roman, FALSE=Liberation Serif,
                         # NULL=default font used.
    dpi=300              # Resolution of jpeg image.
    #filnam=NA           # Output file name (without extension). Default name
                         # is composed of the name of the mod variable, followed
                         # by "_vs_" followed by the name of the obs variable. If
                         # swap.vars is TRUE then the string "_vars_swapped" is
                         # appended to the output file name. pdf and jpg files are
                         # created in the working  directory. If set to NA
                         # no output is created.
  )
  plot(nn_pot_vs_obs$gg)
  ##########
  # nn_act_vs_nn_pot
  ##########
  nn_act_vs_nn_pot <- heatscatter_liuqin(
    df=df_all,      # The input dataframe.
    mod="nn_act",   # The column-name of the datafram corresponding to
                    # the mod-variable.
    obs="nn_pot",   # The column-name of the datafram corresponding to
                    # the obs-variable.
    moist=TRUE,     # Limit data to "moist" days (TRUE), "dry" days (FALSE)
                    # or include "all" days (NULL).
    swap.vars=FALSE,# Swap the obs and mod variables
    text.size=12,   # Font size.
    point.size=0.5, # The size of points on the graph.
    axis.font.size=1,  # Scaling factor for the font size for the axes.
    ## To add mathematical symbols, superscript and subscript see help for 
    ## plotmath in R, i.e. ?plotmath   or   help(plotmath).
    main.title=expression(paste("NN"["act"]," moist day")),
    mod.title=expression(paste("Predicted ","F"["rd"],sep="")),
    obs.title=expression(paste("Observed ","F"["rd"]),sep=""),
    mod.lim=c(0,1), # Limits for the mod-variable.
    obs.lim=c(0,1), # Limits for the obs-variable.
    plot.metrics=TRUE,        # Should the metrics be included as text on graph?
    metric.pos="bottomright", # Position of metrics: "bottomrignt" or "topleft".
    metric.size=1,            # Scaling factor for the font size of the metrics.
    plot.linmod=TRUE,         # plot regression line
    border=TRUE,         # Add border aroud graph (TRUE) or not (FALSE).
    n.tickmarks=6,       # Apporximate number of tickmarks on axes.
    forceorigin=TRUE,    # Force the origin (0,0) to be included in the graph.
    tag="(a)",           # A tag for the image or NULL to discard.
    timesnewroman=TRUE,  # TRUE=Timew New Roman, FALSE=Liberation Serif,
                         # NULL=default font used.
    dpi=300              # Resolution of jpeg image.
    #filnam=NA           # Output file name (without extension). Default name
                         # is composed of the name of the mod variable, followed
                         # by "_vs_" followed by the name of the obs variable. If
                         # swap.vars is TRUE then the string "_vars_swapped" is
                         # appended to the output file name. pdf and jpg files are
                         # created in the working  directory. If set to NA
                         # no output is created.
  )
  plot(nn_act_vs_nn_pot$gg)
  ##########
  ## more graphs if needed... use above as templates
  
  # above was added by eoh
  ########################
}
