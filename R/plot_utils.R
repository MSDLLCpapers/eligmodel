plot_theme <- function(
    # FUNCTION INPUTS with defaults following AG Best Practice
  # See documentation for full explanation on each component
  
  # 0. UNIVERSAL STYLIZATION
  font = "Source Sans Pro",
  
  # 1. TITLE STYLE
  title.hjust = 0.5,
  title.vjust = 0,
  title.size = 16,
  # These arguments will apply to subtitle and legend alignment
  # as well.
  
  # 2. SUBTITLE STYLE
  subtitle.face = "bold.italic",
  subtitle.size = 14,
  
  # 3. LEGEND STYLE (if it exists) (no title - AG Best Practice)
  legend.textsize = 14,
  # Future args for number of rows and columns wanted in the legend?
  # Future arg for order? No, order of legend should be handled outside of ag_theme()
  
  # 4. YAXIS TITLE STYLE
  yaxis.title.size = 16,
  
  # below options are important if orientation needs to be horizontal
  yaxis.title.orientation = "vertical",
  yaxis.title.pull = 0, # "Pulls" graph toward y axis for alignment.
  yaxis.title.vjust = NULL, # Lifts y axis title above the yaxis for clear reading.
  
  
  # 5. XAXIS TITLE STYLE
  xaxis.title.size = 14,
  
  # 6. AXIS TEXT DEFAULTS
  axistext.size = 12
  
) {
  
  #### INTERMEDIATE PARAMETER MODIFIERS BEGIN
  
  # Intermediate params signified by _ instead of .
  
  # 4. YAXIS.TITLE
  # HANDLING HORIZONTAL VS. VERTICAL ORIENTATION
  # If they want the yaxis title to be horizontal...
  if (yaxis.title.orientation == "horizontal") {
    
    # We adjust the angle of the y axis
    yaxis_angle <- 0
    
    # We left align the yaxis title in the column of space it takes up
    yaxis_hjust <- 0
    
    # We modify the vjust of the title to give enough space
    if (is.null(yaxis.title.vjust)) {
      yaxis_vjust <- 1.05
    }
    
    # We give them a message if they haven't modified the pull arg, needed for horizontal
    # orientation.
    if (yaxis.title.pull == 0) {
      print("To align the yaxis title with the yaxis, use the yaxis.title.pull argument in theme to 'pull' the graph toward the yaxis title. e.g., yaxis.title.pull = 90. Use view to look at the pdf version of the plot to adjust properly.")
    }
  }
  
  # Otherwise we prepare intermediate parameters for a vertically oriented y-axis title
  else {
    yaxis_angle <- 90
    yaxis_hjust <- 0.5
    if (is.null(yaxis.title.vjust)) {
      yaxis_vjust <- 3
    }
  }
  # We override yaxis_vjust value if user specified one:
  if (!is.null(yaxis.title.vjust)) {
    yaxis_vjust <- yaxis.title.vjust
  }
  #### INTERMEDIATE PARAMETER MODIFIERS END
  
  
  # BEGIN RETURN
  return(
    ggplot2::theme(
      # Get rid of 'ugly' plot elements
      panel.grid.major = ggplot2::element_blank(),
      # set major grid line type (default none)
      panel.grid.minor = ggplot2::element_blank(),
      # set minor grid line type (default none)
      panel.border = ggplot2::element_blank(),
      # get rid of border
      panel.background = ggplot2::element_blank(),
      # get rid of gray background
      axis.line = ggplot2::element_line(colour = "black"),
      # black axes
      
      
      ## 1. TITLE
      # Stylize title based on user input. AG best practice defaults.
      plot.title = ggplot2::element_text(
        hjust = title.hjust,
        # Horizontal adjustment
        vjust = title.vjust,
        # Vertical Adjustment
        family = font,
        # Font
        face = "bold",
        # Bold, plain, or Italic
        size = title.size # Size
        # ,debug = TRUE # show box for error testing
      ),
      
      ## 2. SUBTITLE
      plot.subtitle = ggplot2::element_text(
        hjust = title.hjust,
        vjust = title.vjust,
        family = font,
        face = subtitle.face,
        size = subtitle.size
      ),
      
      # 3. LEGEND STYLE
      legend.position = "top",
      legend.key = ggplot2::element_blank(),
      # get rid of background behind legend symbols
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = font,
        face = "plain",
        size = legend.textsize,
        color = "black"
        # ,debug = TRUE # show box
      ),
      
      # 4. YAXIS TITLE
      axis.title.y = ggplot2::element_text(
        family = font,
        # font
        face = "bold",
        # face
        size = yaxis.title.size,
        # size
        angle = yaxis_angle,
        margin = ggplot2::margin(r = -yaxis.title.pull),
        vjust = yaxis_vjust,
        hjust = yaxis_hjust
        #debug = TRUE # show box
      ),
      
      # 5. XAXIS TITLE
      axis.title.x = ggplot2::element_text(
        family = font,
        # font
        face = "bold",
        # face
        size = xaxis.title.size # size
        # ,debug = TRUE # show box
      ),
      
      
      
      # 6. PLOT MARGIN
      plot.margin = ggplot2::unit(
        c(
          0.2,
          0.2,
          0.2,
          0.2
        ),
        "in"
      ),
      
      # 7. AXIS STYLE
      # Universal
      axis.text = ggplot2::element_text(
        family = font,
        size = 12,
        colour = "black"
        # ,debug = TRUE # show box
      ),
      
      # 8. TICKS STYLE
      axis.ticks = ggplot2::element_line(color = "black")
      #make ticks black
      
    ) # End theme
  ) # End return
} 


