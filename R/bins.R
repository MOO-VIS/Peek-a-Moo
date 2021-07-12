#' Generate Feed Bin Plot for a Particular Date
#'
#' @param feed_df The data frame containing feed data
#'
#' @return NULL
feed_bin_plot <- function(feed_df){
  ## feed bin plot section
  feed_df <- convert_date_col(feed_df) %>%
    mutate(Startweight = case_when(Startweight < 0 ~ 0, # set to zero where weight is negative
                                   TRUE ~ Startweight),
           Endweight = case_when(Endweight < 0 ~ 0,
                                 TRUE ~ Endweight))
  
  max_wt <- feed_df$Startweight %>% max() # find maximum capacity
  # choose date
  one_bin_df <- feed_df %>% group_by(Bin) %>% arrange(Start) %>% slice(1)
  # Adapted plot from 
  # https://stackoverflow.com/questions/48522350/create-an-image-filled-chart-in-r-using-ggplot
  # Load png file from imgur as binary
  container_img <- readPNG("www/container.png")
  h <- dim(container_img)[1]
  w <- dim(container_img)[2]  
  
  # Find the rows where image starts & ends
  pos1 <- which(apply(container_img[,,1], 1, function(y) any(y==1)))
  mn1 <- min(pos1)
  mx1 <- max(pos1)
  
  # manual colouring for plot
  cols <- c(rgb(255,255,255,maxColorValue = 255),  # bg
            rgb(237,237,237,maxColorValue = 255),  # empty
            rgb(42,128,183,maxColorValue = 255))   # full
  full_df <- data.frame(Var1=integer(),
                        Var2=integer(), 
                        value=integer(), 
                        Bin=integer()) 
  for (n in one_bin_df$Bin){ #one_bin_df$Bin c(1:3)
    imgmtx <- container_img[h:1,,1]
    whitemtx <- (imgmtx==1)
    pospct <- one_bin_df %>% filter(Bin==n) %>% .$Startweight %>%
      magrittr::divide_by(max_wt) %>%
      magrittr::multiply_by(mx1-mn1) %>%
      magrittr::add(mn1) %>% round() 
    colmtx <- matrix(rep(FALSE,h*w),nrow=h) 
    colmtx[mx1:pospct, 1:w] <- TRUE
    imgmtx[whitemtx & colmtx] <- 0.5
    # Need to melt the matrix into a data.frame that ggplot can understand
    df <- reshape2::melt(imgmtx) 
    df <- df %>% mutate(Bin = n)
    full_df <- rbind(full_df, df) # append to get all bins into one df
  }
  
  full_df <- full_df %>% filter(Bin < 14)
  
  # Then use a heatmap with 3 colours for background, and percentage fills
  # Converting the fill value to a factor causes a discrete scale.
  # geom_tile takes three columns: x, y, fill corresponding to 
  # x-coord, y-coord, and colour of the cell.
  ggplot(full_df, aes(x = Var2, y = Var1, fill = factor(value)))+
    geom_tile() +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    facet_wrap(~Bin)
}