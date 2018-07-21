#knmi_load.R
#Version     : 1.0.0
#version date: 2018_07_21 (YYYYMMDD)
#author      : R.A. Posthumus
#author email: raposthumus@gmail.com
#purpose:
# This is code is to make plots/animation for hot_days (based on KNMI data, sourced at station 260 De Bilt)
# Code developed by R.A. Posthumus, with many thanks to @datagraver

#library that are needed 
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(magick)
library(readr)
library(tidyverse)

#some global constants
max_temp <- 20 #set temp cutoff for warm days
ima_width <- 800L #2975L 
ima_height <- 600L #1662L
ima_res <- 96L
line_alpha <- 0.6
graycol <- "#AAAAAA"
redcol <- "#DD0000"

#helper function for getting the number of days upto a certain (counted from month=1 , day=1)
#' 
#' @param y year
#' @param m month
#' @param d day
#'
#' @return number of days based on given datum
#' @export: none
#' 
daynumber <- function(y , m , d) {
  #setup a valid date object
  dt <- today()
  #update with given numbers
  year(dt) <- y
  month(dt) <- m
  day(dt) <- d
  #make a copy for starting point
  ds <- dt
  #update with YYYYMMDD=y0101
  year(ds) <- y
  month(ds) <-  1L
  day(ds) <-  1L
  #return the number of days
  dt - ds + 1
}

#function for getting the data from the KNMI-site
#' Title load_the_data
#'
#' @param save_to_fname the filename the loaded data will be saved to
#'
#' @return global: working_data
#' @export none
#'
load_the_data <- function (save_to_fname="knmi_260.csv") {
  temp_in <- tempfile(pattern = "file" , tmpdir = getwd())
  download.file(
    "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_260.zip" ,
    temp_in
  )
  data <- readLines(unz(temp_in , "etmgeg_260.txt"))
  #save the file
  unlink(temp_in)
  data  <- dplyr::tbl_df(data)
  nrow(data)
  #delete first 48 lines (contains only header info)
  data <- data[48:nrow(data) , ]
  #skip empty lines
  data <- data[!apply(data == "" , 1 , all) , ]
  #save data as csv file and readback the csv
  data <-
    mutate_if(data ,
              is.character ,
              str_replace_all ,
              pattern = '\"' ,
              replacement = "")
  data <-
    mutate_if(data ,
              is.character ,
              str_replace_all ,
              pattern = ' ' ,
              replacement = "")
  write.table(
    data ,
    file = save_to_fname ,
    quote = FALSE ,
    sep = "," ,
    col.names = FALSE ,
    row.names = FALSE
  )
  #output working_data as a global object
  working_data <<- readr::read_delim(save_to_fname, delim = ",")
}

#function for cleaning up the working_data
#select only TX column , temp calc , add date components , add indicator for hot days
#global output: working data cur_year , cur_month , cur_day is set to max values in this dataset
#' Title: clean_up_the_data
#'
#' @return working_data,cur_year,cur_month,cur_day
#' @export none
#'
#'
clean_up_the_data  <- function() {
  #only YYYYMD and TZ columns needed
  working_data <- dplyr::select(working_data , YYYYMMDD , TX)
  #calc temp (unit is 0.1 C)
  working_data <- dplyr::mutate(working_data , temp = 0.1 * TX)
  #work on the date values
  working_data <- dplyr::mutate(working_data , date_val = lubridate::ymd(YYYYMMDD))
  working_data <- dplyr::mutate(working_data , jaar = lubridate::year(date_val))
  working_data <- dplyr::mutate(working_data , maand = lubridate::month(date_val))
  working_data <- dplyr::mutate(working_data , dag = lubridate::day(date_val))
  working_data <- dplyr::select(working_data , date_val , jaar , maand , dag , temp)
  #add a column that gives the daynumber (used for comparing time windows)
  working_data <- dplyr::mutate(working_data , daycounter = daynumber(jaar , maand , dag))
  #and get the max values of the year/month/day values
  working_data <- dplyr::arrange(working_data , desc(date_val))
  #add color scheme / temp
  working_data <- dplyr::mutate(working_data , is_hot = temp > max_temp)
  #we only want the hot days , so output them globally
  working_data <<- dplyr::filter(working_data , is_hot)
  #output current max values globally
  cur_year <<- working_data$jaar[1]
  cur_month <<- working_data$maand[1]
  cur_day <<- working_data$dag[1]
}

make_restricted_plot <- function(fname = "") {
  #restrict working_data to the current portion of year (as indicated by daycounter)
  c_dc <- daynumber(cur_year , cur_month , cur_day)
  graph_data <- dplyr::filter(working_data , daycounter <= c_dc)
  graph_data <- graph_data %>% count(jaar , sort = TRUE)
  #we need an indicator for the current year , in order to plot it in red later on
  graph_data <-
    dplyr::mutate(graph_data , is_curr_year = (jaar == cur_year))
  #make the horizontal barplot
  p <-
    ggplot(graph_data , aes(
      x = reorder(jaar , n) ,
      y = n ,
      fill = is_curr_year
    )) +
    geom_bar(position = "dodge" ,
             stat = "identity" ,
             show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = graycol , "TRUE" = redcol)) +
    labs(x = "Jaar" , y = "Aantal") +
    coord_flip() +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0.5))) +
    ggtitle(
      paste(
        "Warme dagen per jaar sinds 1901 (>" ,
        as.character(max_temp) ,
        "C De Bilt)" ,
        "op basis van current periode."
      )
    )
  #and save it
  if (fname == "") {
    #we have no file name , so lets make one
    ggsave(paste(
      "img_r" ,
      sprintf("%04i" , cur_year) ,
      "_" ,
      sprintf("%02i" , cur_month) ,
      "_" ,
      sprintf("%02i" , cur_day) ,
      ".png" ,
      sep = ""
    ) ,
    p)
  }
  else {
    #save under the given filename
    ggsave(fname , p)
  }
}

make_unrestricted_plot <- function(fname = "") {
  #use data from all hot days
  graph_data <- working_data
  graph_data <- graph_data %>% count(jaar , sort = TRUE)
  #we need an indicator for the current year , in order to plot it in red later on
  graph_data <- dplyr::mutate(graph_data , is_curr_year = (jaar == cur_year))
  #make the horizontal barplot
  p <-
    ggplot(graph_data , aes(
      x = reorder(jaar , n) ,
      y = n ,
      fill = is_curr_year
    )) +
    geom_bar(position = "dodge" ,
             stat = "identity" ,
             show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = graycol , "TRUE" = redcol)) +
    labs(x = "Jaar" , y = "Aantal") +
    coord_flip() +
    theme_bw() +
    theme(axis.text.y = element_text(size = rel(0.5))) +
    ggtitle(paste(
      "Warme dagen per jaar sinds 1901 (>" ,
      as.character(max_temp) ,
      "C De Bilt)."
    ))
  #and save it.
  if (fname == "") {
    #we have no file name , so lets make one
    ggsave(paste(
      "img_u" ,
      sprintf("%04i" , cur_year) ,
      "_" ,
      sprintf("%02i" , cur_month) ,
      "_" ,
      sprintf("%02i" , cur_day) ,
      ".png" ,
      sep = ""
    ) ,
    p)
  }
  else {
    ggsave(fname , p)
  }
}

save_animated <- function(save_indidual_frames = FALSE ,
                          fname = "") {
  #setup the image device
  img <- image_graph(ima_width , ima_height , res = ima_res)
  #initialize counters
  dt <- today()
  year(dt) <- cur_year
  dagteller <- 1L
  #initialize text progressbar
  pb_max <- as.numeric(daynumber(cur_year , cur_month , cur_day))
  print("Start looping for all frames")
  pb <- txtProgressBar(min = 0 , max = pb_max , style = 3)
  #do all the calculations
  for (mc in 1:cur_month) {
    #run over all months
    month(dt) <- mc
    for (dc in 1:days_in_month(dt)) {
      #runover all days in a month
      c_dc <- daynumber(cur_year , mc , dc)
      graph_data <- dplyr::filter(working_data , daycounter <= c_dc)
      graph_data <- graph_data %>% count(jaar , sort = TRUE)
      #we need an indicator for the current year , in order to plot it in red later on
      graph_data <- dplyr::mutate(graph_data , is_curr_year = (jaar == cur_year))
      #make the horizontal barplot
      p <-
        ggplot(graph_data ,
               aes(
                 x = reorder(jaar , n) ,
                 y = as.numeric(as.character(n)) ,
                 fill = is_curr_year
               )) +
        geom_bar(position = "dodge" ,
                 stat = "identity" ,
                 show.legend = FALSE) +
        scale_fill_manual(values = c("FALSE" = graycol , "TRUE" = redcol)) +
        labs(x = "Jaar" , y = "Aantal") +
        coord_flip() +
        theme_bw() +
        theme(axis.text.y = element_text(size = rel(0.5))) +
        ylim(0L , daynumber(cur_year , cur_month , cur_day) + 1) +
        geom_hline(
          yintercept = dagteller ,
          linetype = "dotted" ,
          color = "blue" ,
          alpha = line_alpha
        ) +
        ggtitle(
          paste(
            "Warme dagen per jaar sinds 1901 (>" ,
            as.character(max_temp) ,
            "C De Bilt)" ,
            "op basis van current periode , dag:" ,
            as.character(dagteller)
          )
        )
      #and plot it.
      if (save_indidual_frames == TRUE) {
        ggsave(
          paste(
            "img_" ,
            as.character(cur_year) ,
            "_" ,
            sprintf("%02i" , mc) ,
            "_" ,
            sprintf("%02i" , dc) ,
            "_" ,
            sprintf("%03i" , dagteller) ,
            ".png" ,
            sep = ""
          ) ,
          p
        )
      }
      print(p)
      # update progress bar
      setTxtProgressBar(pb , dagteller)
      dagteller <- dagteller + 1
    }
  }
  #we are done , close the progressbar
  close(pb)
  #close the graphics device
  dev.off()
  print("end looping over the frames")
  #make the animation
  print("start making the animation")
  animation <- image_animate(img , fps = 1)
  print("end making the animation")
  #and save it
  print("start the animation")
  if (fname == "") {
    #we have no file name , so lets make one
    image_write(animation ,
                paste(
                  "img_r" ,
                  sprintf("%04i" , cur_year) ,
                  sprintf("%02i" , cur_month) ,
                  sprintf("%02i" , cur_day) ,
                  ".gif" ,
                  sep = ""
                ))
  }
  else {
    image_write(animation , fname)
  }
  print("the animation is saved")
}

#function to do all the working:getting , cleaning , plotting , animation
make_the_plots <-
  function(get_data_from_server = TRUE ,
           clean_the_data = TRUE ,
           make_unrestricted_plot = TRUE ,
           make_restricted_plot = TRUE ,
           make_animation = TRUE) {
    if (get_data_from_server == TRUE) {
      load_the_data("knmi_260.csv")
    }
    if (clean_the_data == TRUE) {
      clean_up_the_data()
    }
    if (make_unrestricted_plot == TRUE) {
      make_unrestricted_plot(fname = "")
    }
    if (make_restricted_plot == TRUE) {
      make_restricted_plot(fname = "")
    }
    if (make_animation == TRUE) {
      save_animated(save_indidual_frames = FALSE , fname = "")
    }
  }

make_the_plots(
  get_data_from_server = TRUE ,
  clean_the_data = TRUE ,
  make_unrestricted_plot = TRUE ,
  make_restricted_plot = TRUE ,
  make_animation = TRUE
)
