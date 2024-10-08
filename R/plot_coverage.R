#' Function to plot one of the coverage plots
#'
#' @param folder_name path containing the results
#' @param type_plot qs_hist, ss_hist, rd_hist, smart_coverage
#' @return plot
#' @export
f_plot_coverage_tab <- function(folder_name, type_plot){
  if(type_plot == 'qs_hist'){
    return(f_hist_quality_score(folder_name))
  }else if(type_plot == 'ss_hist'){
    return(f_hist_sample_size(folder_name))
  }else if(type_plot == 'rd_hist'){
    return(f_hist_recall_days(folder_name))
  }else{
    return(f_coverage_data(folder_name))
  }
}

#' Function to plot quality score coverage
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_hist_quality_score <- function(folder_name){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'metadata_clean', x=all_files)]
  # Check if the folder exists
  if (!dir.exists(paste(folder_name, '/', 'visualization_output', sep=''))) {
    # If the folder does not exist, create it
    dir.create(paste(folder_name, '/', 'visualization_output', sep=''), recursive = TRUE)
  }
  if(length(all_files) != 0){
    metadata <- rio::import(paste(folder_name, '/', all_files, sep='')) |>
      dplyr::mutate(quality_score = ifelse(is.na(overall_score), 100, 100-overall_score))
    plot_freq <- ggplot2::ggplot(metadata, ggplot2::aes(quality_score)) +
      ggplot2::geom_histogram(fill ="#56B4E9", color='black') +
      ggplot2::ylab('Frequency') +
      ggplot2::xlab('Survey Quality Score') +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size=20),
                     axis.text.x = ggplot2::element_text(size=18),
                     axis.title.y = ggplot2::element_text(size=20),
                     axis.text.y = ggplot2::element_text(size=18))
    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/', 'hist_quality_score.png',sep=""),
                    height = 20, width = 20, units = "cm", dpi = "print")
    return(plot_freq)
  }
}

#' Function to plot sample size
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_hist_sample_size <- function(folder_name){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'metadata_clean', x=all_files)]
  # Check if the folder exists
  if (!dir.exists(paste(folder_name, '/', 'visualization_output', sep=''))) {
    # If the folder does not exist, create it
    dir.create(paste(folder_name, '/', 'visualization_output', sep=''), recursive = TRUE)
  }
  if(length(all_files) != 0){
    metadata <- rio::import(paste(folder_name, '/', all_files, sep='')) |>
      dplyr::mutate(n = ifelse(is.na(sample_size_nutrition), 0, sample_size_nutrition))
    plot_freq <- ggplot2::ggplot(metadata, ggplot2::aes(n)) +
      ggplot2::geom_histogram(fill ="#CC79A7", color='black') +
      ggplot2::ylab('Frequency') + ggplot2::xlab('Mean Survey Sample Size') + ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size=20),
                     axis.text.x = ggplot2::element_text(size=18),
                     axis.title.y = ggplot2::element_text(size=20),
                     axis.text.y = ggplot2::element_text(size=18))
    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/', 'hist_sample_size.png',sep=""),
                    height = 20, width = 20, units = "cm", dpi = "print")
    return(plot_freq)
  }
}

#' Function to plot recall days
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_hist_recall_days <- function(folder_name){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'metadata_clean', x=all_files)]
  # Check if the folder exists
  if (!dir.exists(paste(folder_name, '/', 'visualization_output', sep=''))) {
    # If the folder does not exist, create it
    dir.create(paste(folder_name, '/', 'visualization_output', sep=''), recursive = TRUE)
  }
  if(length(all_files) != 0){
    metadata <- rio::import(paste(folder_name, '/', all_files, sep=''))
    plot_freq <- ggplot2::ggplot(metadata, ggplot2::aes(recall_days)) +
      ggplot2::geom_histogram(fill ="#009E73", color='black') +
      ggplot2::ylab('Frequency') +
      ggplot2::xlab('Survey Recall Period Range') +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size=20),
                     axis.text.x = ggplot2::element_text(size=18),
                     axis.title.y = ggplot2::element_text(size=20),
                     axis.text.y = ggplot2::element_text(size=18))
    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/', 'hist_recall_days.png',sep=""),
                    height = 20, width = 20, units = "cm", dpi = "print")
    return(plot_freq)
  }
}

#' Function to plot coverage
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_coverage_data <- function(folder_name){
  ## Select all the files in the folder
  all_files <- list.files(path = folder_name, pattern = ".csv")
  ## Find the cluster one
  all_files <- all_files[grepl(pattern = 'metadata_clean', x=all_files)]
  # Check if the folder exists
  if (!dir.exists(paste(folder_name, '/', 'visualization_output', sep=''))) {
    # If the folder does not exist, create it
    dir.create(paste(folder_name, '/', 'visualization_output', sep=''), recursive = TRUE)
  }
  if(length(all_files) != 0){
    metadata <- rio::import(paste(folder_name, '/', all_files, sep=''))|>
      dplyr::mutate(quality_score = ifelse(is.na(overall_score), 100, 100-overall_score))|>
      dplyr::mutate(start_date = as.Date(paste('01', '/', min_month_survey, '/', min_year_survey, sep=""),
                                         format = "%d/%m/%Y"),
                    end_date = as.Date(paste('01', '/', max_month_survey, '/', max_year_survey, sep=""),
                                       format = "%d/%m/%Y"))
    # Calculate the coveagre of the survey
    metadata$coverage <- metadata$sum_p_time*metadata$quality_score
    metadata <- metadata |> dplyr::mutate(coverage = ifelse(is.na(coverage), 0, coverage))
    metadata$coverage <- metadata$coverage / max(metadata$coverage)
    # Then we need to change the date to time unit to plot than
    ts <- f_gen_ts(y_start = min(metadata$min_year_survey, na.rm = TRUE) - 1,
                   y_end = max(metadata$max_year_survey, na.rm = TRUE),
                   m_start = 1,
                   m_end=12,
                   burn_in_period = 1,
                   burn_out_period = 0)
    metadata <- merge(metadata, ts[, c('time_unit', 'date')], by.x = 'start_date', by.y='date', all.x=TRUE) |>
      dplyr::rename(time_unit_start = time_unit)
    metadata <- merge(metadata, ts[, c('time_unit', 'date')], by.x = 'end_date', by.y='date', all.x=TRUE) |>
      dplyr::rename(time_unit_end= time_unit)

    ## now we can exapend the infroamtion for the range of time unit
    data_coverage <- metadata |>
      dplyr::rowwise() |>
      dplyr::mutate(time_unit = list(seq(time_unit_start, time_unit_end))) |>
      tidyr::unnest(time_unit) |>
      dplyr::select(coverage, time_unit)

    data_coverage <- merge(data_coverage[, c('time_unit', 'coverage')],
                           ts,
                           by = 'time_unit', all.y = TRUE) |>
      dplyr::mutate(coverage = ifelse(is.na(coverage), 0, coverage))

    # Add a placeholder variable for the y-axis
    data_coverage$placeholder <- 1

    # Create a sequence for every other month
    seq_idx <- seq(1, nrow(ts), by = 2)

    # Select breaks and labels accordingly
    breaks <- unique(ts[, "time_unit"])[seq_idx]
    labels <- ts$month[seq_idx]

    plot <- ggplot2::ggplot(data_coverage, ggplot2::aes(y = placeholder, x = time_unit) ) +
      ggplot2::geom_tile(ggplot2::aes(fill=coverage), colour = "grey80", show.legend = FALSE) +
      ggplot2::scale_x_continuous("month, year", expand=c(0,0),
                                  breaks = breaks,
                                  labels = labels )  +
      ggplot2::scale_fill_gradientn(colours = c("grey90", "yellow", "red"),
                                    values = c(0, 0.0000001, 1 )) +
      ggplot2::facet_grid(.~year, space="free", scales="free", switch="x") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.placement = "outside",
                     strip.background = ggplot2::element_rect(fill=NA, colour="white"),
                     panel.spacing=ggplot2::unit(0,"cm"),
                     #strip.text.y = ggplot2::element_text(angle = 0, size = 16),
                     strip.text.x = ggplot2::element_text(size=12),
                     axis.title.x = ggplot2::element_text(size=12),
                     axis.title.y =ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size=7, angle = 20),
                     axis.text.y = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/', 'coverage.png',sep=""),
                    height = 10, width = 40, units = "cm", dpi = "print")

    return(plot)
  }
}


