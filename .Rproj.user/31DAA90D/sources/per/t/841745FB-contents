#' Function to plot mortality plots
#'
#' @param folder_name path containing the results
#' @param type_plot cdr_plot, cdr_u5_plot
#' @return plot
#' @export
f_plot_morality_tab <- function(folder_name, type_plot){
  if(type_plot == 'cdr_u5_plot'){
    return(f_plot_cdr_u5(folder_name))
  }else{
    return(f_plot_cdr(folder_name))
  }
}

#' Function to plot cdr under 5 over time
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_plot_cdr_u5 <- function(folder_name){
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
      dplyr::mutate(start_date = as.Date(paste(min_day_survey, '/', min_month_survey, '/', min_year_survey, sep=""),
                                         format = "%d/%m/%Y"),
                    end_date = as.Date(paste(max_day_survey, '/', max_month_survey, '/', max_year_survey, sep=""),
                                       format = "%d/%m/%Y")) |>
      dplyr::rename(survey_id = new_name) |>
      dplyr::select(survey_id, start_date, end_date, recall_days,
                    lshtm_cdr_est, lshtm_cdr_u5_est)
    # Add stard and end date into our dataset
    metadata$start_year <- lubridate::year(as.Date(metadata$start_date))
    metadata$start_month <- lubridate::month(as.Date(metadata$start_date))
    metadata$end_year <- lubridate::year(as.Date(metadata$end_date))
    metadata$end_month <- lubridate::month(as.Date(metadata$end_date))
    metadata$recall_mid <- as.Date(metadata$end_date) - metadata$recall_days/2
    metadata$year_recall_mid <- lubridate::year(metadata$recall_mid)
    metadata$month_recall_mid <- lubridate::month(metadata$recall_mid)

    dr_lab <- "Survey estimates of under 5 years death rate (per 10,000 child-days)"
    plot <- ggplot2::ggplot(data = metadata, ggplot2::aes(x = factor(year_recall_mid), y= lshtm_cdr_u5_est)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_continuous(dr_lab)+
      ggplot2::theme(title = ggplot2::element_blank())+
      ggplot2::labs(x = "Year") +
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=16, angle=45, vjust=1, hjust=1),
                     axis.title.y = ggplot2::element_text(size=12),
                     axis.title.x = ggplot2::element_text(size=18),
                     axis.text.y = ggplot2::element_text(size=14))

    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/',
                          'cdr_u5', '_per_year.png', sep=''),
                    dpi = "print", units = "cm", width = 20, height =20)
    return(plot)
  }
}

#' Function to plot cdr over time
#'
#' @param folder_name path containing the results
#' @return plot
#' @export
f_plot_cdr <- function(folder_name){
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
      dplyr::mutate(start_date = as.Date(paste(min_day_survey, '/', min_month_survey, '/', min_year_survey, sep=""),
                                         format = "%d/%m/%Y"),
                    end_date = as.Date(paste(max_day_survey, '/', max_month_survey, '/', max_year_survey, sep=""),
                                       format = "%d/%m/%Y")) |>
      dplyr::rename(survey_id = new_name) |>
      dplyr::select(survey_id, start_date, end_date, recall_days,
                    lshtm_cdr_est, lshtm_cdr_u5_est)
    # Add stard and end date into our dataset
    metadata$start_year <- lubridate::year(as.Date(metadata$start_date))
    metadata$start_month <- lubridate::month(as.Date(metadata$start_date))
    metadata$end_year <- lubridate::year(as.Date(metadata$end_date))
    metadata$end_month <- lubridate::month(as.Date(metadata$end_date))
    metadata$recall_mid <- as.Date(metadata$end_date) - metadata$recall_days/2
    metadata$year_recall_mid <- lubridate::year(metadata$recall_mid)
    metadata$month_recall_mid <- lubridate::month(metadata$recall_mid)

    dr_lab <- "Survey estimates of crude death rate (per 10,000 person-days)"
    plot <- ggplot2::ggplot(data = metadata, ggplot2::aes(x = factor(year_recall_mid), y= lshtm_cdr_est)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_continuous(dr_lab)+
      ggplot2::theme(title = ggplot2::element_blank())+
      ggplot2::labs(x = "Year") +
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=16, angle=45, vjust=1, hjust=1),
                     axis.title.y = ggplot2::element_text(size=12),
                     axis.title.x = ggplot2::element_text(size=18),
                     axis.text.y = ggplot2::element_text(size=14))
    ggplot2::ggsave(paste(folder_name, '/', 'visualization_output', '/',
                          'cdr', '_per_year.png', sep=''),
                    dpi = "print", units = "cm", width = 20, height = 20)
    return(plot)
  }
}
