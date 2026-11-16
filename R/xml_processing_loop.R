# name=xml_processing_loop.R
library(glue)
library(dplyr)

find_python <- function() {
  candidates <- c("python", "python3", "py")
  for (c in candidates) {
    p <- Sys.which(c)
    if (nzchar(p)) {
      if (basename(p) %in% c("py", "py.exe")) return(list(cmd = p, extra = "-3"))
      return(list(cmd = p, extra = NULL))
    }
  }
  stop("Python not found on PATH.")
}

process_one_xml <- function(chemin_xml,
                            python_script,
                            out_base_dir = "py_parquets",
                            record_tag = "InfosFoyerRSA",
                            py_batch_size = 3000,
                            parquet_final_output_dir = "BDD/datalist",
                            write_persons_parquet = TRUE,
                            write_addresses_parquet = TRUE,
                            return_final_df = FALSE,
                            existing_data_behavior.fc = "append") {
  stopifnot(file.exists(chemin_xml), file.exists(python_script))
  pyinfo <- find_python()
  xml_basename <- tools::file_path_sans_ext(basename(chemin_xml))
  out_dir <- file.path(out_base_dir, paste0(xml_basename, "_parquet"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  py_args <- c(if(!is.null(pyinfo$extra)) pyinfo$extra, python_script, chemin_xml, out_dir, record_tag, as.character(py_batch_size))
  res_py <- system2(command = pyinfo$cmd, args = py_args, stdout = TRUE, stderr = TRUE)
  # check for success - python prints 'Finished.' at the end: check last lines or status
  # For simplicity, assume success if out_dir contains part-*.parquet
  parts <- list.files(out_dir, pattern = "^part-.*\\.parquet$", full.names = TRUE)
  if (length(parts) == 0) {
    stop(glue("Python conversion failed or produced no parts for {chemin_xml}. stdout/stderr:\n{paste(res_py, collapse='\\n')}"))
  }

  # Source R helper and process parquet -> long
  source("xml_parquet_to_long.R")  # must be present in working dir
  res_r <- process_parquet_and_make_long(out_dir, max_persons = NULL)

  # Optionally write persons_long / address_selected to parquet
  if (write_persons_parquet && nrow(res_r$persons_long) > 0) {
    persons_dir <- file.path(out_dir, "persons_parquet")
    dir.create(persons_dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_dataset(res_r$persons_long, path = persons_dir)
  }
  if (write_addresses_parquet && nrow(res_r$address_selected) > 0) {
    addr_dir <- file.path(out_dir, "address_selected_parquet")
    dir.create(addr_dir, recursive = TRUE, showWarnings = FALSE)
    arrow::write_dataset(res_r$address_selected, path = addr_dir)
  }

  # final dataframe to write into your central parquet BDD
  final_df <- res_r$final_joined %>% mutate(FICHIER_SOURCE = chemin_xml)
  # ensure DATE and INSTITUTION exist or create placeholders if needed before grouping
  if (!"DATE" %in% names(final_df)) final_df$DATE <- NA_character_
  if (!"INSTITUTION" %in% names(final_df)) final_df$INSTITUTION <- NA_character_

  # write (append) to central parquet dataset
  arrow::write_dataset(dataset = final_df %>% dplyr::group_by(DATE, INSTITUTION),
                       path = parquet_final_output_dir,
                       existing_data_behavior = existing_data_behavior.fc)

  out <- list(ok = TRUE, chemin_xml = chemin_xml, out_dir = out_dir, final_df = if (return_final_df) final_df else NULL)
  return(out)
}

process_multiple_xmls <- function(xml_files,
                                  python_script,
                                  out_base_dir = "py_parquets",
                                  record_tag = "InfosFoyerRSA",
                                  py_batch_size = 3000,
                                  parquet_final_output_dir = "BDD/datalist",
                                  write_persons_parquet = TRUE,
                                  write_addresses_parquet = TRUE,
                                  return_final_df = FALSE,
                                  existing_data_behavior.fc = "append") {
  results <- vector("list", length(xml_files))
  for (i in seq_along(xml_files)) {
    message(glue("Processing {i}/{length(xml_files)} : {basename(xml_files[i])}"))
    results[[i]] <- process_one_xml(
      chemin_xml = xml_files[i],
      python_script = python_script,
      out_base_dir = out_base_dir,
      record_tag = record_tag,
      py_batch_size = py_batch_size,
      parquet_final_output_dir = parquet_final_output_dir,
      write_persons_parquet = write_persons_parquet,
      write_addresses_parquet = write_addresses_parquet,
      return_final_df = return_final_df,
      existing_data_behavior.fc = existing_data_behavior.fc
    )
  }
  names(results) <- xml_files
  return(results)
}