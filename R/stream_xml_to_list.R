# stream_xml_to_list.R
# Retourne une liste où chaque élément est la représentation xml2::as_list()
# d'un élément <record_tag> extrait du fichier xml_file.
stream_xml_to_list <- function(xml_file, record_tag = "InfosFoyerRSA",
                               encoding = "UTF-8", max_lines_buffer = 200000L,
                               report_every = 500L) {
  if (!file.exists(xml_file)) stop("Fichier introuvable: ", xml_file)
  start_re <- paste0("<(?:[A-Za-z0-9_:-]+:)?", record_tag, "\\b")
  end_re   <- paste0("</(?:[A-Za-z0-9_:-]+:)?", record_tag, "\\s*>")
  in_fragment <- FALSE
  buf <- character(0)
  out_list <- list()
  con <- file(xml_file, open = "r", encoding = encoding)
  on.exit(close(con))
  lineno <- 0L
  written <- 0L
  while(TRUE) {
    line <- readLines(con, n = 1L, warn = FALSE)
    if (length(line) == 0L) break
    lineno <- lineno + 1L
    if (!in_fragment) {
      if (grepl(start_re, line, perl = TRUE)) {
        in_fragment <- TRUE
        buf <- line
      } else {
        next
      }
    } else {
      buf <- c(buf, line)
    }
    if (in_fragment && grepl(end_re, line, perl = TRUE)) {
      frag_text <- paste0(buf, collapse = "\n")
      # try to parse fragment safely
      parsed <- tryCatch({
        node <- xml2::read_xml(frag_text, encoding = encoding)
        xml2::as_list(node)
      }, error = function(e) {
        warning("Erreur parse fragment à la ligne ", lineno, ": ", conditionMessage(e))
        NULL
      })
      if (!is.null(parsed)) {
        written <- written + 1L
        out_list[[written]] <- parsed
      }
      in_fragment <- FALSE
      buf <- character(0)
      if (!is.null(report_every) && written > 0 && written %% report_every == 0) {
        message("Fragments parsed: ", written, " (line ", lineno, ")")
      }
      # continue reading
    }
    if (length(buf) > max_lines_buffer) {
      close(con)
      stop("Fragment trop long (> max_lines_buffer). Fichier peut être mal formé.")
    }
  }
  message("Finished. Fragments parsed: ", written, " (lines read: ", lineno, ")")
  return(out_list)
}