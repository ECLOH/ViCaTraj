# xml_to_parquet_via_python.R  -- version corrigée (remplacez l'ancienne définition)
xml_to_parquet_via_python <- function(xml_file,
                                      out_dir,
                                      python_script,
                                      record_tag = "InfosFoyerRSA",
                                      batch_size = 3000,
                                      python_cmd = NULL) {
  stopifnot(file.exists(xml_file))
  stopifnot(file.exists(python_script))

  # find python on PATH if not provided
  if (is.null(python_cmd) || !nzchar(python_cmd)) {
    python_cmd <- Sys.which("python")
    if (!nzchar(python_cmd)) python_cmd <- Sys.which("python3")
    if (!nzchar(python_cmd)) python_cmd <- Sys.which("py")
    if (!nzchar(python_cmd)) stop("Aucun exécutable Python trouvé sur le PATH. Fournissez python_cmd si nécessaire.")
  }

  # handle Windows 'py' wrapper which often requires '-3' to select python3
  extra_arg <- NULL
  base_python <- basename(normalizePath(python_cmd, mustWork = FALSE))
  if (tolower(base_python) %in% c("py", "py.exe")) extra_arg <- "-3"

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # build argument vector (unquoted for processx)
  args_raw <- c(if (!is.null(extra_arg)) extra_arg, python_script, xml_file, out_dir, record_tag, as.character(batch_size))

  if (requireNamespace("processx", quietly = TRUE)) {
    # processx::run handles quoting itself; pass raw args
    px <- processx::run(command = python_cmd, args = args_raw, echo = TRUE, error_on_status = FALSE)
    if (px$status != 0) {
      stop("Erreur python (processx):\n", px$stderr)
    }
  } else {
    # fallback to system2: quote each arg
    args_quoted <- shQuote(args_raw)
    res <- system2(command = python_cmd, args = args_quoted, stdout = TRUE, stderr = TRUE)
    # you may inspect 'res' for debug, but we'll check produced parts below
  }

  # verify output parts exist
  parts <- list.files(out_dir, pattern = "^part-.*\\.parquet$", full.names = TRUE)
  if (length(parts) == 0) {
    stop("Conversion Python terminée sans produire de part-*.parquet. Vérifiez le script python, le chemin et les dépendances (lxml, pyarrow).")
  }

  invisible(out_dir)
}