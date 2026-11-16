#' Convertir un fichier XML en Parquet (wrapper Python)
#'
#' Lance le script Python fourni dans inst/python pour convertir un fichier XML en
#' un dossier Parquet \"wide\" (parts). Le script Python est fourni dans le paquet.
#'
#' @param xml_file Chemin vers le fichier XML à convertir.
#' @param out_dir Dossier de sortie pour les fichiers part-*.parquet (créé s'il n'existe pas).
#' @param record_tag Le nom du noeud racine à extraire (par défaut \"InfosFoyerRSA\").
#' @param batch_size Taille de batch en nombre d'enregistrements (passée au script Python).
#' @param python_cmd (optionnel) chemin vers l'exécutable Python; si NULL on utilise Sys.which().
#' @return chemin vers le dossier contenant les fichiers Parquet (invisible), ou lève une erreur.
#' @examples
#' \dontrun{
#' path_py <- system.file("python", "xml_to_parquet_wide_direct.py", package = "monpkg")
#' xml_to_parquet_via_python("data/gros.xml", tempfile("out_parquet_"), python_script = path_py)
#' }
#' @export
xml_to_parquet_via_python <- function(xml_file, out_dir, record_tag = "InfosFoyerRSA",
                                      batch_size = 3000, python_cmd = NULL) {
  stopifnot(file.exists(xml_file))
  # get script packaged in inst/python
  py_script <- system.file("python", "xml_to_parquet_wide_direct.py", package = "monpkg")
  if (py_script == "") stop("Script Python introuvable dans le paquet. Assurez-vous qu'il est placé dans inst/python.")
  if (is.null(python_cmd)) {
    # prefer "python" then "python3" then "py"
    python_cmd <- Sys.which("python")
    if (!nzchar(python_cmd)) python_cmd <- Sys.which("python3")
    if (!nzchar(python_cmd)) python_cmd <- Sys.which("py")
    if (!nzchar(python_cmd)) stop("Python non trouvé sur le PATH. Installez Python ou fournissez 'python_cmd'.")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # build raw args WITHOUT shQuote when using processx (processx handles quoting)
  args_raw <- c(py_script, xml_file, out_dir, record_tag, as.character(batch_size))

  if (requireNamespace("processx", quietly = TRUE)) {
    # processx::run wants unquoted args as a character vector
    px <- processx::run(command = python_cmd, args = args_raw, echo = TRUE, error_on_status = FALSE)
    if (px$status != 0) stop("Erreur python: ", px$stderr)
  } else {
    # fallback to system2: use shQuote to protect args with spaces on Windows/Unix
    args_quoted <- shQuote(args_raw)
    res <- system2(command = python_cmd, args = args_quoted, stdout = TRUE, stderr = TRUE)
    # note: system2 returns a character vector with output; we check parts below
  }
  parts <- list.files(out_dir, pattern = "^part-.*\\.parquet$", full.names = TRUE)
  if (length(parts) == 0) stop("Conversion Python terminée sans produire de part-*.parquet.")
  invisible(out_dir)
}