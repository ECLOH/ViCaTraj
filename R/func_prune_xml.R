prune_xml <- function(xml, chemins_a_conserver) {
  stopifnot(inherits(xml, "xml_document") ||
              inherits(xml, "xml_node"))

  # Copie indépendante de l'objet XML
  xml_filtre <- xml2::read_xml(
    as.character(xml)
  )

  racine <- xml2::xml_root(xml_filtre)
  nom_racine <- xml2::xml_name(racine)

  # Normalisation des chemins :
  # suppression du slash initial et du nom de racine
  chemins <- sub("^/", "", chemins_a_conserver)
  chemins <- sub(
    paste0("^", gsub("([][(){}.+*?$^|\\\\])", "\\\\\\1", nom_racine), "/?"),
    "",
    chemins
  )

  supprimer_enfants_non_utiles <- function(
    node,
    chemin_parent = ""
  ) {
    enfants <- xml2::xml_children(node)

    if (length(enfants) == 0L) {
      return(invisible(NULL))
    }

    for (enfant in enfants) {
      nom_enfant <- xml2::xml_name(enfant)

      chemin_enfant <- if (nzchar(chemin_parent)) {
        paste(chemin_parent, nom_enfant, sep = "/")
      } else {
        nom_enfant
      }

      # Le nœud est utile si :
      # - un chemin conservé commence par ce chemin ;
      # - ou si ce chemin conservé est déjà atteint.
      est_utile <- any(
        startsWith(chemins, chemin_enfant) |
          startsWith(chemin_enfant, chemins)
      )

      if (est_utile) {
        supprimer_enfants_non_utiles(
          node = enfant,
          chemin_parent = chemin_enfant
        )
      } else {
        xml2::xml_remove(enfant)
      }
    }

    invisible(NULL)
  }

  supprimer_enfants_non_utiles(
    node = racine,
    chemin_parent = nom_racine
  )

  xml_filtre
}
