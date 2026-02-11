
#' @title \link[utils]{bibentry} for \CRANpkg{MASS}
#' 
#' @param key,... \link[utils]{bibentry}
#' 
#' @keywords internal
#' @name MASS_bib
#' @importFrom utils bibentry person
#' @export
.akaike74 <- \(key = 'Akaike74', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = 'Hirotugu', family = 'Akaike'),
    journal = 'IEEE Transactions on Automatic Control', 
    title = 'A new look at the statistical model identification', 
    year = '1974',
    volume = '19',
    number = '6',
    pages = '716--723',
    doi = '10.1109/TAC.1974.1100705'
  )
}