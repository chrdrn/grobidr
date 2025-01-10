#' Convert XML files to a tibble
#'
#' This function reads all XML files with a `.tei.xml` extension from a specified
#' directory, extracts the title, DOI, and body text from each file, and combines
#' the results into a tibble.
#'
#' @param input_dir A character string specifying the directory containing the XML files.
#' @return A tibble with columns `title`, `doi`, and `body`, where each row corresponds
#' to an XML file.
#' @importFrom here here
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text
#' @importFrom tibble tibble
#' @export
xml_to_tibble <- function(input_dir) {
    if (file.info(input_dir)$isdir) {
        files <- list.files(here::here(input_dir), pattern = "\\.tei\\.xml$", full.names = TRUE)
    } else {
        files <- list(here::here(input_dir))
    }

    docs <- lapply(files, xml2::read_xml)

    ns <- c(tei = "http://www.tei-c.org/ns/1.0")

    results <- lapply(docs, function(doc) {
        title <- xml2::xml_text(xml2::xml_find_first(doc, "//tei:titleStmt/tei:title[@type='main']", ns))
        doi <- xml2::xml_text(xml2::xml_find_first(doc, "//tei:idno[@type='DOI']", ns))
        body_nodes <- xml2::xml_find_all(doc, "//tei:text/tei:body//tei:div/tei:p", ns)
        body_text <- paste(xml2::xml_text(body_nodes), collapse = " ")

        tibble::tibble(
            title = title,
            doi = doi,
            body = body_text
        )
    })

    do.call(rbind, results)
}
