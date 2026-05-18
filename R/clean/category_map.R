#' Canonical visa-category mapping
#'
#' The modelling pipeline uses a single canonical taxonomy of 10 visa
#' categories (see `config.yml::categories.levels`). Each raw source uses
#' its own labelling: ABS OAD groups, ABS NOM groups (the published
#' breakdown is much coarser than DHA's), DHA visa subclass numbers, and
#' free-text in Department of Education tables.
#'
#' This function returns a list of regex-based mappings keyed by source.
#' Patterns are matched in declaration order and the first match wins, so
#' put narrower patterns first.
#'
#' Updating the map
#'
#' If you discover an unmapped raw label, add it here. Document
#' provenance in the methodology report — taxonomy choices materially
#' affect both the empirical π and the model.
#'
#' @return A named list. Top-level names are source identifiers
#'   (`oad`, `nom`, `visa_subclass`). Inside each, names are canonical
#'   categories and values are character vectors of regular expressions.
#' @export
nn_category_map <- function() {
  list(
    # ABS OAD `series` descriptions
    oad = list(
      student = c(
        "(?i)long.?term.*student",
        "(?i)temporary visa.*student",
        "(?i)subclass.*500|subclass.*570|subclass.*571|subclass.*572|subclass.*573|subclass.*574|subclass.*575|subclass.*576"
      ),
      skilled = c(
        "(?i)long.?term.*skilled",
        "(?i)long.?term.*employment",
        "(?i)temporary work \\(skilled\\)",
        "(?i)subclass.*482|subclass.*457|subclass.*186|subclass.*189|subclass.*190|subclass.*491"
      ),
      working_holiday = c(
        "(?i)working holiday",
        "(?i)subclass.*417|subclass.*462"
      ),
      family = c(
        "(?i)long.?term.*family",
        "(?i)partner.*visa",
        "(?i)subclass.*820|subclass.*100|subclass.*801|subclass.*309|subclass.*801"
      ),
      nz_citizen = c(
        "(?i)new zealand citizen",
        "(?i)subclass.*444"
      ),
      bridging = c(
        "(?i)bridging visa",
        "(?i)subclass.*010|subclass.*020|subclass.*030|subclass.*040|subclass.*041|subclass.*050|subclass.*051|subclass.*060|subclass.*070"
      ),
      other_temp = c(
        "(?i)other temporary",
        "(?i)temporary resident"
      ),
      permanent = c(
        "(?i)long.?term.*permanent",
        "(?i)permanent visa"
      ),
      returning_au = c(
        "(?i)australian citizen",
        "(?i)returning australian"
      ),
      other = c(
        "(?i)long.?term.*visitor",
        "(?i)other"
      )
    ),

    # ABS NOM published categories (coarser than OAD)
    nom = list(
      student          = c("(?i)student"),
      skilled          = c("(?i)skilled|employer.?sponsored"),
      working_holiday  = c("(?i)working holiday"),
      family           = c("(?i)family|partner"),
      nz_citizen       = c("(?i)new zealand"),
      bridging         = c("(?i)bridging"),
      other_temp       = c("(?i)other temporary"),
      permanent        = c("(?i)permanent"),
      returning_au     = c("(?i)australian citizen|returning australian"),
      total            = c("(?i)total|all visas|net overseas migration$")
    ),

    # DHA visa-subclass numbers / free-text labels
    visa_subclass = list(
      student = c(
        "^(500|570|571|572|573|574|575|576|580|590)\\b",
        "(?i)student"
      ),
      skilled = c(
        "^(186|187|189|190|191|482|457|494|491|858)\\b",
        "(?i)skilled|employer.?sponsored|global talent|business innovation"
      ),
      working_holiday = c(
        "^(417|462)\\b",
        "(?i)working holiday"
      ),
      family = c(
        "^(100|101|102|103|114|115|116|117|143|151|173|300|309|310|801|802|820|826|832|835|836|837|838|864|884)\\b",
        "(?i)family|partner|parent|child"
      ),
      nz_citizen = c(
        "^444\\b",
        "(?i)new zealand"
      ),
      bridging = c(
        "^(010|020|030|040|041|050|051|060|070)\\b",
        "(?i)bridging"
      ),
      permanent = c(
        "(?i)permanent"
      ),
      other_temp = c(
        "^(400|403|408|476|485|488|600|601|602|651|771|785|790|866)\\b",
        "(?i)temporary"
      ),
      returning_au = c(
        "(?i)australian citizen"
      ),
      other = c(
        ".*"   # catch-all; must be last
      )
    )
  )
}
