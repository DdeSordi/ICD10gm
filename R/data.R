#' data.frame containing metadata for all ICD-10-GM codes
#'
#' DIMDI provide a CSV file with metadata on all valid codes. This table is
#' read in with only minor modifications to facilitate changes between versions.
#'
#' This metadata is not suitable for operative coding and does not include all
#' relevant information concerning the codes. For example, the file contains
#' neither the inclusion and exclusion notes nor the detailed definitions
#' (where present, mainly in Chapter V). DIMDI provide additional reference
#' material for operative coding and detailed research.
#'
#' The block U00-U49 contains reserved codes that can be allocated quickly for
#' the documentation of new diseases or epidemiological phenomena. Such usage
#' is allowed only when mandated by DIMDI. In particular, the codes may not be
#' utilised on the initiative of other parties, for example, for clinical trials
#' or contractual purposes. Notable uses of the reserved codes are for the Zika
#' and COVID-19 viruses. These are included in the DIMDI online documentation,
#' but not in the download files. They are therefore added manually to this
#' data set as documented in the [package source](https://github.com/edonnachie/ICD10gm/blob/master/data-raw/additions/icd_meta_codes_additions.json).
#'
#' Die Schlüsselnummern U05.0-U05.9 dieser Kategorie sollen ein schnelles Reagieren auf aktuelle epidemiologische Phänomene ermöglichen. Sie dürfen nur zusätzlich benutzt werden, um einen anderenorts klassifizierten Zustand besonders zu kennzeichnen. Die Schlüsselnummern dieser Kategorie dürfen nur über das Deutsche Institut für Medizinische Dokumentation und Information (DIMDI) mit Inhalten belegt werden; eine Anwendung für andere Zwecke ist nicht erlaubt. DIMDI wird den Anwendungszeitraum solcher Schlüsselnummern bei Bedarf bekannt geben.
#'
#' @format A data.frame containing the following variables:
#' \describe{
#'   \item{year}{Year of validity (from 2004)}
#'   \item{level}{Level of the hierarchy (3, 4 or 5 digits)}
#'   \item{terminal}{Whether the code is a terminal code (i.e. with no further subcodes) (T: yes; N: no)}
#'   \item{subcode_type}{Whether the subcode is pre- or postcombinated (X: precombinated; S: postcombinated). Precombinated codes are listed directly under the three-digit ICD code, whereas postcombinated codes are lists of possible values for the fourth and fifth digits that are not specific to the particular code (e.g. the group E10-E14 shares a common list of postcombinated fourth and fifth digits)}
#'   \item{chapter_nr}{Chapter number (arabic digits 1-22)}
#'   \item{icd_block_first}{First code in the respective ICD block, can be used to join with the table ICD10gm::icd_meta_blocks}
#'   \item{icd_code}{Full icd code (up to 7 characters) with all symbols except the "dagger" (for aetiological codes that can be combined with an "asterisk" code to denote the manifestation)}
#'   \item{icd_normcode}{The ICD "normcode", consisting of up to 6 characters and without all symbols except the period (e.g. E11.30)}
#'   \item{icd_sub}{Complete ICD code without any symbols or punctuation, consisting of up to 5 characters  (e.g. E1130)}
#'   \item{label}{ICD label for the complete code.}
#'   \item{label_icd3}{ICD label for the three-digit ICD code.}
#'   \item{label_icd4}{ICD label for fourth digit of the ICD code.}
#'   \item{label_icd5}{ICD label for the fifth digit of the ICD code.}
#'   \item{usage_295}{Usage of the code in the ambulatory sector (Paragraph 295 SGB V) (P: primary code; O: only as a "star" code in conjunction with a "dagger" code for aetiology; Z: only an optional "!" code in conjunction with a primary code; V: not to be used for coding)}
#'   \item{usage_301}{Usage of the code in the stationary (hospital) sector (Paragraph 301 SGB V) (P: primary code; O: only as a "star" code in conjunction with a "dagger" code for aetiology; Z: only an optional "!" code in conjunction with a primary code; V: not to be used for coding)}
#'   \item{mort_list1}{Key to join with the WHO mortality list 1}
#'   \item{mort_list2}{Key to join with the WHO mortality list 2}
#'   \item{mort_list3}{Key to join with the WHO mortality list 3}
#'   \item{mort_list4}{Key to join with the WHO mortality list 4}
#'   \item{morb_list}{Key to join with the WHO morbidity list}
#'   \item{gender_specific}{Whether the diagnosis is gender specific (M: male; W: female; 9: Not gender specific)}
#'   \item{gender_error_type}{Type of error implied by the field \code{gender_specific} (9: irrelevant; K: possible error)}
#'   \item{age_min}{Minimum age for which the diagnosis is plausible (T001: from one day; Y005: from five years)}
#'   \item{age_max}{Maximum age for which the diagnosis is plausible (T010: up to 10 days; Y005: up to five years)}
#'   \item{age_error_type}{Type of error resulting from implausible age (9: irrelevant; M: always an error ("Muss-Fehler"); K: possible error ("Kann-Fehler"))}
#'   \item{rare_in_central_europe}{Indicates whether the diagnosis is rare in Central Europe (J: yes; N: no)}
#'   \item{code_with_content}{Indicates whether the code has content associated with it (J: yes; N: no, leads to an error)}
#'   \item{notifiable}{Indicates whether the diagnosis is notifiable in Germany (J: yes; N: no)}
#'   \item{notifiable_lab}{Indicates whether the diagnosis is notifiable for laboratories in Germany (J: yes; N: no)}
#' }
#'
#' @family ICD-10-GM metadata
#' @source The source data was downloaded from the official download centre
#' of the German Institute for Medical Documentation and Information (DIMDI).
#' See also \url{https://www.dimdi.de/dynamic/en/classifications/icd/icd-10-gm/tabular-list/#metadata}
"icd_meta_codes"

#' data.frame containing metadata for the ICD-10-GM code blocks
#'
#' The ICD blocks (German: "Gruppen") constitute a level in the hierarchy
#' between the chapters and the three-digit codes. The three-digit code are
#' grouped in sequence to form 240 groups that represent similar aetiological
#' diagnoses. Unlike some other grouper systems, the ICD blocks do not consider
#' similar diagnoses from different chapters of the ICD classification, for
#' example chronic pain coded as a unspecific symptom (R52.1) and as a
#' somatoform disorder (F45.4).
#'
#' \describe{
#'   \item{year}{Year of validity (from 2004)}
#'   \item{icd_block_first}{First three-digit ICD code in the block}
#'   \item{icd_block_last}{Last three-digit ICD code in the block}
#'   \item{chapter}{ICD-10 chapter to which the block belongs}
#'   \item{block_label}{Label for the block}
#'   \item{block_id}{Short label for the block in format "A00-A09"}
#' }
#'
#' @family ICD-10-GM metadata
#' @source The source data was downloaded from the official download centre
#' of the German Institute for Medical Documentation and Information (DIMDI).
#' See also \url{https://www.dimdi.de/dynamic/en/classifications/icd/icd-10-gm/tabular-list/structure/}
"icd_meta_blocks"


#' data.frame containing metadata for the ICD-10-GM chapters
#'
#' The ICD chapters group codes according to their aetiology.
#'
#' \describe{
#'   \item{year}{Year of validity (from 2004)}
#'   \item{chapter}{Chapter number (arabic numerals)}
#'   \item{chapter_roman}{Chapter number (Roman numerals)}
#'   \item{chapter_label}{Label for the chapter}
#' }
#'
#' @family ICD-10-GM metadata
#' @source The source data was downloaded from the official download centre
#' of the German Institute for Medical Documentation and Information (DIMDI).
#' See also \url{https://www.dimdi.de/dynamic/en/classifications/icd/icd-10-gm/tabular-list/structure/}
"icd_meta_chapters"

#' data.frame detailling the changes in ICD-10-GM codes between versions
#'
#' A data.frame providing old and new ICD codes
#' (identical if no changes)
#' and information as to whether the transition is automatic when
#' transitioning forwards or backwards
#'
#' \describe{
#'   \item{year_from}{Year of validity of the old code (from 2004)}
#'   \item{year_to}{Year of validity of the new code (from 2005)}
#'   \item{icd_from}{Old ICD code}
#'   \item{icd_to}{New ICD code}
#'   \item{automatic_forward}{Whether the transition is automatic in the forward direction (i.e. the old code can always be converted to the new code). (A: automatic, otherwise NA)}
#'   \item{automatic_backward}{Whether the transition is automatic in the forward direction (i.e. the new code can always be converted to the old code) (A: automatic, otherwise NA)}
#'   \item{change_5}{Whether the change relates to the fifth digit of the ICD-10 code (TRUE/FALSE).}
#'   \item{change_4}{Whether the change relates to the fourth digit of the ICD-10 code (TRUE/FALSE).}
#'   \item{change_3}{Whether the change relates to the three-digit ICD-10 code (TRUE/FALSE).}
#'   \item{icd3}{The first three digits of `icd_from`.}
#'   \item{icd_chapter}{The first character of `icd_from` (i.e. the letter denoting the chapter).}
#' }
#
#' @family ICD-10-GM metadata
#' @source The source data was downloaded from the official download centre
#' of the German Institute for Medical Documentation and Information (DIMDI).
#' See also \url{https://www.dimdi.de/dynamic/en/classifications/icd/icd-10-gm/tabular-list/#crosswalks}
"icd_meta_transition"


#' Get or query ICD-10 labels
#'
#' A utility function to get or query [icd_meta_codes],
#' returning a limited selection of ICD-10 codes and labels.
#'
#' If an ICD code is provided as argument `icd3`, all
#' corresponding codes and subcodes are returned. If a search
#' term is provided, all codes are returned whose label matches
#' the string approximately.
#'
#' Returns a data frame with ICD metadata, consisting of
#' year, ICD code and label. Optional arguments allow selection of
#' entries by year, code or label. This is beneficial because the
#' entire history is relatively large and rarely required in full.
#'
#' @param year Year or years to get (numeric or character vector)
#' @param icd3 A character vector of three-digit ICD-10 codes to select
#' @param search (optional) A string to search for in the label column using fuzzy matching (agrep)
#' @param ... (optional) Further arguments passed to agrep when searching with icd_label
#' @return data.frame(year, icd3, icd_code, icd_normcode, icd_sub, label), see icd_labels
#' @examples
#' get_icd_labels(year = 2019, icd3 = "I25")
#' get_icd_labels(year = 2019, search = "Asthma")
#' @export
get_icd_labels <- function (year = NULL, icd3 = NULL, icd_code = NULL, icd_normcode = NULL, icd_sub=NULL, search = NULL,
                        outvars=c("year", "icd3","icd_code", "icd_normcode", "icd_sub","label","chapter_nr","level","terminal","icd_block_first"), ...) 
  # It is like get_icd_labels() but with the option for more different input and output
{
  out <- ICD10gm::icd_meta_codes[, outvars]
  if (!is.null(year) & all(grepl("^\\d{4}$", year))) 
    out <- out[out$year %in% year, ]
  if (!is.null(icd3) & all(grepl("^[A-Za-z]\\d{2}", icd3))) 
    out <- out[out$icd3 %in% icd3, ]
  if (!is.null(icd_code) & all(grepl("^[A-Za-z]\\d{2}", icd_code))) 
    out <- out[out$icd_code %in% icd_code, ]  
  if (!is.null(icd_normcode) & all(grepl("^[A-Za-z]\\d{2}", icd_normcode))) 
    out <- out[out$icd_normcode %in% icd_normcode, ]
  if (!is.null(icd_sub) & all(grepl("^[A-Za-z]\\d{2}", icd_sub))) 
    out <- out[out$icd_sub %in% icd_sub, ] 
  if (!is.null(search) & is.character(search)) 
    out <- out[agrep(search, out$label, ...), ]
  return(out)
}

#' Get ICD history metadata
#'
#' A utility function to query the [icd_meta_transition] table.
#'
#' Returns a data frame with ICD transition history, consisting of
#' year, ICD code and label. Optional arguments allow selection of
#' entries by year or ICD code. This is beneficial because the
#' entire history is relatively large and rarely required in full.
#'
#' @param years Year or years to get (numeric or character vector)
#' @param icd3 (optional) ICD codes to select (regular expression, matched exactly using grep)
#' @return data.frame, see icd_hist
#' @examples
#' get_icd_history(years = 2009:2010, icd3 = "K52")
#' @export
get_icd_history <- function(years = NULL, icd3 = NULL){
  out <- ICD10gm::icd_meta_transition

  if(!is.null(years) & all(grepl("^\\d{4}$", years)))
    out <- out[out$year_from %in% years, ]

  if(!is.null(icd3) & all(grepl("^[A-Za-z]\\d{2}", icd3)))
    out <- out[grepl(icd3, out$icd_from) | grepl(icd3, out$icd_to), ]

   return(out)
}


#' Charlson Comorbidities (Royal College of Surgeons)
#'
#' Specification of the Charlson comorbidity index
#' in the version of the Royal College of Surgeons (2010).
#'
#' The specification can be expanded using the [icd_expand]
#' function to return all corresponding ICD-10-GM codes.
#'
#' This table was created on the basis of the publication
#' referenced below. It is provided as is with no guarantee
#' of accuracy. Furthermore, the applicability of the codes
#' in the context of the German ICD-10-GM is unclear.
#'
#' \describe{
#'   \item{Condition}{Disease entity}
#'   \item{ICD_SPEC}{Secification of the corresponding ICD-10 codes, suitable for input to [icd_expand]}
#'   }
#' @family Charlson
#' @source \doi{10.1002/bjs.6930}
"charlson_rcs"

#' Charlson Comorbidities (Sundararajan)
#'
#' Specification of the Charlson comorbidity index
#' in the version of Sunhararahan et al. (2004).
#'
#' The specification can be expanded using the [icd_expand]
#' function to return all corresponding ICD-10-GM codes.
#'
#' This table was created on the basis of the publication
#' referenced below. It is provided as is with no guarantee
#' of accuracy. Furthermore, the applicability of the codes
#' in the context of the German ICD-10-GM is unclear.
#'
#' \describe{
#'   \item{Condition}{Disease entity}
#'   \item{Weight}{Controbition of the disease entity towords the combined comorbidity index}
#'   \item{ICD_10_AM}{Secification of the corresponding ICD-10 codes, suitable for input to [icd_expand]}
#'   }
#' @family Charlson
#' @source \doi{10.1016/j.jclinepi.2004.03.012}
"charlson_sundararajan"


  # This function should give back a line per Code with all Infos about chapter and subchapter and labels of an ICD-Code
  # It was created to label a big dataset where only ICD-Codes where availible
  # With the additional labels the ICD-Codes can be displayed better in different levels, this helbs for tables and graphics
  # This function can be slickend a lot i think but it works for me.

icd_label <- function(Year=NULL , ICD_normcode=NULL ){
if(!all(na.omit(as.numeric(Year)) %in% min(ICD10gm::icd_meta_chapters$year):max(ICD10gm::icd_meta_chapters$year))) stop("At least one year is not in the range of 2004 to 2022")
if(!all(is_icd_code(na.omit(ICD_normcode)))){stop("ICD_normcode contains at least one string that is not an ICD-Code \n use is_icd_code() to check the variable")}

for (k in 1:length(Year)){
if(!is_icd_code(ICD_normcode[k], year = Year[k])) stop(paste("Normcode",ICD_normcode[k],"in row",k,"does not exist or at least not in the year",Year[k],".\n Please use is_icd_code() to check."))
icd_lab <- get_icd_labels(icd_normcode =  ICD_normcode[k], year = Year[k])
icd_lab$row <- as.numeric(row.names(icd_lab))

# chapter label (level1)
icd_lab <- merge(icd_lab,icd_meta_chapters[icd_meta_chapters$year== Year[k],c("chapter","chapter_label")],by.x="chapter_nr",by.y = "chapter")
# icd_block_first (level2)
icd_lab <-merge(icd_lab,icd_meta_blocks[icd_meta_blocks$year== Year[k],c("icd_block_first","block_label")],by="icd_block_first")
icd_lab <- icd_lab[,c("row","year",
                      "icd3",
                      "icd_code",
                      "icd_normcode",
                      "icd_sub",
                      "label",
                      "level",
                      "terminal",
                      "chapter_nr",
                      "chapter_label",
                      "icd_block_first",
                      "block_label")]
                   
out_e <- data.frame(matrix("",ncol = 15, nrow = 1))
names(out_e) <- c("lvl_3_icd_code",
                 "lvl_3_icd_normcode",
                 "lvl_3_icd_sub",
                 "lvl_3_label",
                 "lvl_3_terminal",
                 "lvl_4_icd_code",
                 "lvl_4_icd_normcode",
                 "lvl_4_icd_sub",
                 "lvl_4_label",
                 "lvl_4_terminal",
                 "lvl_5_icd_code",
                 "lvl_5_icd_normcode",
                 "lvl_5_icd_sub",
                 "lvl_5_label",
                 "lvl_5_terminal")
icd_lab <- cbind(icd_lab,out_e)

if (icd_lab$level == 3){
  icd_lab[,stringr::str_detect(names(icd_lab),"lvl_3")] <- icd_lab[,c("icd_code", "icd_normcode", "icd_sub", 
                                                             "label","terminal")]
}

l3 <- l4 <- icd_lab[,c("icd_code", "icd_normcode", "icd_sub", "label","terminal", "level")]
i=j=0
if (icd_lab$level >= 4){
  while( l3$level >= 4){
    l3 <- icd_meta_codes[icd_lab$row-i,c("icd_code", "icd_normcode", "icd_sub", 
                                                            "label","terminal", "level")]
    i = i+1
  }
  icd_lab[,stringr::str_detect(names(icd_lab),"lvl_3")] <- l3[,c("icd_code", "icd_normcode", "icd_sub", 
                                                        "label","terminal")]
  icd_lab[,stringr::str_detect(names(icd_lab),"lvl_4")] <- icd_lab[,c("icd_code", "icd_normcode", "icd_sub", 
                                                             "label","terminal")]
}
if (icd_lab$level == 5){
while( l4$level >= 5){
  l4 <-  icd_meta_codes[icd_lab$row-j,c("icd_code", "icd_normcode", "icd_sub", 
                                                        "label","terminal", "level")]
    j = j+1
}
  icd_lab[,stringr::str_detect(names(icd_lab),"lvl_4")] <- l4[,c("icd_code", "icd_normcode", "icd_sub", 
                                                        "label","terminal")]
  icd_lab[,stringr::str_detect(names(icd_lab),"lvl_5")] <- icd_lab[,c("icd_code", "icd_normcode", "icd_sub", 
                                                             "label","terminal")]
}
ifelse(k>1, icd_out <- rbind(icd_out, icd_lab), icd_out <- icd_lab)
icd_out
}
return(icd_out)
}

# a <- data.frame(matrix(
#   c("2019", "E66.80","2019","E66.00", "2020", "E66.12","2017","E66.19","2020","E66.24"),ncol=2, byrow=T  ))
# 
# a
# 
# icds <- icd_label(
#   Year=a[,1]
#   ,
#   ICD_normcode =  a[,2]
# )
# 
# View(t(icds))
