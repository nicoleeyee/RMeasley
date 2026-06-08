#' Analyze whether measles outcomes differ by region after accounting for GDP
#'
#' Fit two linear models and use ANOVA to test whether adding region explains
#' additional variation in epidemiologically linked measles cases after
#' accounting for GDP.
#'
#' @param measles_data A data frame of cleaned measles case and GDP data.
#' @param start_year First year to include. Defaults to 2012.
#' @param end_year Last year to include. Defaults to 2024.
#'
#' @returns A tibble with ANOVA results testing whether region explains additional variation after accounting for GDP.
#' @importFrom dplyr distinct filter mutate case_when
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#' @importFrom stats lm anova
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' region_gdp_anova_table(load_data())
#' region_gdp_anova_table(load_data(), start_year = 2020, end_year = 2024)

region_gdp_anova_table <- function(measles_data, start_year = 2012, end_year = 2024) {

  epi_gdp_model_data <- measles_data |>
    dplyr::distinct(
      .data$country,
      .data$year,
      .data$region,
      .data$gdp_per_capita,
      .data$measles_epi_linked
    ) |>
    dplyr::filter(
      .data$year >= start_year,
      .data$year <= end_year,
      .data$gdp_per_capita > 0
    ) |>
    tidyr::drop_na(
      gdp_per_capita,
      measles_epi_linked,
      region
    ) |>
    dplyr::mutate(
      region = factor(.data$region),
      log_gdp = log10(.data$gdp_per_capita),
      log_epi_linked = log1p(.data$measles_epi_linked)
    )

  model_gdp_only <- stats::lm(
    log_epi_linked ~ log_gdp,
    data = epi_gdp_model_data
  )

  model_gdp_region <- stats::lm(
    log_epi_linked ~ log_gdp + region,
    data = epi_gdp_model_data
  )

  region_anova <- stats::anova(
    model_gdp_only,
    model_gdp_region
  )

  tibble::tibble(
    term = "Overall region effect",
    comparison = "GDP-only model vs. GDP + region model",
    df = region_anova$Df[2],
    sumsq = region_anova$`Sum of Sq`[2],
    rss = region_anova$RSS[2],
    f_statistic = region_anova$F[2],
    p_value = region_anova$`Pr(>F)`[2],
    conclusion = dplyr::case_when(
      .data$p_value < 0.05 ~ "At least one region differs",
      TRUE ~ "No evidence that regions differ overall"
    )
  )
}
