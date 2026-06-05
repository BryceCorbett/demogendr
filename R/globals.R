# Declare the bare column names used inside data.table's non-standard
# evaluation (in top_transition_countries()) so that R CMD check does not
# raise "no visible binding for global variable" notes.
utils::globalVariables(c(
  "year", "country_name", "is_democracy", ".prev",
  "to_democracy", "from_democracy",
  "transitions_in", "transitions_out", "total_transitions"
))
