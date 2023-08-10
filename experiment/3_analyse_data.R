# compute cor matrix for fo subscales
psych::corr.test(data[, grepl("fo_", names(data))])

# compute descriptives for fo subscales
psych::describe(data[, grepl("fo_", names(data))])
