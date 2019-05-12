library(test.nest)

test_lintr()
# test_strict was commented out
# TODO: figure out why it throws simpleWarning in
# model.matrix.default(Terms, mf, contrasts = contrast.arg):
# partial argument match of 'contrasts' to 'contrasts.arg'
test_regexp()
