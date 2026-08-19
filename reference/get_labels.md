# This function will return corresponding labels in a dataset.

This function will return corresponding labels in a dataset.

## Usage

``` r
get_labels(dataset, col_names = NULL, keep_as_original = FALSE)
```

## Arguments

- dataset:

  Dataset to get labels from.

- col_names:

  Vector of character names of the columns in the dataset.

- keep_as_original:

  Logical TRUE or FALSE: If TRUE, the corresponding column with no label
  will return column name, else return NA. If not assigned, it will
  return all labels of the the dataset. Otherwise, only labels of
  corresponding columns will be returned.

## Value

Vector containing the labels of the dataset
