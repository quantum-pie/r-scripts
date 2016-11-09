# function for string reverse
# details:
# strsplit here returns character list of length = 1, where element 1 is character list of input string splits (see docs)
# we extract this first and the only element and then reversing list of splits, collapsing it afterwards into character list of length = 1, where element one is list of splits in reversed order
# finally we apply paste to this first ant the only element
reverse_string <- function(string) {
  paste(rev(strsplit(string, split = "")[[1]]), collapse = "")
}

# we pass optional arguments (ellipsis) directly to the paste inside of decorate_string
decorate_string <- function(pattern, ...) { 
  reversed_pattern <- reverse_string(pattern)
  paste(pattern, paste(...), reversed_pattern, sep = "")
}

# use example
decorated <- decorate_string("123", c("abs", "ad"), sep = ";")
