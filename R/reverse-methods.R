### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "reverse" methods.
###

setMethod("reverse", "XVector", function(x, ...) xvcopy(x, reverse=TRUE))

setMethod("rev", "XVector", function(x) reverse(x))

setMethod("reverse", "XVectorList", function(x, ...) xvcopy(x, reverse=TRUE))

