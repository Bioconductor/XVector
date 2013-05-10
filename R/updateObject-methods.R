### ###################################################################
### Update methods
### ###################################################################


## "XVector" -> "XVector"
setMethod("updateObject", signature(object="XVector"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XVector')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new(class(object),
                        shared = slot(object, "shared"),
                        offset = slot(object, "offset"),
                        length = slot(object, "length"))
              }
              object
          })

## "XIntegerViews" -> "XIntegerViews"
setMethod("updateObject", signature(object="XIntegerViews"),
          function(object, ..., verbose=FALSE) {
              if (verbose) message("updateObject(object = 'XIntegerViews')")
              if (!("metadata" %in% names(attributes(object)))) {
                  object <-
                    new("XIntegerViews",
                        subject = updateObject(slot(object, "subject")),
                        start = slot(object, "start"),
                        width = slot(object, "width"),
                        NAMES = slot(object, "NAMES"))
              }
              object
          })

