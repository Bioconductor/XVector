### =========================================================================
### Some low-level (not exported) I/O utility functions
### -------------------------------------------------------------------------
###
### Unless stated otherwise, nothing in this file is exported.
###


new_input_filexp <- function(filepath)
{
    .Call2("new_input_filexp", filepath, PACKAGE="XVector")
}

new_output_filexp <- function(filepath, append,
                                       compress, compression_level)
{
    .Call2("new_output_filexp", filepath, append,
                                compress, compression_level,
                                PACKAGE="XVector")
}

finalize_filexp <- function(filexp)
{
    .Call2("finalize_filexp", filexp, PACKAGE="XVector")
}

