#' Convert to a fullpage.js presentation
#' 
#' Format for converting from R Markdown to a fullpage.js presentation.
#' @inheritParams rmarkdown::html_document
#' @export
fullpagejs_slide <- function(fig_width = 8,
                             fig_height = 6,
                             fig_retina = if(!fig_caption) 2,
                             fig_caption = FALSE,
                             smart = TRUE,
                             self_contained = TRUE,
                             highlight = "default",
                             mathjax = "default",
                             template = "default",
                             css = NULL,
                             includes = NULL,
                             keep_md = FALSE,
                             lib_dir = NULL,
                             pandoc_args = NULL,
                             extra_dependencies = NULL,
                             fullpagejs_options = NULL,
                             ...) {
  
  # function to lookup fullpage resource
  fullpagejs_path <- function(){
    system.file("rmarkdown/templates/fullpagejs_slide/resources",
                package = 'fullpagejs')
  }
  
  # base pandoc options for all fullpage.js output
  args <- c()
  
  # template path and assets
  if(identical(template, "default")) {
    default_template <- system.file(
      "rmarkdown/templates/fullpagejs_slide/resources/default.html",
      package = "fullpagejs"
    )
    args <- c(args, "--template", pandoc_path_arg(default_template))
  } else if(!is.null(template)) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }
  
  # set "--section-divs"
  args <- c(args, "--section-divs")
  
  # content includes
  args <- c(args, includes_to_pandoc_args(includes))
  
  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css))
  
  # pre-processor
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {
    # not work shiny
    if (identical(runtime, "shiny")) {
      stop("revealjs_presentation is not compatible with runtime 'shiny'", 
           call. = FALSE)
    }
    
    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir
    
    # extra_args
    args <- c()
    
    # fullpage.js
    fullpagejs_path <- system.file("fullPage.js-2.9.4", package = "fullpagejs")
    if (!self_contained || identical(.Platform$OS.type, "windows"))
      fullpagejs_path <- relative_to(
        output_dir, render_supporting_files(fullpagejs_path, lib_dir))
    else
      fullpagejs_path <- pandoc_path_arg(fullpagejs_path)
    args <- c(args, "--variable", paste0("fullpagejs-path=", fullpagejs_path))
    
    # highlight
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))
    
    # fullPage options
    args <- c(args, "--variable", paste0("fullpagejs-options=", fullpagejs_options))
    
    # return additional args
    args
  }
  
  # post-processor
  post_processor <- function(metadata, input_file, output_file, clean,
                             verbose) {
    # get lines
    lines <- readLines(output_file)
    
    # change classes in h2
    # lines <- sub(
    #   "(<div .*)class=\"section level2\"",
    #   "\\1class=\"slide level2\"",
    #   lines,
    #   perl = TRUE
    # )
    
    # change classes in h2-h6
    lines <- sub(
      "(<div .*)class=\"section (level[2-6])",
      "\\1class=\"\\2",
      lines,
      perl = TRUE
    )
    
    writeLines(lines, output_file)
    output_file
  }
  
  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md),
    pandoc = pandoc_options(to = "html",
                            from = rmarkdown_format(ifelse(fig_caption,
                                                           "",
                                                           "-implicit_figures")),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = html_document_base(smart = smart, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     ...))
}