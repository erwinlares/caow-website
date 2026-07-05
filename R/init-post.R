# init-post.R
# Scaffolding helpers for the Capital Aikikai of Wisconsin website.
# Source this file (or load via .Rprofile) to make the functions available.
#
# Functions
#   init_blog_post(slug, date = NULL, image = NULL, yaml_data = NULL)
#     -> blog/YYYY-MM-DD-slug/index.qmd
#   init_blog_post_with_code(slug, date = NULL, image = NULL, yaml_data = NULL)
#     -> blog/YYYY-MM-DD-slug/index.qmd  (+ execute block)
#   init_event_post(slug, date = NULL, event_date = NULL, image = NULL, yaml_data = NULL)
#     -> events/YYYY-MM-DD-slug/index.qmd
#   init_event_post_with_code(slug, date = NULL, event_date = NULL, image = NULL, yaml_data = NULL)
#     -> events/YYYY-MM-DD-slug/index.qmd  (+ execute block)
#   set_thumbnail(section, slug, image = NULL)
#     -> generate/regenerate a post's thumbnail after the fact
#   retract_post(section, slug, permanent = FALSE)
#     -> unpublish (default) or permanently delete an existing post
#
# Notes on the date arguments
#   `date` is the publication date and controls both the front-matter
#   `date:` field and the YYYY-MM-DD prefix on the post's directory. If
#   omitted, it defaults to today, matching the original behavior.
#
#   `event_date` (events only) records when the seminar, gasshuku, or
#   open mat actually happens, independent of when the announcement was
#   posted. It's written to the front matter as `event-date:`. Quarto
#   listings sort by `date` by default, so surfacing event-date in the
#   events listing (sort order and/or a visible field) is a separate
#   change to that listing's config in _quarto.yml -- this script only
#   writes the field, it doesn't wire up the listing to use it.
#
# Two ways to set a thumbnail
#   1. At scaffold time: init_blog_post()/init_event_post() take an
#      `image` argument pointing at a photo that already exists
#      somewhere on disk. It gets copied into the new post directory,
#      and a cropped/resized "-thumb" derivative is generated and
#      wired into the `image:` front-matter field.
#   2. After the fact: if you scaffold the post first and drop a photo
#      into its folder by hand afterward (the more common workflow),
#      call set_thumbnail(section, slug) with no `image` argument --
#      it looks inside the post's own folder for a candidate photo and
#      generates the thumbnail from whatever it finds.
#   Either way, the original photo is never modified in place.
#
#   For in-body galleries (multiple photos within a post, browsable via
#   Quarto's native lightbox support), reference the photo directly in
#   the post's Markdown with a shared `group` attribute -- see
#   get-started.R for the syntax.


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.resolve_slug <- function(slug) {
    is_code  <- grepl("^`.*`$", slug)
    is_words <- grepl(" ", slug) && !is_code
    
    if (is_code) {
        raw      <- gsub("^`|`$", "", slug)
        dir_slug <- gsub(" ", "-", raw)
        title    <- paste0("`", raw, "`")
        
    } else if (is_words) {
        dir_slug <- gsub(" ", "-", tolower(slug))
        title    <- tools::toTitleCase(slug)
        
    } else {
        dir_slug <- slug
        title    <- tools::toTitleCase(gsub("-", " ", slug))
    }
    
    list(dir_slug = dir_slug, title = title)
}

# Validates and normalizes a date argument to "YYYY-MM-DD".
# NULL falls back to today. Anything unparseable raises an informative error.
.resolve_date <- function(date, label = "date") {
    if (is.null(date)) {
        return(format(Sys.Date(), "%Y-%m-%d"))
    }
    
    parsed <- tryCatch(as.Date(date), error = function(e) NA)
    
    if (is.na(parsed)) {
        stop(
            "Invalid ", label, ": '", date, "'. ",
            "Expected a string R can parse as a date, e.g. '2026-03-14'.",
            call. = FALSE
        )
    }
    
    format(parsed, "%Y-%m-%d")
}

.resolve_author <- function(yaml_data) {
    if (!is.null(yaml_data)) {
        yaml_path <- if (grepl("\\.ya?ml$", yaml_data)) yaml_data else paste0(yaml_data, ".yml")
        if (!file.exists(yaml_path)) {
            stop("yaml_data file not found: ", yaml_path, call. = FALSE)
        }
        author_data <- yaml::read_yaml(yaml_path)
        if (is.null(author_data$author)) {
            stop("yaml_data file must contain an 'author' key.", call. = FALSE)
        }
        # Pluck only `name` -- ignore all other fields in the yaml file
        authors <- lapply(author_data$author, function(a) list(name = a$name))
        yaml::as.yaml(list(author = authors))
    } else {
        'author:\n  - name: ""\n'
    }
}

# Finds the directory for `slug` within `section` ("blog" or "events"),
# fuzzy-matching against the <date>-<slug> naming convention. Shared by
# retract_post() and set_thumbnail() so the matching logic lives in
# exactly one place.
.find_post_dir <- function(section, slug) {
    if (!section %in% c("blog", "events")) {
        stop('section must be "blog" or "events".', call. = FALSE)
    }
    
    dir_slug <- .resolve_slug(slug)$dir_slug
    
    candidates <- list.dirs(section, full.names = TRUE, recursive = FALSE)
    matches <- candidates[grepl(paste0("-", dir_slug, "$"), basename(candidates))]
    
    if (length(matches) == 0) {
        stop("No post found matching slug '", slug, "' in ", section, "/.", call. = FALSE)
    }
    if (length(matches) > 1) {
        stop(
            "Slug '", slug, "' matches more than one post:\n",
            paste0("  - ", basename(matches), collapse = "\n"),
            "\nBe more specific.", call. = FALSE
        )
    }
    
    matches[1]
}

# Replaces the first line matching `key_pattern` in a post's front
# matter with `new_line`. Returns TRUE if a matching line was found and
# replaced, FALSE otherwise -- callers decide whether that's an error.
.replace_front_matter_line <- function(post_path, key_pattern, new_line) {
    lines <- readLines(post_path)
    line_idx <- grep(key_pattern, lines)
    
    if (length(line_idx) == 0) {
        return(FALSE)
    }
    
    lines[line_idx[1]] <- new_line
    writeLines(lines, post_path)
    TRUE
}

# Generates a resized, center-cropped "-thumb" derivative of
# `source_path` inside `dest_dir`. Does NOT touch or copy the original.
# Returns the thumbnail's filename (not a full path) -- what belongs in
# the front-matter `image:` field.
.generate_thumbnail <- function(source_path, dest_dir, thumb_width = 1000, thumb_height = 750,
                                mode = c("cover", "contain"), bg_color = "white") {
    mode <- match.arg(mode)
    
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop(
            "The magick package is required for thumbnail generation. ",
            "Install it with install.packages(\"magick\").",
            call. = FALSE
        )
    }
    if (!file.exists(source_path)) {
        stop("Image not found: ", source_path, call. = FALSE)
    }
    
    img <- magick::image_read(source_path)
    
    if (mode == "cover") {
        # "widthxheight^" resizes so the image *covers* the target box,
        # preserving aspect ratio and overflowing on one dimension; the
        # subsequent center crop trims that overflow. Right for
        # photographs, where losing a sliver off an edge is harmless.
        img <- magick::image_resize(img, paste0(thumb_width, "x", thumb_height, "^"))
        img <- magick::image_crop(img, paste0(thumb_width, "x", thumb_height), gravity = "center")
    } else {
        # "widthxheight" (no modifier) resizes so the image *fits within*
        # the target box -- nothing cropped -- and image_extent() pads
        # out to the exact canvas size. Right for flyers and posters,
        # where Quarto's own listing-grid crop-to-fill would otherwise
        # cut off text; landing at the same aspect ratio as "cover"
        # thumbnails means that later crop has nothing left to trim.
        img <- magick::image_resize(img, paste0(thumb_width, "x", thumb_height))
        img <- magick::image_extent(img, paste0(thumb_width, "x", thumb_height),
                                    gravity = "center", color = bg_color)
    }
    
    base_name  <- tools::file_path_sans_ext(basename(source_path))
    ext        <- tools::file_ext(source_path)
    thumb_name <- paste0(base_name, "-thumb.", ext)
    thumb_path <- file.path(dest_dir, thumb_name)
    
    magick::image_write(img, thumb_path)
    thumb_name
}

.build_template <- function(title, author_block, date, event_date, event_end, image,
                            category_lines, nav_partial, with_code) {
    event_date_line <- if (!is.null(event_date)) {
        paste0("event-date: ", event_date, "\n")
    } else {
        ""
    }
    
    event_end_line <- if (!is.null(event_end)) {
        paste0("event-end: ", event_end, "\n")
    } else {
        ""
    }
    
    code_block <- if (with_code) {
        paste0(
            "format:\n",
            "  html:\n",
            "    toc: true\n",
            "    toc-depth: 3\n",
            "    toc-title: Contents\n",
            "    number-sections: false\n",
            "    embed-resources: false\n",
            '    include-before-body: "', nav_partial, '"\n',
            "execute:\n",
            "  include: true\n",
            "  echo: true\n",
            "  message: false\n",
            "  error: false\n"
        )
    } else {
        paste0(
            "format:\n",
            "  html:\n",
            '    include-before-body: "', nav_partial, '"\n'
        )
    }
    
    paste0(
        "---\n",
        'title: "', title, '"\n',
        'subtitle: ""\n',
        'description: "One or two sentences."\n',
        'image: "', image, '"\n',
        author_block,
        "date: ", date, "\n",
        event_date_line,
        event_end_line,
        "date-modified: last-modified\n",
        "categories:\n",
        category_lines, "\n",
        "status: draft\n",
        code_block,
        "---\n",
        "\n"
    )
}

.scaffold_post <- function(section, slug, date, event_date = NULL, event_end = NULL, image, yaml_data,
                           categories, with_code, thumb_width, thumb_height,
                           thumb_mode, bg_color) {
    parsed   <- .resolve_slug(slug)
    dir_slug <- parsed$dir_slug
    title    <- parsed$title
    date     <- .resolve_date(date, "date")
    
    event_date_resolved <- NULL
    event_end_resolved  <- NULL
    if (section == "events") {
        if (!is.null(event_date)) {
            event_date_resolved <- .resolve_date(event_date, "event_date")
            if (as.Date(event_date_resolved) < as.Date(date)) {
                message(
                    "Note: event_date (", event_date_resolved,
                    ") is earlier than the publication date (", date,
                    "). Double check this is intentional -- it would mean the ",
                    "announcement went up after the event took place."
                )
            }
        }
        
        if (!is.null(event_end)) {
            if (is.null(event_date)) {
                stop(
                    "event_end requires event_date to also be set -- a date ",
                    "range needs both endpoints.",
                    call. = FALSE
                )
            }
            event_end_resolved <- .resolve_date(event_end, "event_end")
            if (as.Date(event_end_resolved) < as.Date(event_date_resolved)) {
                message(
                    "Note: event_end (", event_end_resolved,
                    ") is earlier than event_date (", event_date_resolved,
                    "). Double check the range is the right way round."
                )
            }
        }
    }
    
    if (!is.null(image) && !file.exists(image)) {
        stop("image not found: ", image, call. = FALSE)
    }
    
    dir_path     <- file.path(section, paste0(date, "-", dir_slug))
    post_path    <- file.path(dir_path, "index.qmd")
    author_block <- .resolve_author(yaml_data)
    
    if (dir.exists(dir_path)) {
        stop("Directory already exists: ", dir_path, call. = FALSE)
    }
    
    dir.create(dir_path, recursive = TRUE)
    
    image_field <- ""
    if (!is.null(image)) {
        dest_original <- file.path(dir_path, basename(image))
        file.copy(image, dest_original, overwrite = FALSE)
        image_field <- .generate_thumbnail(dest_original, dir_path, thumb_width, thumb_height,
                                           mode = thumb_mode, bg_color = bg_color)
    }
    
    category_lines <- paste0("  - ", categories, collapse = "\n")
    
    # path is relative to the post's index.qmd, two levels deep
    nav_partial <- if (section == "blog") {
        "../../_partials/blog-nav.html"
    } else {
        "../../_partials/events-nav.html"
    }
    
    template <- .build_template(title, author_block, date, event_date_resolved, event_end_resolved,
                                image_field, category_lines, nav_partial, with_code)
    
    writeLines(template, post_path)
    rstudioapi::navigateToFile(post_path)
    message("Created: ", post_path)
    if (!is.null(image)) {
        message("  + copied original: ", basename(image))
        message("  + generated thumbnail: ", image_field)
    }
    invisible(post_path)
}


# ---------------------------------------------------------------------------
# Public functions
# ---------------------------------------------------------------------------

#' Scaffold a minimal blog post (no code)
#'
#' Creates blog/YYYY-MM-DD-<slug>/index.qmd with a lean YAML front matter.
#' Format and execute options inherit from _quarto.yml.
#'
#' @param slug  Post identifier. Accepts three forms:
#'   - "word-slug"         -> dir: word-slug,       title: Word Slug
#'   - "natural language"  -> dir: natural-language, title: Natural Language
#'   - "`code term`"       -> dir: code-term,        title: `code term`
#' @param date  Publication date, e.g. "2026-03-14". Defaults to today.
#'   Also sets the YYYY-MM-DD prefix on the post's directory.
#' @param image  Optional path to a source photo that already exists
#'   somewhere on disk. Copied untouched into the post directory; a
#'   cropped/resized "-thumb" derivative is generated alongside it and
#'   used as the listing thumbnail. If you'd rather scaffold the post
#'   first and drop a photo into its folder afterward, leave this NULL
#'   and call set_thumbnail() once the photo is in place.
#' @param yaml_data Optional path to a .yml file containing an `author` key.
#'
#' @examples
#' init_blog_post("First Trip to Japan")
#' init_blog_post("summer-gasshuku-recap", date = "2025-08-02", image = "~/Photos/mat-work.jpg")
init_blog_post <- function(slug, date = NULL, image = NULL, yaml_data = NULL,
                           thumb_width = 1000, thumb_height = 750,
                           thumb_mode = "cover", bg_color = "white") {
    categories <- c("aikido", "announcements", "community", "personal")
    .scaffold_post(section = "blog", slug = slug, date = date, image = image,
                   yaml_data = yaml_data, categories = categories, with_code = FALSE,
                   thumb_width = thumb_width, thumb_height = thumb_height,
                   thumb_mode = thumb_mode, bg_color = bg_color)
}


#' Scaffold a blog post with R code support
#'
#' Like init_blog_post() but includes toc, number-sections, and execute
#' blocks for posts that embed R output (charts, tables, data summaries).
#'
#' @inheritParams init_blog_post
#'
#' @examples
#' init_blog_post_with_code("dojo-attendance-2026")
init_blog_post_with_code <- function(slug, date = NULL, image = NULL, yaml_data = NULL,
                                     thumb_width = 1000, thumb_height = 750,
                                     thumb_mode = "cover", bg_color = "white") {
    categories <- c("aikido", "announcements", "community", "personal")
    .scaffold_post(section = "blog", slug = slug, date = date, image = image,
                   yaml_data = yaml_data, categories = categories, with_code = TRUE,
                   thumb_width = thumb_width, thumb_height = thumb_height,
                   thumb_mode = thumb_mode, bg_color = bg_color)
}


#' Scaffold a minimal event post (no code)
#'
#' Creates events/YYYY-MM-DD-<slug>/index.qmd with a lean YAML front matter.
#'
#' @param slug  Event identifier. Same three-form convention as init_blog_post().
#' @param date  Publication date, e.g. "2026-03-14". Defaults to today. This
#'   is when the announcement goes live, not when the event happens.
#' @param event_date  Optional date the event itself takes place, e.g.
#'   "2026-07-11" -- the start date, for a multi-day event. Recorded
#'   separately as `event-date:` in the front matter so listings can
#'   surface the date that matters to a reader deciding whether to attend.
#' @param event_end  Optional last day of a multi-day event, e.g.
#'   "2026-07-12" for a weekend gasshuku running the 11th through the
#'   12th. Requires event_date to also be set. Written as `event-end:`
#'   only when supplied -- single-day events have no such field.
#' @param image  Optional path to a source photo. See init_blog_post().
#' @param yaml_data Optional path to a .yml file containing an `author` key.
#'
#' @examples
#' init_event_post("dave-millar-seminar", event_date = "2026-09-12")
#' init_event_post("Fall Gasshuku", event_date = "2026-10-10", event_end = "2026-10-12")
#' init_event_post("Bob Poresky Seminar", date = "2026-05-01",
#'                  event_date = "2026-07-18", image = "~/Photos/flyer-hero.jpg")
init_event_post <- function(slug, date = NULL, event_date = NULL, event_end = NULL, image = NULL,
                            yaml_data = NULL, thumb_width = 1000, thumb_height = 750,
                            thumb_mode = "cover", bg_color = "white") {
    categories <- c("seminar", "gasshuku", "workshop", "testing")
    .scaffold_post(section = "events", slug = slug, date = date, event_date = event_date,
                   event_end = event_end, image = image, yaml_data = yaml_data,
                   categories = categories, with_code = FALSE,
                   thumb_width = thumb_width, thumb_height = thumb_height,
                   thumb_mode = thumb_mode, bg_color = bg_color)
}


#' Scaffold an event post with R code support
#'
#' Like init_event_post() but includes toc, number-sections, and execute
#' blocks for event posts that embed R output.
#'
#' @inheritParams init_event_post
#'
#' @examples
#' init_event_post_with_code("2026-attendance-summary", event_date = "2026-11-01")
init_event_post_with_code <- function(slug, date = NULL, event_date = NULL, event_end = NULL, image = NULL,
                                      yaml_data = NULL, thumb_width = 1000, thumb_height = 750,
                                      thumb_mode = "cover", bg_color = "white") {
    categories <- c("seminar", "gasshuku", "workshop", "testing")
    .scaffold_post(section = "events", slug = slug, date = date, event_date = event_date,
                   event_end = event_end, image = image, yaml_data = yaml_data,
                   categories = categories, with_code = TRUE,
                   thumb_width = thumb_width, thumb_height = thumb_height,
                   thumb_mode = thumb_mode, bg_color = bg_color)
}


#' Set or regenerate a post's thumbnail
#'
#' Handles the workflow where a post already exists -- scaffolded by
#' init_blog_post()/init_event_post() -- and a photo gets dropped into
#' its folder by hand afterward, rather than being handed to the
#' scaffolding function up front. Generates a cropped/resized "-thumb"
#' derivative and writes it into the post's front-matter `image:` field.
#' Safe to re-run: each call regenerates the thumbnail from the
#' original.
#'
#' @param section  "blog" or "events".
#' @param slug  The post's slug, matched fuzzily against the
#'   <date>-<slug> directory name, same convention as retract_post().
#' @param image  Optional path to a source photo. If omitted,
#'   set_thumbnail() looks inside the post's own directory for a
#'   candidate: if exactly one non-thumbnail image file is found, it's
#'   used automatically; if none or more than one are found, you'll
#'   need to pass image explicitly to disambiguate.
#' @param thumb_width,thumb_height  Target thumbnail dimensions, in
#'   pixels. Default 1000x750 (4:3), matching init_blog_post().
#' @param thumb_mode  "cover" (default) crops the image to fill the
#'   canvas -- right for photographs, where losing a sliver off an edge
#'   is harmless. "contain" shrinks the whole image to fit within the
#'   canvas with nothing cropped, padding the rest -- right for flyers
#'   and posters, where Quarto's own listing-grid crop would otherwise
#'   cut off text.
#' @param bg_color  Padding color used in "contain" mode. Default
#'   "white". Only matters when thumb_mode = "contain".
#'
#' @examples
#' # Photo already sitting in the post's own folder:
#' set_thumbnail("blog", "summer-gasshuku-recap")
#'
#' # A flyer, where cropping would cut off text:
#' set_thumbnail("events", "grand-opening-seminar", thumb_mode = "contain")
set_thumbnail <- function(section, slug, image = NULL, thumb_width = 1000, thumb_height = 750,
                          thumb_mode = "cover", bg_color = "white") {
    dir_path  <- .find_post_dir(section, slug)
    post_path <- file.path(dir_path, "index.qmd")
    
    if (!file.exists(post_path)) {
        stop("Expected index.qmd not found in ", dir_path, call. = FALSE)
    }
    
    if (!is.null(image)) {
        # Source lives elsewhere -- copy it in, same as init_*_post().
        if (!file.exists(image)) {
            stop("image not found: ", image, call. = FALSE)
        }
        source_path <- file.path(dir_path, basename(image))
        if (!file.exists(source_path)) {
            file.copy(image, source_path, overwrite = FALSE)
        }
        
    } else {
        # No path given -- look for a photo already dropped into the
        # post's own folder by hand. Exclude existing "-thumb" files so
        # re-running this on a post that already has a thumbnail
        # doesn't pick up its own derivative as a second candidate.
        thumb_pattern <- "-thumb\\.[a-zA-Z0-9]+$"
        image_pattern <- "\\.(jpe?g|png|webp|gif|tiff?)$"
        
        candidates <- list.files(dir_path, pattern = image_pattern,
                                 ignore.case = TRUE, full.names = TRUE)
        candidates <- candidates[!grepl(thumb_pattern, candidates, ignore.case = TRUE)]
        
        if (length(candidates) == 0) {
            stop(
                "No image found in ", dir_path, ". ",
                "Drop a photo into the post's folder, or pass image = \"path/to/photo.jpg\".",
                call. = FALSE
            )
        }
        if (length(candidates) > 1) {
            stop(
                "More than one candidate image found in ", dir_path, ":\n",
                paste0("  - ", basename(candidates), collapse = "\n"),
                "\nPass image = \"...\" to specify which one.", call. = FALSE
            )
        }
        
        source_path <- candidates[1]
    }
    
    thumb_name <- .generate_thumbnail(source_path, dir_path, thumb_width, thumb_height,
                                      mode = thumb_mode, bg_color = bg_color)
    
    found <- .replace_front_matter_line(
        post_path,
        key_pattern = "^image:\\s*",
        new_line    = paste0('image: "', thumb_name, '"')
    )
    
    if (!found) {
        stop("No 'image:' field found in ", post_path, ". Front matter may be malformed.", call. = FALSE)
    }
    
    message("Thumbnail set: ", thumb_name)
    invisible(thumb_name)
}


#' Retract a post
#'
#' By default, unpublishes a post by flipping its front-matter `status`
#' field from "published" to "draft," which pulls it out of listings
#' while leaving the file and its git history untouched. Pass
#' permanent = TRUE to delete the post's directory outright.
#'
#' The post is located by a fuzzy match on `slug` against the
#' <date>-<slug> directory naming convention, so there's no need to
#' know or retype the date the post was created under.
#'
#' @param section  "blog" or "events".
#' @param slug  The post's slug (same three-form convention as
#'   init_blog_post()). Matched against the tail end of the directory name.
#' @param permanent  If TRUE, deletes the post directory instead of
#'   unpublishing it. Irreversible outside of git history. Default FALSE.
#'
#' @examples
#' retract_post("blog", "summer-gasshuku-recap")
#' retract_post("events", "dave-millar-seminar", permanent = TRUE)
retract_post <- function(section, slug, permanent = FALSE) {
    dir_path <- .find_post_dir(section, slug)
    
    if (permanent) {
        unlink(dir_path, recursive = TRUE)
        message("Permanently deleted: ", dir_path)
        return(invisible(NULL))
    }
    
    post_path <- file.path(dir_path, "index.qmd")
    if (!file.exists(post_path)) {
        stop("Expected index.qmd not found in ", dir_path, call. = FALSE)
    }
    
    lines <- readLines(post_path)
    status_line <- grep("^status:\\s*", lines)
    
    if (length(status_line) == 0) {
        stop("No 'status:' field found in ", post_path, ". Nothing to retract.", call. = FALSE)
    }
    
    if (grepl("draft", lines[status_line[1]])) {
        message("Post is already a draft: ", post_path)
        return(invisible(post_path))
    }
    
    lines[status_line[1]] <- "status: draft"
    writeLines(lines, post_path)
    message("Retracted (set to draft): ", post_path)
    invisible(post_path)
}