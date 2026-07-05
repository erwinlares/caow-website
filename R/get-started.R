# get-started.R
# Prints a concise workflow reference for common site tasks.
# Sourced automatically via .Rprofile when the project is opened.

get_started <- function() {
  cat("
=======================================================
  aikidoofwisconsin — quick reference
=======================================================

NEW BLOG POST
  1. init_blog_post(\"your-post-slug\")
       → creates blog/YYYY-MM-DD-your-post-slug/index.qmd
     Optional arguments:
       date        publication date, e.g. \"2026-03-14\" (default: today)
       image       path to a source photo, copied in + auto-thumbnailed
       yaml_data   path to a .yml file with an `author` key
  2. Write the post
  3. If it has a gallery, see GALLERY IMAGES below
  4. Set status: published in the YAML front matter
  5. quarto render blog/YYYY-MM-DD-your-post-slug/index.qmd
  6. git add . && git commit -m \"new post: your post slug\" && git push

NEW EVENT POST
  1. init_event_post(\"your-event-slug\", event_date = \"2026-07-11\")
       → creates events/YYYY-MM-DD-your-event-slug/index.qmd
     Optional arguments:
       date        publication date, e.g. \"2026-03-14\" (default: today)
       event_date  the date the event itself happens -- its start date,
                   for a multi-day event (distinct from the publication
                   date; used to sort the events listing)
       event_end   last day of a multi-day event, e.g. \"2026-07-12\" for
                   a weekend gasshuku running the 11th-12th. Requires
                   event_date to also be set. Omit entirely for a
                   single-day event -- no event-end field gets written.
       image       path to a source photo, copied in + auto-thumbnailed
       yaml_data   path to a .yml file with an `author` key

     # single-day:
     init_event_post(\"dave-millar-seminar\", event_date = \"2026-09-12\")
     # multi-day:
     init_event_post(\"fall-gasshuku\", event_date = \"2026-10-10\", event_end = \"2026-10-12\")
  2. Write the event description
  3. If it has a gallery, see GALLERY IMAGES below
  4. Set status: published in the YAML front matter
  5. quarto render events/YYYY-MM-DD-your-event-slug/index.qmd
  6. git add . && git commit -m \"new event: your event slug\" && git push

SLUG FORMATS ACCEPTED
  \"word-slug\"         → dir: word-slug,        title: Word Slug
  \"Natural Language\"  → dir: natural-language,  title: Natural Language
  \"`code-term`\"       → dir: code-term,         title: `code term`

THUMBNAIL IMAGES
  Two ways to set one, depending on when you have the photo ready:

  1. Have the photo before scaffolding:
       init_blog_post(\"your-slug\", image = \"path/to/photo.jpg\")
     The original is copied into the post folder untouched; a cropped
     and resized \"<name>-thumb.jpg\" is generated alongside it and
     wired into the front-matter `image:` field automatically.

  2. Scaffold first, drop the photo in by hand afterward (the more
     common case):
       init_blog_post(\"your-slug\")
       # ...drop photo.jpg into blog/YYYY-MM-DD-your-slug/ via Finder...
       set_thumbnail(\"blog\", \"your-slug\")
     With no image argument, set_thumbnail() looks for a single photo
     already in the post's folder and uses it automatically. If the
     folder has more than one candidate (e.g. gallery photos are
     already in there too), pass image = \"...\" to disambiguate.

     Slug is just the slug -- not the full <date>-<slug> folder name.
     set_thumbnail(\"events\", \"2025-10-10-grand-opening\")  # wrong
     set_thumbnail(\"events\", \"grand-opening\")               # right

  Either way, override thumb_width / thumb_height if a photo needs
  different framing (default 1000x750, a 4:3 crop). Safe to re-run
  set_thumbnail() any time to regenerate the thumbnail.

  COVER vs CONTAIN (thumb_mode)
    thumb_mode = \"cover\"    (default) crops to fill the canvas.
                              Right for photographs -- losing a sliver
                              off an edge is harmless.
    thumb_mode = \"contain\"  shrinks the whole image to fit within the
                              canvas, nothing cropped, and pads the
                              rest (bg_color, default \"white\"). Right
                              for flyers and posters, where cropping
                              would cut off text -- Quarto's own
                              listing grid crops-to-fill by default, so
                              a flyer left in \"cover\" mode gets cropped
                              twice: once by us, once by Quarto.

    set_thumbnail(\"blog\", \"summer-gasshuku-recap\")
      # a photo -- cover is correct, no need to specify it
    set_thumbnail(\"events\", \"grand-opening\", thumb_mode = \"contain\")
      # a flyer -- contain preserves the full poster

GALLERY IMAGES (multiple photos within a post)
  Reference the copied image directly in the post's Markdown body and
  tag it with .lightbox and a shared group so readers can click into
  one and page through the rest without leaving the page:

    ![Caption](photo1.jpg){.lightbox group=\"your-post-slug\"}
    ![Caption](photo2.jpg){.lightbox group=\"your-post-slug\"}
    ![Caption](photo3.jpg){.lightbox group=\"your-post-slug\"}

  Use the same group value for every image that belongs to one gallery.
  Images without .lightbox render normally and are not clickable.

RETRACT A POST OR EVENT
  retract_post(\"blog\", \"your-post-slug\")
       → sets status: draft, pulling it out of listings (reversible,
         file and git history untouched)
  retract_post(\"events\", \"your-event-slug\", permanent = TRUE)
       → deletes the post directory outright (irreversible outside
         of git history -- use with care)
  Slug is matched fuzzily against the <date>-<slug> folder name, so
  there's no need to know or retype the date it was created under.

EDIT AN EXISTING POST OR EVENT
  1. Edit the .qmd file directly
  2. If you changed the YAML front matter, re-render first:
       quarto render <path-to-index.qmd>
  3. git add . && git commit -m \"update: description\" && git push

STRUCTURAL CHANGES (layout, CSS, _quarto.yml, new pages)
  1. Edit the relevant files
  2. git add . && git commit -m \"describe change\" && git push

=======================================================
")
}