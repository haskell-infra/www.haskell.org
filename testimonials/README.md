# Haskell.org Testimonials

Is your company using haskell? Do you want to tell the world about it? This is
the place! This repository contains all of our haskell user testimonials. If you
want to add a new testimonial, you can add a new file and logo in this
directory. Here's how:

## Adding Your Testimonial

A testimonial is a simple yaml file with three required keys:

### Required Keys

1. `companyName` is the name you would like the quote attributed to. This can be
   the full name of your company, or a name you do business as.
2. `logoURL` is the path (relative to the testimonials directory) where we can
   find the logo that should be displayed on the page. Make sure to add the logo
   too. Logos are usually stored in the `logos` directory. There's a section
   later on in this document with guidelines on how to format your logo for use
   on the site.
3. `shortTestimonial` is the quote that will be displayed on the landing
   page. Keep it to 1 or 2 sentences, and avoid any markup, images, or emoji.

### Reserved Keys

We've reserved two additional keys that aren't being used yet, but might be used
in future versions of this project that you can include if you like:

1. `language` if present should contain an IETF BCP 47 language tag string,
   indicating the language used in the quote.
2. `longTestimonial` if present should contain a longer quote that may be shown
   to users who click to expand a short quote, or on a separate page with longer
   quotes.

## Example Testimonial

Below is an example testimonial that you can use as a starting point.

```yaml
---
companyName: Company B, Incorporated With A Long Name
logoURL: logos/your-company/sample-logo-2.png
shortTestimonial: |
  This is just a couple of sentences long. It is shorter than the other quote.
language: "enUS"
longTestimonial: |
  this is a longer testimonial. It won't be shown yet, but it might be used later if we click to expand the quotes, or do a separate page with longer quotes.
```

## Logo Guidelines

_Note: These guidelines are subject to change as we update the style of the
site_.

When uploading a logo, or logos, it's best to follow as many of the guidelines
as possibe from the list below. This will help us make sure that your logo looks
good and fits with the theme of the site.

1. All logos should be provided as PNG files.
2. When possible, also provide an SVG file. This will allow us to resize the
   images losslessly if necessary.
3. Prefer transparent backgrounds when possible.
4. The file should be square. If the logo is not square, you should include
   transparent padding with your preferred alignment to get the file to be
   square.
