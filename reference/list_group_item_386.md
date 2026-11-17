# Create a Bootstrap 386 list group item

Create a Bootstrap 386 list group item

## Usage

``` r
list_group_item_386(
  ...,
  active = FALSE,
  disabled = FALSE,
  type = c("basic", "action", "heading"),
  src = "#",
  title = NULL,
  subtitle = NULL,
  footer = NULL
)
```

## Arguments

- ...:

  Item content.

- active:

  Whether the item is active or not. FALSE by default. Only if type is
  "action" or "heading".

- disabled:

  Whether the item is disabled or not. FALSE by default. Only if type is
  "action" or "heading".

- type:

  Item type. Choose between "basic", "action" and "heading".

- src:

  Item external link.

- title:

  Item title (only if type is "heading").

- subtitle:

  Item subtitle (only if type is "heading").

- footer:

  Item footer content (only if type is "heading").

## Author

David Granjon, <dgranjon@ymail.com>
