## `bitmap%`

1. A call to `get-data-from-file` returns `#f` unless the bitmap
   was created with `save-data-from-file` and the image was loaded successfully
   (i.e. `ok?` method returns `#t`).
2. `load-file` does not work with bitmaps created by `make-platform-bitmap`,
   `make-screen-bitmap`, or `make-bitmap` in `canvas%`.

## `bitmap-dc%`

3. The methods `get-text-extent`, `get-char-height`, and `get-char-width` can
   be called before a bitmap is installed. All others cannot.
4. `set-argb-pixels` cannot be called if the current bitmap was produced by
   `make-screen-bitmap` or `make-bitmap` in `canvas%`.
5. A bitmap can be selected into at most one bitmap DC, and only when it is not
   used by a control (as a label) or in a `pen%` or `brush%` (as a stipple).

## `brush%`

6. **A brush cannot be modified while it is selected into a drawing context.**
7. A brush cannot be modified if it was obtained from a `brush-list%`.

## `color%`

8. A color created by passing a string to `make-object` or finding a color
   in a color database cannot be modified.

## `dc<%>`

9. The methods `start-doc`, `start-page`, `end-page`, and `end-doc` must
   be called in the correct sequence.

## `dc-path%`

10. Some methods extend the open sub-path, some methods close the open
    sub-path, and some methods add closed sub-paths.

## `pen%`

11. A pen cannot be modified if it was obtained from a `pen-list%`.
12. **A pen cannot be modified while it is selected into a drawing context.**

## `post-script-dc%`

13. If `as-eps` is set, then only one page can be created.

## `region%`

14. The `is-empty?` method can only be called when associated with
    a drawing context.

## `record-dc%`

15. There are no restrictions on the sequence of `start-doc`, `start-page`,
    `end-page`, and `end-doc` for `record-dc%`.
