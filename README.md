# fpunfold blog

Static blog generated with pandoc + sakura CSS.

## Build

```bash
./build.sh && ./generate-index.sh
```

## Structure

- `posts/` - Markdown posts with YAML frontmatter
- `docs/` - Generated HTML files (served by GitHub Pages)
- `build.sh` - Converts posts to HTML
- `generate-index.sh` - Creates index page
