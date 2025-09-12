#!/bin/bash

# Create output directory
mkdir -p docs

# Convert all posts
for post in posts/*.md; do
    filename=$(basename "$post" .md)
    pandoc "$post" -s --css=https://unpkg.com/sakura.css/css/sakura.css -o "docs/${filename}.html"
    echo "Converted: $post -> docs/${filename}.html"
done

echo "All posts converted with sakura CSS!"
