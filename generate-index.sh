#!/bin/bash

# Start index file
cat > index.md << 'EOF'
---
title: fpunfold
---

My adventures with code.

## Posts

EOF

# Create temporary file with posts and dates for sorting
> temp_posts.txt

for post in posts/*.md; do
    filename=$(basename "$post" .md)
    title=$(grep "^title:" "$post" | sed 's/title: //')
    date=$(grep "^date:" "$post" | sed 's/date: //')
    
    # Format date nicely
    formatted_date=$(date -j -f "%Y-%m-%d" "$date" "+%B %d, %Y" 2>/dev/null || echo "$date")
    
    # Add to temp file with date prefix for sorting
    echo "$date|[$title](${filename}.html) - $formatted_date" >> temp_posts.txt
done

# Sort by date (newest first) and format as markdown list
sort -r temp_posts.txt | cut -d'|' -f2 | sed 's/^/- /' >> index.md

rm temp_posts.txt

# Convert index to HTML
pandoc index.md -s --css=https://unpkg.com/sakura.css/css/sakura.css -o docs/index.html

echo "Generated index.html"
