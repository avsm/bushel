# Bushel File Format Specification

## Overview

Bushel is a knowledge management system designed to store and organize different types of entries such as notes, papers, ideas, projects, contacts, news items, and videos. This specification details the file format, structure, and conventions used by Bushel to allow independent implementations.

## Core Concepts

1. **Entries**: The basic unit of information in Bushel. Each entry has a unique ID, type, and content.
2. **Database**: A collection of entries stored as individual files in a directory.
3. **Tags**: Labels attached to entries for categorization and organization.
4. **Markdown**: The primary format for content, extended with YAML front matter.

## File Structure

### File Naming

Bushel uses two different file naming conventions:

1. **Category-Based Organization**: In the standard Bushel database structure, entries are organized into category directories with specific naming formats:

   - **Notes**: Dated filenames in the format `YYYY-MM-DD-slug.md` (e.g., `2022-05-15-project-update.md`)
   - **Contacts**: Simple slug-based filenames (e.g., `jsmith.md`)
   - **Projects**: Simple slug-based filenames (e.g., `bushel-implementation.md`)
   - **Ideas**: Simple slug-based filenames (e.g., `new-algorithm.md`)
   - **News**: Dated filenames similar to notes
   - **Papers**: Organized in subdirectories by slug with version files (e.g., `papers/my-paper/v1.md`)
   
2. **UUID-Based Organization**: For some content sources (like PeerTube videos), Bushel uses UUID-based naming:
   ```
   {uuid}.md
   ```
   Example: `053fb1b4-d9e0-4bdc-b119-39326d90f62e.md`

### File Format

Entries are stored as Markdown files with YAML front matter. The general structure is:

```
---
title: {title}
slug: {slug}
date: {date}
tags: {list of tags}
{additional type-specific fields}
---

{markdown content}
```

The YAML front matter is enclosed by `---` lines and contains metadata about the entry. The actual content follows in Markdown format.

**Note**: Unlike some metadata systems, Bushel does not require an explicit `type` field in the front matter. The entry type is determined by the subdirectory where the file is stored (e.g., files in `/data/notes/` are notes, regardless of front matter).

### Common Front Matter Fields

These fields are common across most entry types:

| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `title` | String | Title or summary of the entry | Yes |
| `slug` | String | Short identifier for the entry | Yes |
| `tags` | List | List of tags for categorization | No |
| `date` | Date (YYYY-MM-DD) | Creation or publication date | Most types |
| `uuid` | UUID | Unique identifier (required in UUID-based entries) | For UUID-based entries |

## Entry Types

Bushel supports several entry types, each with type-specific fields and conventions:

### Note (`type: note`)

A basic text note for capturing general information.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `date` | ISO 8601 date | Creation date | Yes |
| `updated` | ISO 8601 date | Last update date | No |
| `draft` | Boolean | Whether the note is a draft | No |
| `index_page` | Boolean | Whether the note is an index page | No |
| `synopsis` | String | Brief summary of the note | No |
| `titleimage` | String | Image to display with the title | No |
| `via` | Object | Reference to external source | No |
| `sidebar` | String | Optional sidebar content | No |

Example:
```yaml
---
type: note
title: My Note Title
slug: my-note
date: 2023-05-20
tags:
  - personal
  - idea
  - reminder
---

This is the content of my note.
```

### Paper (`type: paper`)

A reference to an academic paper or article. Papers are stored differently from other entries, using a directory structure that supports versioning.

#### Directory Structure

Papers are organized in subdirectories by slug, with version files inside:
```
data/papers/[paper-slug]/[version].md
```

For example:
```
data/papers/ml-advances/v1.md
data/papers/ml-advances/v2.md
```

#### Version Management

Paper entries support versioning through these mechanisms:
- Each version is stored in a separate file (e.g., `v1.md`, `v2.md`)
- The version number is extracted from the filename
- The `latest` field is computed automatically by sorting versions and marking the highest as `latest: true`
- When rendering, only the latest version is shown by default, but previous versions remain accessible

#### Paper Fields

Papers differ from other entry types by storing most metadata as JSON in a `paper` field.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `author` | List | List of author names | Yes |
| `year` | String | Publication year | Yes |
| `month` | String | Publication month (three-letter code: "jan", "feb", etc.) | No |
| `bibtype` | String | Bibliographic type (e.g., "article", "inproceedings") | Yes |
| `doi` | String | Digital Object Identifier | No |
| `url` | String | URL to the paper | No |
| `journal` | String | Journal name (for articles) | For articles |
| `booktitle` | String | Conference/book name (for inproceedings/book chapters) | For conferences |
| `volume` | String | Volume number | No |
| `number` | String | Issue number | No |
| `pages` | String | Page range | No |
| `publisher` | String | Publisher name | No |
| `latest` | Boolean | Whether this is the latest version (computed automatically) | No |
| `projects` | List | Related project slugs | No |
| `slides` | List | URLs to presentation slides | No |
| `bib` | String | BibTeX entry | No |
| `tags` | List | Tags for the paper | No |
| `keywords` | List | Keywords (treated as additional tags) | No |

#### Bibliographic Types

Common bibliographic types (`bibtype`) and their interpretation:
- `article` - Journal article (tagged as "journal")
- `inproceedings` - Conference paper (tagged as "conference")
- `techreport` - Technical report (tagged as "report")
- `misc` - Preprint or miscellaneous (tagged as "preprint")
- `book` - Book or book chapter (tagged as "book")

#### Example:
```yaml
---
title: Advances in Machine Learning
author:
  - Jane Doe
  - John Smith
year: "2023"
month: "apr"
bibtype: article
journal: Journal of AI Research
doi: 10.1234/5678
url: https://example.org/paper
tags:
  - machine-learning
  - ai
  - research
---

This paper presents novel approaches to machine learning...
```

The file would be stored at `data/papers/advances-in-machine-learning/v1.md`.

### Idea (`type: idea`)

A concept or idea to explore further, often used for research project proposals.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `level` | String | Academic level (Any, PartII, MPhil, PhD, Postdoc) | Yes |
| `project` | String | Associated project slug | Yes |
| `status` | String | Status (Available, Discussion, Ongoing, Completed) | Yes |
| `year` | Integer | Year of idea creation | Yes |
| `month` | Integer | Month of idea creation | Yes |
| `supervisors` | List | List of supervisor handles | No |
| `students` | List | List of student handles | No |
| `reading` | String | Suggested reading materials | No |

Example:
```yaml
---
type: idea
title: New Algorithm for Data Processing
slug: data-processing-algo
level: PhD
project: data-systems
status: Available
year: 2023
month: 6
supervisors:
  - alice
  - bob
tags:
  - algorithm
  - optimization
  - research
---

Develop a new algorithm that optimizes data processing...
```

### Project (`type: project`)

A defined project with goals and status.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `status` | String | Project status (e.g., "active", "completed", "planned") | Yes |
| `start` | Integer | Project start year | Yes |

Example:
```yaml
---
type: project
title: Bushel Format Implementation
slug: bushel-impl
status: active
start: 2023
tags:
  - software
  - development
---

Project to implement the Bushel format in Python.
```

### Contact (`type: contact`)

Information about a person or organization.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `name` | String | Full name of the contact | Yes |
| `handle` | String | Unique identifier for the contact | Yes |
| `email` | String | Email address | No |
| `organization` | String | Organization or company | No |
| `website` | String | Website URL | No |
| `twitter` | String | Twitter/X handle | No |
| `github` | String | GitHub username | No |

Example:
```yaml
---
type: contact
title: Jane Doe
slug: jane-doe
name: Jane Doe
handle: jane
email: jane@example.com
organization: Example Corp
website: https://janedoe.com
twitter: janedoe
github: janedoe
tags:
  - colleague
  - expert
---

Notes about Jane and our collaboration.
```

### News (`type: news`)

A news item or article reference.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `date` | ISO 8601 date | Publication date | Yes |
| `source` | String | News source | Yes |
| `url` | String | URL to the news article | No |
| `slug_ent` | String | Related entity slug | No |

Example:
```yaml
---
type: news
title: New Breakthrough in Quantum Computing
slug: quantum-breakthrough
date: 2023-05-20
source: Tech News Daily
url: https://example.com/news/quantum-breakthrough
tags:
  - quantum
  - computing
  - technology
---

Researchers have achieved a significant breakthrough...
```

### Video (`type: video`)

A reference to a video.

**Additional fields:**
| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `uuid` | String | Unique identifier (used as filename in UUID-based structure) | Yes |
| `slug` | String | Short identifier (used as filename in category-based structure) | Yes |
| `url` | String | URL to the video | Yes |
| `published_date` | ISO 8601 date | Publication date | Yes |
| `description` | String | Video description | Yes |
| `talk` | Boolean | Whether the video is a talk/presentation | No |
| `paper` | String | Related paper slug | No |
| `project` | String | Related project slug | No |

Example (Category-based structure):
```yaml
---
title: Introduction to OCaml Programming
description: A comprehensive introduction to OCaml programming.
url: https://example.com/videos/ocaml-intro
uuid: 2b8b33bb-8ec6-4a78-b7fd-031cba82e7e8
slug: ocaml-intro
published_date: 2023-03-15T10:30:00Z
talk: true
tags:
  - programming
  - ocaml
  - tutorial
paper: ocaml-paper-2023
project: ocaml-training
---
```

Example (UUID-based structure from PeerTube):
```yaml
---
title: Peregrine falcon chick #1 on the Pembroke roof
description: This is the first of the Cambridge peregrine falcons to fledge...
url: https://crank.recoil.org/videos/watch/053fb1b4-d9e0-4bdc-b119-39326d90f62e
uuid: 053fb1b4-d9e0-4bdc-b119-39326d90f62e
slug: "236"
published_date: 2023-06-06T16:15:27-00:00
talk: false
tags: []
paper:
project:
---
```

## Tag System

Tags are used to categorize and organize entries. They are specified in the front matter as a list.

### Tag Format

- Tags are represented as strings in a YAML list.
- Multiple words can be separated by dashes (`-`) or spaces.
- Tags are typically lowercase.

Example: 
```yaml
tags:
  - programming
  - machine-learning
  - research
  - to-read
```

### Special Tag Prefixes

Bushel uses special prefixes to denote different types of tags:

1. `:` prefix (Slug Tags): References another entry by its slug
   - Example: `:project-x` references the entry with slug "project-x"

2. `@` prefix (Contact Tags): References a contact by handle
   - Example: `@jane` references the contact with handle "jane"

3. `#` prefix (Set Tags): References a collection of entries by type
   - Example: `#papers` references all paper entries

4. Numeric Year Tags: Any number between 1900-2100 is interpreted as a year tag
   - Example: `2023` tags content from that year

## Markdown Extensions

The Bushel format uses standard Markdown with the following extensions or conventions:

### Cross-References

Bushel implements a rich cross-referencing system that extends standard Markdown links. These extensions allow entries to reference each other, contacts, and tags with special prefixes:

#### Special Link Syntax

1. **Slug Links** - Reference another entry by slug:
   ```markdown
   [:entry-slug]
   ```
   When rendered, this is replaced with a link to the entry, using the entry's title as the link text.

2. **Contact Links** - Reference a contact by handle:
   ```markdown
   [@contact-handle]
   ```
   When rendered, this is replaced with a link to the contact, using the contact's name as the link text.

3. **Tag Links** - Reference all entries with a specific tag:
   ```markdown
   [##tag-name]
   ```
   When rendered, this is replaced with a link to a tag search page.

#### Link Resolution

The resolution process works as follows:

1. Markdown is parsed using a custom resolver that detects special prefixes
2. Links with special prefixes are transformed into the appropriate link types
3. When rendered:
   - Slug links resolve to `/[entry-type]/[slug]` URLs with the entry's title as the link text
   - Contact links resolve to custom `bushel:contact:[handle]:[url]` protocol links
   - Tag links resolve to `/news?t=[tag]` filter URLs

#### Image References

Special handling is also provided for image references with the Bushel slug prefix:

```markdown
![Alt text](:image-slug)
```

These are resolved to `/images/[image-slug]` or, if the slug refers to a video entry, to the video's URL.

#### Obsidian Compatibility

Bushel also supports conversion to Obsidian-compatible link formats:
- Slug links become Obsidian wiki links: `[[entry-slug]]`
- Tag links become Obsidian tags: `#tag-name`

This allows content to be viewed and edited in Obsidian without losing link relationships.

### Sections

Content is often organized into sections using Markdown headings. Common sections include:

- `## Summary` - Brief overview
- `## Notes` - Detailed notes
- `## References` - Related resources
- `## Future Work` - Planned improvements or follow-ups

### Code Blocks

Standard Markdown code blocks are used for code snippets, with language identification:

````
```ocaml
let hello = "world"
```
````

## Directory Structure

A typical Bushel database consists of:

1. **Standard Category-Based Structure**
   - `data/` - Root directory for all entries
     - `data/notes/` - Notes with date-based filenames (YYYY-MM-DD-slug.md)
     - `data/papers/` - Papers organized in subdirectories by slug with version files (e.g., `papers/my-paper/v1.md`)
     - `data/projects/` - Projects with simple slug-based filenames
     - `data/ideas/` - Ideas with simple slug-based filenames
     - `data/videos/` - Videos with simple slug-based filenames
     - `data/news/` - News items with date-based filenames
     - `data/contacts/` - Contacts with simple slug-based filenames
     - `data/sidebar/` - Optional sidebar content for notes (filename matches note's basename)
   - `images/` - Images referenced by entries
     - `images/index.json` - Index of images

   **Important**: The subdirectory name determines the type of entry. The Bushel library loads files from these specific directories and processes them according to their type, without requiring explicit type fields in the front matter.

2. **UUID-Based Structure**
   - Typically used for external content sources like PeerTube videos
   - Flat directory with all entries as UUID-named files (e.g., `053fb1b4-d9e0-4bdc-b119-39326d90f62e.md`)
   - Front matter contains both the UUID and a separate slug field
   - Without subdirectory categorization, these files typically need additional metadata to indicate their type

## Jekyll Compatibility

Bushel is designed to be compatible with Jekyll-formatted files, with some extensions. This section details the Jekyll compatibility aspects of the format.

### Front Matter Format

Bushel uses YAML front matter delimited by triple dashes, identical to Jekyll:

```
---
key: value
array:
  - item1
  - item2
---
```

### Date Extraction

Dates in Bushel can come from multiple sources, similar to Jekyll:

1. **Filename-based dates**: For notes and news items, filenames often follow the Jekyll pattern:
   ```
   YYYY-MM-DD-slug.md
   ```
   The date is extracted from the filename and used as the entry's creation date.

2. **Front matter dates**: Entries can specify dates in the front matter using either:
   - `date` field: Full date in YYYY-MM-DD format
   - `year` and `month` fields: For papers, ideas, and some other types
   - `published_date` field: For videos and some other types (RFC3339 format)

3. **Update tracking**: Some entry types support an `updated` field to track the last modification date separate from the creation date.

### Tag Scanning

One important aspect of Bushel is automatic tag scanning:

1. **Explicit Tags**: Tags listed in the front matter's `tags` field
2. **Implicit Tags**: 
   - Year tags added automatically based on the entry's date
   - Type tags added automatically based on the entry's type (e.g., "notes", "papers")
   - For papers: bibliographic type tags (e.g., "journal", "conference")
3. **Body Tags**: References in the body content using the special syntax (`:slug`, `@contact`, etc.) are also considered implicit tags

The system scans Markdown content to find all referenced entries, building a network of relationships automatically.

## Implementation Considerations

When implementing a Bushel-compatible system, consider:

1. **Directory Significance**: The subdirectory an entry resides in determines its type (e.g., files in `data/notes/` are notes, files in `data/papers/` are papers)
2. **File Naming**: Support both category-based naming (with Jekyll-style dates) and UUID-based naming
3. **YAML Parsing**: Use a robust YAML parser that handles the front matter format
4. **Date Extraction**: Implement the various date extraction methods from filenames and front matter
5. **Markdown Parsing**: Use a Markdown parser that can be extended with custom link handling
6. **Cross-Reference Scanning**: Add link resolution similar to the `scan_for_slugs` function in the reference implementation
7. **Tag Management**: Implement both explicit and implicit tagging, including body content scanning
8. **Entry Relationships**: Support the complex relationships between different entry types
9. **Paper Versioning**: Handle the nested directory structure and versioning system for papers
10. **Error Handling**: Gracefully handle missing fields or malformed entries
11. **Timeline Generation**: Support sorting entries by date for chronological views

## Data Relationships and Entry Network

Bushel creates a rich network of interconnected entries through various relationship types. Understanding these relationships is crucial for implementing a compatible system.

### Entry Relationships

1. **Direct References**:
   - Explicit references using slug tags (`:slug`)
   - References are extracted by scanning entry bodies for the special link syntax
   - These create bidirectional relationships between entries

2. **Contact Relationships**:
   - Papers list authors that reference contacts
   - Ideas reference supervisors and students
   - Contact references use handles (`@handle`)

3. **Project Associations**:
   - Papers can reference projects via the `projects` field
   - Ideas are associated with a primary project
   - Videos can reference a related project

4. **Media Relationships**:
   - Videos can reference papers they're related to
   - Papers can list related slides and videos

5. **News Associations**:
   - News items can reference specific entries with `slug_ent`
   - News items can be filtered by tags

6. **Tag-Based Relationships**:
   - Entries sharing the same tags form implicit relationships
   - Type tags group entries by their type (e.g., all papers)
   - Year tags group entries chronologically

### Entry Sorting and Ordering

Different entry types have specific sorting rules:

1. **Notes**: Sorted by date descending (newest first) with updated dates taking precedence
2. **Papers**: Sorted by publication date descending
3. **Ideas**: Complex sorting:
   - Primary sort by status (Available → Discussion → Ongoing → Completed)
   - Secondary sort by academic level
   - Tertiary sort by date (reversed for Completed status)
4. **Projects**: Typically sorted by start year
5. **Videos**: Sorted by published date descending

When displaying feeds or timelines, entries from different types can be intermixed and ordered chronologically using the `compare` functions.

### Relationship Implementation

To implement this relationship network:

1. **In-Memory Index**: Build an in-memory index of all entries by slug (similar to the `slugs` hashtable in the reference implementation)
2. **Body Scanning**: Scan entry bodies for references using patterns similar to the `scan_for_slugs` function
3. **Tag Extraction**: Implement both explicit tag extraction from front matter and implicit tagging
4. **Bidirectional Lookup**: Support looking up entries that reference a specific entry, as well as what a specific entry references

These relationships form the foundation of Bushel's knowledge graph capabilities.

## API Operations

Implementations should support the following operations:

1. **Create**: Add new entries to the database
2. **Read**: Retrieve entries by ID, slug, or query by tags
3. **Update**: Modify existing entries
4. **Delete**: Remove entries from the database
5. **List**: Enumerate entries, optionally filtered by type or tags
6. **Link**: Find related entries via tags or explicit references
7. **Export**: Convert entries to other formats (optional)

## Example Implementation Pseudocode

Here are examples of how to implement key components of a Bushel-compatible system.

### Loading the Database

```python
def load_database(base_directory):
    db = Database()
    
    # Load all entry types
    db.papers = load_papers(base_directory + "/data/papers")
    db.notes = load_notes(base_directory + "/data/notes")
    db.ideas = load_ideas(base_directory + "/data/ideas")
    db.projects = load_projects(base_directory + "/data/projects")
    db.contacts = load_contacts(base_directory + "/data/contacts")
    db.videos = load_videos(base_directory + "/data/videos")
    db.news = load_news(base_directory + "/data/news")
    
    # Build slug index
    db.slug_index = {}
    for entry_list in [db.papers, db.notes, db.ideas, db.projects, db.videos]:
        for entry in entry_list:
            db.slug_index[entry.slug] = entry
            
    # Scan for references in content
    for entry_type in db.all_entries():
        for entry in entry_type:
            entry.references = scan_for_references(entry.body, db)
            
    return db

def load_papers(papers_dir):
    papers = []
    
    # Find all paper directories
    for slug_dir in list_directories(papers_dir):
        slug = os.path.basename(slug_dir)
        
        # Find all version files
        versions = []
        for file in list_files(slug_dir, "*.md"):
            ver = os.path.basename(file).replace(".md", "")
            paper = parse_paper_file(file, slug, ver)
            versions.append(paper)
        
        # Determine latest version
        versions.sort(key=lambda p: p.ver)
        if versions:
            versions[-1].latest = True
            
        papers.extend(versions)
        
    return papers
```

### Reading an Entry

```python
def read_entry(file_path):
    file_content = read_file(file_path)
    
    # Split front matter and content
    parts = split_by_triple_dash_delimiters(file_content)
    if len(parts) != 3:  # [empty, front_matter, content]
        raise "Invalid format: missing front matter delimiters"
    
    front_matter = parse_yaml(parts[1])
    content = parts[2].strip()
    
    # Determine entry type from the directory
    entry_type = determine_type_from_directory(file_path)
    
    # Extract common fields and type-specific fields
    if entry_type == "note":
        entry = parse_note(front_matter, content, file_path)
    elif entry_type == "paper":
        slug = os.path.basename(os.path.dirname(file_path))
        ver = os.path.basename(file_path).replace(".md", "")
        entry = parse_paper(front_matter, content, slug, ver)
    elif entry_type == "video":
        entry = parse_video(front_matter, content)
    # ... and so on for other types
    
    # Extract date from filename for certain types
    if entry_type in ["note", "news"] and entry.date is None:
        basename = os.path.basename(file_path)
        date_match = re.match(r"(\d{4}-\d{2}-\d{2})-.*\.md", basename)
        if date_match:
            entry.date = parse_date(date_match.group(1))
    
    return entry
```

### Scanning for References

```python
def scan_for_references(content, database):
    """Scan markdown content for Bushel-style references."""
    references = []
    
    # Regular expressions for different reference types
    slug_pattern = r'\[:([^\]]+)\]'  # Matches [:slug]
    contact_pattern = r'\[@([^\]]+)\]'  # Matches [@handle]
    tag_pattern = r'\[##([^\]]+)\]'  # Matches [##tag]
    
    # Find all slug references
    for match in re.finditer(slug_pattern, content):
        slug = match.group(1)
        if slug in database.slug_index:
            references.append({
                "type": "slug",
                "slug": slug,
                "target": database.slug_index[slug]
            })
    
    # Find all contact references
    for match in re.finditer(contact_pattern, content):
        handle = match.group(1)
        contact = database.find_contact_by_handle(handle)
        if contact:
            references.append({
                "type": "contact",
                "handle": handle,
                "target": contact
            })
    
    # Find all tag references
    for match in re.finditer(tag_pattern, content):
        tag = match.group(1)
        references.append({
            "type": "tag",
            "tag": tag
        })
    
    return references
```

### Converting Markdown with References

```python
def render_markdown_with_references(content, database):
    """Convert Markdown with Bushel references to HTML with proper links."""
    
    # Function to replace slug references with links
    def replace_slug_ref(match):
        slug = match.group(1)
        entry = database.find_by_slug(slug)
        if entry:
            return f'<a href="{entry.url}">{entry.title}</a>'
        return match.group(0)  # Return unchanged if not found
    
    # Function to replace contact references with links
    def replace_contact_ref(match):
        handle = match.group(1)
        contact = database.find_contact_by_handle(handle)
        if contact:
            url = contact.url or f"/contacts/{contact.handle}"
            return f'<a href="{url}">{contact.name}</a>'
        return match.group(0)
    
    # Function to replace tag references with links
    def replace_tag_ref(match):
        tag = match.group(1)
        return f'<a href="/news?t={tag}">{tag}</a>'
    
    # Replace all reference types
    content = re.sub(r'\[:([^\]]+)\]', replace_slug_ref, content)
    content = re.sub(r'\[@([^\]]+)\]', replace_contact_ref, content)
    content = re.sub(r'\[##([^\]]+)\]', replace_tag_ref, content)
    
    # Convert the modified markdown to HTML
    html = convert_markdown_to_html(content)
    return html
```

### Generating Entry Views

```python
def generate_feed(database, limit=20, tag=None):
    """Generate a time-ordered feed of entries, optionally filtered by tag."""
    
    # Collect entries from different types
    entries = []
    
    for paper in database.papers:
        if not tag or tag in paper.tags or tag == "papers":
            entries.append({"type": "paper", "entry": paper, "date": paper.date})
    
    for note in database.notes:
        if not tag or tag in note.tags or tag == "notes":
            entries.append({"type": "note", "entry": note, "date": note.date})
    
    # Add other entry types similarly...
    
    # Sort by date, newest first
    entries.sort(key=lambda e: e["date"], reverse=True)
    
    # Return limited number of entries
    return entries[:limit]
```

### Exporting to Obsidian Format

```python
def export_to_obsidian(database, output_dir):
    """Export the database to Obsidian-compatible format."""
    
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Export all entries
    for entry_type in ["notes", "papers", "ideas", "projects"]:
        for entry in getattr(database, entry_type):
            # Convert references to Obsidian format
            content = convert_references_to_obsidian(entry.body)
            
            # Create frontmatter
            frontmatter = {
                "title": entry.title,
                "date": format_date(entry.date),
                "tags": entry.tags,
                "type": entry_type[:-1]  # Remove plural 's'
            }
            
            # Write file
            filename = f"{entry.slug}.md"
            write_file(
                os.path.join(output_dir, filename),
                format_frontmatter(frontmatter) + "\n\n" + content
            )

def convert_references_to_obsidian(content):
    """Convert Bushel reference syntax to Obsidian syntax."""
    # Convert slug references to wiki links
    content = re.sub(r'\[:([^\]]+)\]', r'[[\\1]]', content)
    
    # Convert tag references to Obsidian tags
    content = re.sub(r'\[##([^\]]+)\]', r'#\\1', content)
    
    # Keep contact references as is, or convert as needed
    
    return content
```

These examples demonstrate the key components needed for a working implementation of the Bushel format. A complete implementation would include additional error handling, more sophisticated markdown processing, and support for all entry types and relationships.

## Conclusion

The Bushel file format provides a flexible, Markdown-based system for managing knowledge across different entry types. Its strength lies in its simplicity (files on disk), human-readability, and rich interconnection capabilities through its tagging and reference system.

This specification should enable developers to create compatible implementations in any programming language, allowing interoperability with existing Bushel databases.