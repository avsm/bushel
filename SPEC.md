# Bushel File Format Specification (Refined)

## Overview

Bushel is a knowledge management system designed to store and organize different types of entries such as notes, papers, ideas, projects, contacts, news items, and videos. This specification details the file format, structure, and conventions used by Bushel to allow independent implementations.

## Core Concepts

1. **Entries**: The basic unit of information in Bushel. Each entry has a unique ID, type, and content.
2. **Database**: A collection of entries stored as individual files in a directory.
3. **Tags**: Labels attached to entries for categorization and organization.
4. **Markdown**: The primary format for content, extended with YAML front matter.

## Implementation Notes and Clarifications

Based on practical implementation experience, the following clarifications are offered:

1. **Mandatory vs. Optional Fields**: While the specification lists required fields, implementations should be resilient when parsing entries with missing fields. Sensible defaults should be provided rather than failing to load entries entirely. This ensures backward compatibility with older entries or entries created by different tools.

2. **Database Location**: Implementations should support multiple ways to locate the database directory:
   - Command-line arguments (e.g., `--db` option)
   - Environment variables (e.g., `BUSHEL_DB`)
   - Default locations (e.g., `.db` subdirectory, current directory)

3. **Tag Handling**: Tag parsing should handle diverse formats, including:
   - Case insensitivity in tag prefixes (`:slug`, `@contact`, `#set`)
   - Automatic conversion of text tags with years (1900-2100) to year tags
   - Graceful handling of tags that don't strictly conform to specifications

4. **Date Handling**: Implementations should be flexible with dates:
   - Extract from filenames for notes and news (YYYY-MM-DD-slug.md)
   - Support both ISO 8601 dates and separate year/month fields
   - Handle both numeric and string representation of months (e.g., "jan", "feb", 1, 2)
   - Provide sensible defaults when dates are missing

5. **Bibliographic Types**: Parse BibTeX types with case-insensitive matching. Non-standard types should be gracefully handled, either by mapping to standard types or using a default type like "misc".

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

| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `title` | String | Title or summary of the entry | Yes | Derived from slug or filename |
| `slug` | String | Short identifier for the entry | Yes | Derived from filename or title |
| `tags` | List | List of tags for categorization | No | Empty list |
| `date` | Date (YYYY-MM-DD) | Creation or publication date | Most types | Current date |
| `uuid` | UUID | Unique identifier (required in UUID-based entries) | For UUID-based entries | Generated UUID |

## Entry Types

Bushel supports several entry types, each with type-specific fields and conventions:

### Note (`type: note`)

A basic text note for capturing general information.

**Additional fields:**
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `date` | ISO 8601 date | Creation date | Yes | Current date or extracted from filename |
| `updated` | ISO 8601 date | Last update date | No | None |
| `draft` | Boolean | Whether the note is a draft | No | False |
| `index_page` | Boolean | Whether the note is an index page | No | False |
| `synopsis` | String | Brief summary of the note | No | None |
| `titleimage` | String | Image to display with the title | No | None |
| `via` | Object | Reference to external source | No | None |
| `sidebar` | String | Optional sidebar content | No | None |

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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `author` | List | List of author names | Yes | ["Unknown"] |
| `year` | String | Publication year | Yes | Current year |
| `month` | String/Int | Publication month (three-letter code or number) | No | None |
| `bibtype` | String | Bibliographic type (e.g., "article", "inproceedings") | Yes | "misc" |
| `doi` | String | Digital Object Identifier | No | None |
| `url` | String | URL to the paper | No | None |
| `journal` | String | Journal name (for articles) | For articles | None |
| `booktitle` | String | Conference/book name (for inproceedings/book chapters) | For conferences | None |
| `volume` | String | Volume number | No | None |
| `number` | String | Issue number | No | None |
| `pages` | String | Page range | No | None |
| `publisher` | String | Publisher name | No | None |
| `latest` | Boolean | Whether this is the latest version (computed automatically) | No | False |
| `projects` | List | Related project slugs | No | Empty list |
| `slides` | List | URLs to presentation slides | No | Empty list |
| `bib` | String | BibTeX entry | No | None |
| `tags` | List | Tags for the paper | No | Empty list |
| `keywords` | List | Keywords (treated as additional tags) | No | Empty list |

#### Bibliographic Types

Common bibliographic types (`bibtype`) and their interpretation:
- `article` - Journal article (tagged as "journal")
- `inproceedings` - Conference paper (tagged as "conference")
- `techreport` - Technical report (tagged as "report")
- `misc` - Preprint or miscellaneous (tagged as "preprint")
- `book` - Book or book chapter (tagged as "book")
- `abstract` - Extended abstract (non-standard, but common in research)
- `proceedings` - Complete proceedings (non-standard)
- `poster` - Poster presentation (non-standard)
- `workshop` - Workshop paper (non-standard)

**Note**: Implementations should handle non-standard types gracefully, either by mapping to standard types or using "misc" as a fallback.

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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `level` | String | Academic level (Any, PartII, MPhil, PhD, Postdoc) | Yes | "Any" |
| `project` | String | Associated project slug | Yes | Empty string |
| `status` | String | Status (Available, Discussion, Ongoing, Completed) | Yes | "Available" |
| `year` | Integer/String | Year of idea creation | Yes | Current year |
| `month` | Integer/String | Month of idea creation | Yes | 1 |
| `supervisors` | List | List of supervisor handles | No | Empty list |
| `students` | List | List of student handles | No | Empty list |
| `reading` | String | Suggested reading materials | No | None |

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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `status` | String | Project status (e.g., "active", "completed", "planned") | Yes | "active" |
| `start` | Integer/String | Project start year | Yes | Current year |

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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `names` | List[String] | List of name variations for the contact | Yes | [] |
| `name` | String | Full name of the contact (deprecated - use names instead) | No | First entry from names list |
| `handle` | String | Unique identifier for the contact | Yes | Same as slug |
| `email` | String | Email address | No | None |
| `organization` | String | Organization or company | No | None |
| `website` | String | Website URL | No | None |
| `twitter` | String | Twitter/X handle | No | None |
| `github` | String | GitHub username | No | None |

**Note**: When displaying contacts in lists or user interfaces, implementations MUST use the first entry from the `names` array as the primary display name, not the `title` field. This ensures consistent display of contact names.

Example:
```yaml
---
type: contact
title: Jane Doe
slug: jane-doe
names:
  - Jane Doe
  - Jane M. Doe
  - Dr. Jane Doe
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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `date` | ISO 8601 date | Publication date | Yes | Current date or extracted from filename |
| `source` | String | News source | Yes | Empty string |
| `url` | String | URL to the news article | No | None |
| `slug_ent` | String | Related entity slug | No | None |

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
| Field | Type | Description | Required | Default if Missing |
|-------|------|-------------|----------|-------------------|
| `uuid` | String | Unique identifier (used as filename in UUID-based structure) | Yes | Generated UUID |
| `slug` | String | Short identifier (used as filename in category-based structure) | Yes | Derived from title |
| `url` | String | URL to the video | Yes | Empty string |
| `published_date` | ISO 8601 date | Publication date | Yes | Current date |
| `description` | String | Video description | Yes | Same as title |
| `talk` | Boolean | Whether the video is a talk/presentation | No | False |
| `paper` | String | Related paper slug | No | None |
| `project` | String | Related project slug | No | None |

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

2. **Alternative Structure Forms**
   - **Without data/ subdirectory**: Some databases may organize entries directly under the root, omitting the `data/` prefix. Implementations should detect this by looking for category directories at the root.
   - **UUID-Based Structure**: Typically used for external content sources like PeerTube videos. Flat directory with all entries as UUID-named files (e.g., `053fb1b4-d9e0-4bdc-b119-39326d90f62e.md`). Front matter contains both the UUID and a separate slug field. Without subdirectory categorization, these files need additional metadata to indicate their type.

## Implementation Considerations

When implementing a Bushel-compatible system, consider:

1. **Directory Discovery and Configuration**:
   - Support multiple ways to locate the database directory (args, env vars, defaults)
   - Auto-detect whether entries are in a `data/` subdirectory or directly at the root
   - Provide a clean configuration interface for users to specify paths

2. **Resilient Parsing**:
   - Handle missing required fields with sensible defaults
   - Be flexible with date formats, supporting both ISO dates and separate year/month fields
   - Support both string and numeric months, with conversion between formats
   - Case-insensitive matching for enumerated values (status, level, bibtype)

3. **File Naming and Organization**:
   - Support both category-based and UUID-based naming schemes
   - Extract dates from filenames when appropriate
   - Handle nested directory structures for papers with versions

4. **Relationship Handling**:
   - Build bidirectional indexes for quick cross-reference lookups
   - Support all tag types, including special prefixes
   - Provide filtering and query capabilities based on tags and references

5. **Error Recovery**:
   - Log parsing errors but continue processing when possible
   - Attempt to extract minimal useful data even from malformed entries
   - Provide clear feedback about validation issues

6. **User Interface Considerations**:
   - Show loading progress for large databases
   - Provide a clean CLI with filtering options
   - Support for quiet modes and verbosity controls
   - Rich output formatting when possible

## CLI Interface Recommendations

A Bushel implementation should provide a command-line interface with these features:

1. **Database Specification**:
   ```bash
   bushel --db /path/to/database command [options]
   ```
   - Support for environment variables: `BUSHEL_DB=/path/to/db bushel command`
   - Default to standard locations: `.db/` subdirectory or current directory

2. **Common Commands**:
   - `list`: List entries with filtering and limits
   - `show`: Display details of a specific entry
   - `tags`: List and count tags across entries
   - `slugs`: Show all entry slugs for quick reference
   - `summary`: Display database statistics and overview

3. **Filtering Options**:
   - By entry type: `--type note`
   - By tag: `--tag research`
   - Limiting results: `--limit 50`
   - Sorting: `--sort date` or `--sort name`

By following these recommendations, implementations can provide a consistent and user-friendly experience while maintaining compatibility with the Bushel format specification.

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

## Conclusion

The Bushel file format provides a flexible, Markdown-based system for managing knowledge across different entry types. Its strength lies in its simplicity (files on disk), human-readability, and rich interconnection capabilities through its tagging and reference system.

This specification should enable developers to create compatible implementations in any programming language, allowing interoperability with existing Bushel databases.