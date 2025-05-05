#\!/usr/bin/env python3
"""
Bushel - A knowledge management system for notes, papers, ideas, projects, contacts, news, and videos.
Python implementation of the Bushel file format.
"""

import os
import sys
import re
import yaml
import glob
import typer
from enum import Enum
from pathlib import Path
from datetime import datetime, date
from typing import List, Dict, Optional, Any, Union, Tuple
from pydantic import BaseModel, field_validator, model_validator, ConfigDict
from rich.console import Console
from rich.table import Table
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich import print as rprint

# Global app state for configuration
class AppState:
    def __init__(self):
        self.quiet = False
        self.console = Console()
        
    def print(self, message):
        if not self.quiet:
            rprint(message)

app_state = AppState()

# Enums for entry types and specialized fields
class EntryType(str, Enum):
    NOTE = "note"
    PAPER = "paper"
    IDEA = "idea"
    PROJECT = "project"
    CONTACT = "contact"
    NEWS = "news"
    VIDEO = "video"

class IdeaLevel(str, Enum):
    ANY = "Any"
    PART_II = "PartII" 
    MPHIL = "MPhil"
    PHD = "PhD"
    POSTDOC = "Postdoc"

class IdeaStatus(str, Enum):
    AVAILABLE = "Available"
    DISCUSSION = "Discussion"
    ONGOING = "Ongoing"
    COMPLETED = "Completed"

class ProjectStatus(str, Enum):
    ACTIVE = "active"
    COMPLETED = "completed"
    PLANNED = "planned"

class BibType(str, Enum):
    ARTICLE = "article"
    INPROCEEDINGS = "inproceedings"
    TECHREPORT = "techreport"
    MISC = "misc"
    BOOK = "book"
    ABSTRACT = "abstract"
    PROCEEDINGS = "proceedings"
    POSTER = "poster"
    WORKSHOP = "workshop"

# Base entry model with common fields
class BaseEntry(BaseModel):
    """Base model for all entry types."""
    title: Optional[str] = ""
    slug: Optional[str] = ""
    tags: List[str] = []
    body: str = ""
    type: EntryType
    
    model_config = ConfigDict(extra="allow")
    
    def model_post_init(self, __context):
        """Ensure title and slug are set."""
        # Use slug as title if title is missing
        if not self.title and self.slug:
            self.title = self.slug.replace('-', ' ').title()
        # Use title as slug if slug is missing
        if not self.slug and self.title:
            self.slug = self.title.lower().replace(' ', '-')
    
# Note entry model
class Note(BaseEntry):
    """Note entry type"""
    type: EntryType = EntryType.NOTE
    date: Optional[date] = None
    updated: Optional[date] = None
    draft: bool = False
    index_page: bool = False
    synopsis: Optional[str] = None
    titleimage: Optional[str] = None
    via: Optional[dict] = None
    sidebar: Optional[str] = None
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if self.date is None:
            self.date = date.today()
        return self

# Paper entry model
class Paper(BaseEntry):
    """Paper entry type"""
    type: EntryType = EntryType.PAPER
    author: List[str] = []
    year: Union[str, int] = ""
    month: Optional[Union[str, int]] = None
    bibtype: Optional[BibType] = None
    doi: Optional[str] = None
    url: Optional[str] = None
    journal: Optional[str] = None
    booktitle: Optional[str] = None
    volume: Optional[str] = None
    number: Optional[str] = None
    pages: Optional[str] = None
    publisher: Optional[str] = None
    latest: bool = False
    projects: List[str] = []
    slides: List[str] = []
    bib: Optional[str] = None
    keywords: List[str] = []
    ver: str = "v1"
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if not self.author:
            self.author = ["Unknown"]
        if not self.year:
            self.year = date.today().year
        if not self.bibtype:
            self.bibtype = BibType.MISC
        return self
    
    @field_validator('bibtype', mode='before')
    @classmethod
    def validate_bibtype(cls, v):
        """Allow case-insensitive bibtype matching"""
        if v is None:
            return None
        if isinstance(v, str):
            try:
                return BibType(v.lower())
            except ValueError:
                # For non-standard types, fall back to misc
                return BibType.MISC
        return v
    
    @field_validator('month', mode='before')
    @classmethod
    def validate_month(cls, v):
        """Convert month strings to integers"""
        if v is None:
            return None
        
        month_map = {
            "jan": 1, "feb": 2, "mar": 3, "apr": 4, 
            "may": 5, "jun": 6, "jul": 7, "aug": 8,
            "sep": 9, "oct": 10, "nov": 11, "dec": 12
        }
        
        if isinstance(v, str):
            # Try to match month name
            v_lower = v.lower()
            if v_lower in month_map:
                return month_map[v_lower]
            # Try to parse as integer
            try:
                return int(v)
            except ValueError:
                return 1  # Default to January if unparseable
        return v

# Idea entry model
class Idea(BaseEntry):
    """Idea entry type"""
    type: EntryType = EntryType.IDEA
    level: IdeaLevel = IdeaLevel.ANY
    project: str = ""
    status: IdeaStatus = IdeaStatus.AVAILABLE
    year: Union[str, int] = ""
    month: Union[str, int] = 1
    supervisors: List[str] = []
    students: List[str] = []
    reading: Optional[str] = None
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if not self.year:
            self.year = date.today().year
        # Ensure year is int
        if isinstance(self.year, str):
            try:
                self.year = int(self.year)
            except ValueError:
                self.year = date.today().year
        # Ensure month is int
        if isinstance(self.month, str):
            try:
                self.month = int(self.month)
            except ValueError:
                self.month = 1
        return self
    
    @field_validator('level', mode='before')
    @classmethod
    def validate_level(cls, v):
        """Allow case-insensitive level matching"""
        if isinstance(v, str):
            v_lower = v.lower()
            if v_lower == "any":
                return IdeaLevel.ANY
            elif v_lower == "partii":
                return IdeaLevel.PART_II
            elif v_lower == "mphil":
                return IdeaLevel.MPHIL
            elif v_lower == "phd":
                return IdeaLevel.PHD
            elif v_lower == "postdoc":
                return IdeaLevel.POSTDOC
        return v
    
    @field_validator('status', mode='before')
    @classmethod
    def validate_status(cls, v):
        """Allow case-insensitive status matching"""
        if isinstance(v, str):
            v_lower = v.lower()
            if v_lower == "available":
                return IdeaStatus.AVAILABLE
            elif v_lower == "discussion":
                return IdeaStatus.DISCUSSION
            elif v_lower == "ongoing":
                return IdeaStatus.ONGOING
            elif v_lower == "completed":
                return IdeaStatus.COMPLETED
        return v

# Project entry model
class Project(BaseEntry):
    """Project entry type"""
    type: EntryType = EntryType.PROJECT
    status: ProjectStatus = ProjectStatus.ACTIVE
    start: Union[str, int] = ""
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if not self.start:
            self.start = date.today().year
        # Ensure start is int
        if isinstance(self.start, str):
            try:
                self.start = int(self.start)
            except ValueError:
                self.start = date.today().year
        return self
    
    @field_validator('status', mode='before')
    @classmethod
    def validate_status(cls, v):
        """Allow case-insensitive status matching"""
        if isinstance(v, str):
            v_lower = v.lower()
            if v_lower == "active":
                return ProjectStatus.ACTIVE
            elif v_lower == "completed":
                return ProjectStatus.COMPLETED
            elif v_lower == "planned":
                return ProjectStatus.PLANNED
        return v

# Contact entry model
class Contact(BaseEntry):
    """Contact entry type"""
    type: EntryType = EntryType.CONTACT
    names: List[str] = []
    name: Optional[str] = None
    handle: Optional[str] = None
    email: Optional[str] = None
    organization: Optional[str] = None
    website: Optional[str] = None
    twitter: Optional[str] = None
    github: Optional[str] = None
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        # Set name from names array
        if self.names and not self.name:
            self.name = self.names[0]
        # Fallback if no names or name set
        if not self.name:
            self.name = self.title
        if not self.handle:
            self.handle = self.slug
        return self

# News entry model
class News(BaseEntry):
    """News entry type"""
    type: EntryType = EntryType.NEWS
    date: Optional[date] = None
    source: str = ""
    url: Optional[str] = None
    slug_ent: Optional[str] = None
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if self.date is None:
            self.date = date.today()
        return self

# Video entry model
class Video(BaseEntry):
    """Video entry type"""
    type: EntryType = EntryType.VIDEO
    uuid: Optional[str] = None
    url: str = ""
    published_date: Optional[date] = None
    description: Optional[str] = None
    talk: bool = False
    paper: Optional[str] = None
    project: Optional[str] = None
    
    @model_validator(mode='after')
    def set_defaults(self):
        """Set default values for required fields"""
        if not self.description:
            self.description = self.title
        if self.published_date is None:
            self.published_date = date.today()
        return self

# Tag handling
class Tag:
    """Representation of a Bushel tag"""
    
    def __init__(self, tag_str: str):
        """Parse a tag string into a structured tag"""
        self.raw = tag_str
        
        if not tag_str or len(tag_str) < 1:
            self.type = "text"
            self.value = tag_str
            return
            
        # Check for special prefixes
        if tag_str[0] == ':':
            self.type = "slug"
            self.value = tag_str[1:]
        elif tag_str[0] == '@':
            self.type = "contact"
            self.value = tag_str[1:]
        elif tag_str[0] == '#':
            self.type = "set"
            self.value = tag_str[1:]
        else:
            # Check for year
            try:
                year = int(tag_str)
                if 1900 <= year <= 2100:
                    self.type = "year"
                    self.value = year
                else:
                    self.type = "text"
                    self.value = tag_str
            except ValueError:
                self.type = "text"
                self.value = tag_str
    
    def __str__(self):
        """Convert tag back to string representation"""
        if self.type == "slug":
            return f":{self.value}"
        elif self.type == "contact":
            return f"@{self.value}"
        elif self.type == "set":
            return f"#{self.value}"
        else:
            return str(self.value)

# File loading and parsing functions

def read_markdown_file(file_path: Path) -> Tuple[dict, str]:
    """
    Read a markdown file with YAML front matter.
    Returns a tuple of (front_matter, body).
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Parse YAML front matter
    front_matter_match = re.match(r'^---\s+(.*?)\s+---\s*(.*)', content, re.DOTALL)
    
    if front_matter_match:
        front_matter_str, body = front_matter_match.groups()
        try:
            front_matter = yaml.safe_load(front_matter_str)
            if front_matter is None:  # Empty front matter
                front_matter = {}
        except yaml.YAMLError:
            front_matter = {}
            app_state.print(f"[yellow]Warning: Failed to parse front matter in {file_path}[/yellow]")
    else:
        front_matter = {}
        body = content
    
    return front_matter, body.strip()

def extract_date_from_filename(file_path: Path) -> Optional[date]:
    """
    Extract date from filename format YYYY-MM-DD-slug.md
    Returns None if the filename doesn't match the pattern.
    """
    pattern = r'^(\d{4})-(\d{2})-(\d{2})-(.*)'
    match = re.match(pattern, file_path.stem)
    
    if match:
        try:
            year, month, day = map(int, match.groups()[:3])
            return date(year, month, day)
        except (ValueError, TypeError):
            return None
    return None

def load_note(file_path: Path) -> Note:
    """Load a Note entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Extract date from filename if present
    filename_date = extract_date_from_filename(file_path)
    if filename_date and 'date' not in front_matter:
        front_matter['date'] = filename_date
    
    # Build note object
    note_data = {**front_matter, 'body': body}
    return Note(**note_data)

def load_paper(file_path: Path) -> Paper:
    """Load a Paper entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Check if this is in a versioned directory
    slug = file_path.parent.name
    ver = file_path.stem  # Assumes v1, v2, etc.
    
    # Build paper object with versioning info
    paper_data = {**front_matter, 'body': body, 'slug': slug, 'ver': ver}
    return Paper(**paper_data)

def load_idea(file_path: Path) -> Idea:
    """Load an Idea entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Slug from filename if not specified
    if 'slug' not in front_matter:
        front_matter['slug'] = file_path.stem
    
    # Build idea object
    idea_data = {**front_matter, 'body': body}
    return Idea(**idea_data)

def load_project(file_path: Path) -> Project:
    """Load a Project entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Slug from filename if not specified
    if 'slug' not in front_matter:
        front_matter['slug'] = file_path.stem
    
    # Build project object
    project_data = {**front_matter, 'body': body}
    return Project(**project_data)

def load_contact(file_path: Path) -> Contact:
    """Load a Contact entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Slug/handle from filename if not specified
    if 'slug' not in front_matter:
        front_matter['slug'] = file_path.stem
    if 'handle' not in front_matter:
        front_matter['handle'] = file_path.stem
    
    # Build contact object
    contact_data = {**front_matter, 'body': body}
    return Contact(**contact_data)

def load_news(file_path: Path) -> News:
    """Load a News entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Extract date from filename if present
    filename_date = extract_date_from_filename(file_path)
    if filename_date and 'date' not in front_matter:
        front_matter['date'] = filename_date
    
    # Slug from filename if not specified
    if 'slug' not in front_matter:
        front_matter['slug'] = file_path.stem.split('-', 3)[-1] if filename_date else file_path.stem
    
    # Build news object
    news_data = {**front_matter, 'body': body}
    return News(**news_data)

def load_video(file_path: Path) -> Video:
    """Load a Video entry from a markdown file."""
    front_matter, body = read_markdown_file(file_path)
    
    # Slug and UUID handling
    if 'uuid' not in front_matter and file_path.stem.replace('-', '').isalnum():
        # Filename might be a UUID
        front_matter['uuid'] = file_path.stem
    if 'slug' not in front_matter:
        front_matter['slug'] = file_path.stem
    
    # Build video object
    video_data = {**front_matter, 'body': body}
    return Video(**video_data)

def load_entries_from_dir(
    dir_path: Path, 
    load_func: callable,
    entry_type: str = "entry"
) -> List[Any]:
    """Load all entries of a specific type from a directory."""
    entries = []
    error_count = 0
    
    # Skip if directory doesn't exist
    if not dir_path.exists():
        return entries
    
    # Load all markdown files
    for file_path in dir_path.glob("*.md"):
        try:
            # Try the standard loading function
            entry = load_func(file_path)
            entries.append(entry)
        except Exception as e:
            # On failure, try a more permissive approach
            try:
                # Basic minimal loading with just title and slug
                front_matter, body = read_markdown_file(file_path)
                
                # Get the mapping class based on entry_type
                entry_class = {
                    "note": Note,
                    "paper": Paper,
                    "idea": Idea,
                    "project": Project,
                    "contact": Contact,
                    "news": News,
                    "video": Video
                }.get(entry_type)
                
                if entry_class:
                    # Extract minimal data and create a basic entry
                    title = front_matter.get("title", file_path.stem)
                    slug = front_matter.get("slug", file_path.stem)
                    tags = front_matter.get("tags", [])
                    
                    # Create minimal entry
                    entry = entry_class(title=title, slug=slug, tags=tags, body=body)
                    entries.append(entry)
                    continue  # Skip error logging if we recovered
            except Exception as recovery_err:
                # If recovery also fails, continue with normal error reporting
                pass
                
            # Log the original error
            error_count += 1
            if error_count <= 5:  # Limit error messages to avoid flooding
                app_state.print(f"[yellow]Warning: Failed to load {entry_type} {file_path}: {e}[/yellow]")
            elif error_count == 6:
                app_state.print(f"[yellow]Warning: Additional {entry_type} loading errors suppressed...[/yellow]")
    
    if entries:
        app_state.print(f"[green]Loaded {len(entries)} {entry_type}s[/green]")
    
    return entries

def load_papers_from_dir(dir_path: Path) -> List[Paper]:
    """Load papers from their versioned directory structure."""
    papers = []
    
    # Skip if directory doesn't exist
    if not dir_path.exists():
        return papers
    
    # Find all paper directories
    for paper_dir in dir_path.iterdir():
        if paper_dir.is_dir():
            slug = paper_dir.name
            versions = []
            
            # Load all versions of this paper
            for version_file in paper_dir.glob("*.md"):
                try:
                    paper = load_paper(version_file)
                    paper.slug = slug
                    versions.append(paper)
                except Exception as e:
                    app_state.print(f"[yellow]Warning: Failed to load paper {version_file}: {e}[/yellow]")
            
            # Sort versions and mark latest
            if versions:
                versions.sort(key=lambda p: p.ver)
                latest = versions[-1]
                latest.latest = True
                papers.extend(versions)
    
    if papers:
        app_state.print(f"[green]Loaded {len(papers)} papers[/green]")
    
    return papers

# Database loading and entry management

class BushelDatabase:
    """Manages a Bushel database of entries."""
    
    def __init__(self, base_path: Path):
        """Initialize database with path."""
        self.base_path = base_path
        self.data_path = base_path / "data" if (base_path / "data").exists() else base_path
        
        # Entry collections
        self.notes = []
        self.papers = []
        self.ideas = []
        self.projects = []
        self.contacts = []
        self.news = []
        self.videos = []
        
        # Index for looking up entries by slug
        self.slug_index = {}
        
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=app_state.console,
            disable=app_state.quiet
        ) as progress:
            # Load data files
            progress.add_task("Loading papers...", total=1)
            self.papers = load_papers_from_dir(self.data_path / "papers")
            
            progress.add_task("Loading notes...", total=1)
            self.notes = load_entries_from_dir(self.data_path / "notes", load_note, "note")
            
            progress.add_task("Loading ideas...", total=1)
            self.ideas = load_entries_from_dir(self.data_path / "ideas", load_idea, "idea")
            
            progress.add_task("Loading projects...", total=1)
            self.projects = load_entries_from_dir(self.data_path / "projects", load_project, "project")
            
            progress.add_task("Loading contacts...", total=1)
            self.contacts = load_entries_from_dir(self.data_path / "contacts", load_contact, "contact")
            
            progress.add_task("Loading news...", total=1)
            self.news = load_entries_from_dir(self.data_path / "news", load_news, "news")
            
            progress.add_task("Loading videos...", total=1)
            self.videos = load_entries_from_dir(self.data_path / "videos", load_video, "video")
            
            # Build lookup index
            progress.add_task("Building indices...", total=1)
            self._build_index()
    
    def _build_index(self):
        """Build lookup index for all entries."""
        for entry in self.all_entries():
            if entry.slug:
                self.slug_index[entry.slug] = entry
    
    def all_entries(self):
        """Get all entries in the database."""
        return [
            *self.notes,
            *[p for p in self.papers if p.latest],  # Only latest paper versions
            *self.ideas,
            *self.projects,
            *self.contacts,
            *self.videos
        ]
    
    def lookup(self, slug: str) -> Optional[BaseEntry]:
        """Look up an entry by slug."""
        return self.slug_index.get(slug)
    
    def get_entries_by_type(self, entry_type: EntryType) -> List[BaseEntry]:
        """Get all entries of a specific type."""
        type_map = {
            EntryType.NOTE: self.notes,
            EntryType.PAPER: [p for p in self.papers if p.latest],
            EntryType.IDEA: self.ideas,
            EntryType.PROJECT: self.projects,
            EntryType.CONTACT: self.contacts,
            EntryType.NEWS: self.news,
            EntryType.VIDEO: self.videos
        }
        return type_map.get(entry_type, [])
    
    def get_entries_by_tag(self, tag: str) -> List[BaseEntry]:
        """Get all entries with a specific tag."""
        return [entry for entry in self.all_entries() if tag in entry.tags]
    
    def summary(self) -> Dict[str, int]:
        """Get a summary of the database contents."""
        return {
            "notes": len(self.notes),
            "papers": len([p for p in self.papers if p.latest]),
            "ideas": len(self.ideas),
            "projects": len(self.projects),
            "contacts": len(self.contacts),
            "news": len(self.news),
            "videos": len(self.videos),
            "total": len(self.all_entries())
        }

# Path handling helpers

def get_default_db_path() -> Path:
    """Get the default database path based on environment variables or default locations."""
    # First, check for BUSHEL_DB environment variable
    env_path = os.environ.get("BUSHEL_DB")
    if env_path:
        return Path(env_path)
    
    # Second, check for .db directory in current directory
    default_path = Path.cwd() / ".db"
    if default_path.exists() and default_path.is_dir():
        return default_path
    
    # Finally, fall back to current directory
    return Path.cwd()

# CLI application

app = typer.Typer(help="Bushel - A knowledge management system")

@app.callback()
def main(
    db: Optional[Path] = typer.Option(
        None, 
        "--db", 
        help="Path to the Bushel database directory"
    ),
    quiet: bool = typer.Option(
        False,
        "--quiet",
        "-q",
        help="Suppress progress and warning messages"
    )
):
    """Bushel - A knowledge management system for notes, papers, ideas, projects, contacts, news, and videos."""
    # Set app state
    app_state.quiet = quiet
    
    # Configure global database path
    if not hasattr(main, "db_path"):
        main.db_path = db if db else get_default_db_path()

@app.command()
def list(
    type: Optional[str] = typer.Option(
        None, 
        "--type", 
        "-t", 
        help="Filter by entry type"
    ),
    tag: Optional[str] = typer.Option(
        None, 
        "--tag", 
        help="Filter by tag"
    ),
    limit: int = typer.Option(
        0, 
        "--limit", 
        "-l", 
        help="Limit the number of results"
    )
):
    """List entries in the database."""
    db = BushelDatabase(main.db_path)
    
    # Apply filters
    if type:
        try:
            entry_type = EntryType(type)
            entries = db.get_entries_by_type(entry_type)
        except ValueError:
            app_state.print(f"[red]Invalid entry type: {type}[/red]")
            entries = []
    elif tag:
        entries = db.get_entries_by_tag(tag)
    else:
        entries = db.all_entries()
    
    # Sort entries by type and title
    entries.sort(key=lambda e: (e.type.value, e.title))
    
    # Apply limit
    if limit > 0:
        entries = entries[:limit]
    
    # Display results
    table = Table(title=f"Bushel Entries ({len(entries)})")
    table.add_column("Type", style="cyan")
    table.add_column("Title", style="green")
    table.add_column("Slug", style="blue")
    
    for entry in entries:
        # For contacts, use the name instead of title
        if entry.type == EntryType.CONTACT:
            # Display name from the first entry in names list or fallback to name field
            if hasattr(entry, 'names') and entry.names:
                display_title = entry.names[0]
            elif entry.name:
                display_title = entry.name
            else:
                display_title = entry.title
        else:
            display_title = entry.title
        table.add_row(entry.type.value, display_title, entry.slug)
    
    app_state.console.print(table)

@app.command()
def show(
    slug: str = typer.Argument(..., help="Slug of the entry to show")
):
    """Show details of a specific entry."""
    db = BushelDatabase(main.db_path)
    entry = db.lookup(slug)
    
    if not entry:
        app_state.print(f"[red]Entry not found: {slug}[/red]")
        return
    
    # Display entry details
    app_state.console.print(f"[bold green]{entry.title}[/bold green] ({entry.type.value})")
    app_state.console.print(f"[blue]Slug:[/blue] {entry.slug}")
    
    # Display type-specific fields
    if entry.type == EntryType.NOTE:
        app_state.console.print(f"[blue]Date:[/blue] {entry.date}")
        if entry.updated:
            app_state.console.print(f"[blue]Updated:[/blue] {entry.updated}")
    elif entry.type == EntryType.PAPER:
        app_state.console.print(f"[blue]Authors:[/blue] {', '.join(entry.author)}")
        app_state.console.print(f"[blue]Year:[/blue] {entry.year}")
        app_state.console.print(f"[blue]Type:[/blue] {entry.bibtype.value}")
    elif entry.type == EntryType.IDEA:
        app_state.console.print(f"[blue]Level:[/blue] {entry.level.value}")
        app_state.console.print(f"[blue]Status:[/blue] {entry.status.value}")
        app_state.console.print(f"[blue]Project:[/blue] {entry.project}")
    elif entry.type == EntryType.PROJECT:
        app_state.console.print(f"[blue]Status:[/blue] {entry.status.value}")
        app_state.console.print(f"[blue]Start:[/blue] {entry.start}")
    elif entry.type == EntryType.CONTACT:
        if hasattr(entry, 'names') and entry.names:
            app_state.console.print(f"[blue]Name:[/blue] {entry.names[0]}")
            if len(entry.names) > 1:
                app_state.console.print(f"[blue]Alternate Names:[/blue] {', '.join(entry.names[1:])}")
        elif entry.name:
            app_state.console.print(f"[blue]Name:[/blue] {entry.name}")
        app_state.console.print(f"[blue]Handle:[/blue] {entry.handle}")
        if entry.email:
            app_state.console.print(f"[blue]Email:[/blue] {entry.email}")
    elif entry.type == EntryType.NEWS:
        app_state.console.print(f"[blue]Date:[/blue] {entry.date}")
        app_state.console.print(f"[blue]Source:[/blue] {entry.source}")
        if entry.url:
            app_state.console.print(f"[blue]URL:[/blue] {entry.url}")
    elif entry.type == EntryType.VIDEO:
        app_state.console.print(f"[blue]URL:[/blue] {entry.url}")
        app_state.console.print(f"[blue]Published:[/blue] {entry.published_date}")
    
    # Display tags
    if entry.tags:
        app_state.console.print(f"\n[blue]Tags:[/blue] {', '.join(entry.tags)}")
    
    # Display content
    if entry.body:
        app_state.console.print("\n[bold]Content:[/bold]")
        app_state.console.print(entry.body[:500] + ("..." if len(entry.body) > 500 else ""))

@app.command()
def tags():
    """List all tags in the database."""
    db = BushelDatabase(main.db_path)
    
    # Collect tags with counts
    tag_counts = {}
    for entry in db.all_entries():
        for tag in entry.tags:
            tag_counts[tag] = tag_counts.get(tag, 0) + 1
    
    # Sort by count descending
    sorted_tags = sorted(tag_counts.items(), key=lambda x: (-x[1], x[0]))
    
    # Display results
    table = Table(title=f"Tags ({len(sorted_tags)})")
    table.add_column("Tag", style="green")
    table.add_column("Count", style="cyan", justify="right")
    
    for tag, count in sorted_tags:
        table.add_row(tag, str(count))
    
    app_state.console.print(table)

@app.command()
def slugs():
    """List all slugs in the database."""
    db = BushelDatabase(main.db_path)
    
    # Collect slugs by type
    slug_by_type = {}
    for entry in db.all_entries():
        if entry.type.value not in slug_by_type:
            slug_by_type[entry.type.value] = []
        slug_by_type[entry.type.value].append(entry.slug)
    
    # Sort types and slugs
    for type_name in slug_by_type:
        slug_by_type[type_name].sort()
    
    # Display results
    for type_name in sorted(slug_by_type.keys()):
        slugs = slug_by_type[type_name]
        app_state.console.print(f"[bold cyan]{type_name}s[/bold cyan] ({len(slugs)})")
        
        # Display in columns
        columns = 3
        for i in range(0, len(slugs), columns):
            row = slugs[i:i+columns]
            app_state.console.print("  " + "  ".join(f"[blue]{slug}[/blue]".ljust(30) for slug in row))
        
        app_state.console.print()

@app.command()
def summary():
    """Display a summary of the database."""
    db = BushelDatabase(main.db_path)
    stats = db.summary()
    
    app_state.console.print(f"[bold]Bushel Database at {main.db_path}[/bold]\n")
    
    table = Table(title="Database Summary")
    table.add_column("Type", style="cyan")
    table.add_column("Count", style="green", justify="right")
    
    for entry_type, count in stats.items():
        if entry_type != "total":
            table.add_row(entry_type.title(), str(count))
    
    table.add_row("Total", str(stats["total"]), style="bold")
    
    app_state.console.print(table)

if __name__ == "__main__":
    app()
