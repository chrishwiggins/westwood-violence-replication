#!/usr/bin/env python3
"""
Upload Westwood replication data files to Google Drive.

This script uploads each CSV data file to Google Drive and saves
the resulting URLs to data_urls.json for use in Colab notebooks.

Usage:
    python upload_to_gdrive.py

Requirements:
    - Google Drive API credentials set up (see gdrive-share documentation)
    - The gdrive-share tool from /Users/wiggins/mise/py/gdrive-share
"""

import subprocess
import json
import sys
from pathlib import Path

# Data files to upload (relative to westwood-replication/)
DATA_FILES = [
    "data/study14.csv",      # Studies 1 & 4 (Qualtrics, Jan 2021)
    "data/study25.csv",      # Studies 2 & 5 (Qualtrics, Apr 2021)
    "data/study3.csv",       # Study 3 (YouGov, Nov 2021)
    "data/priorestimates.csv",  # Kalmoe-Mason estimates from media
    "data/newsCoverage2016-2021.csv",  # News coverage data
]

# Path to gdrive-share tool
GDRIVE_SHARE = "/Users/wiggins/mise/py/gdrive-share"

def upload_file(filepath: str, email: str) -> str:
    """
    Upload a file to Google Drive and return the sharing URL.

    Args:
        filepath: Local path to the file
        email: Email to share with (for initial permissions)

    Returns:
        Google Drive URL for the uploaded file
    """
    result = subprocess.run(
        [GDRIVE_SHARE, filepath, email],
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"Error uploading {filepath}:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        return None

    # The URL is printed to stdout
    url = result.stdout.strip()
    return url


def main():
    # Get the repository root (parent of export/)
    repo_root = Path(__file__).parent.parent

    # Ask for email to share with
    email = input("Enter email to share files with: ").strip()
    if not email or "@" not in email:
        print("Invalid email")
        sys.exit(1)

    urls = {}

    for rel_path in DATA_FILES:
        full_path = repo_root / rel_path

        if not full_path.exists():
            print(f"Warning: {full_path} does not exist, skipping")
            continue

        print(f"Uploading {rel_path}...")
        url = upload_file(str(full_path), email)

        if url:
            # Extract file ID from URL for gdown compatibility
            # URL format: https://drive.google.com/file/d/FILE_ID/view
            file_id = url.split("/d/")[1].split("/")[0] if "/d/" in url else None

            urls[Path(rel_path).name] = {
                "url": url,
                "file_id": file_id,
                "gdown_url": f"https://drive.google.com/uc?id={file_id}" if file_id else None
            }
            print(f"  -> {url}")
        else:
            print(f"  -> FAILED")

    # Save URLs to JSON
    output_path = Path(__file__).parent / "data_urls.json"
    with open(output_path, "w") as f:
        json.dump(urls, f, indent=2)

    print(f"\nURLs saved to {output_path}")
    print("\nTo use in Colab, files should be made public:")
    print("1. Go to each URL above")
    print("2. Click Share -> General access -> Anyone with the link")


if __name__ == "__main__":
    main()
