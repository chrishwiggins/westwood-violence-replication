"""
================================================================================
WESTWOOD ET AL. (2022) REPLICATION - PART 1: DATA LOADING
================================================================================

This script loads the survey data from Google Drive for the replication of:

    Westwood, S. J., Grimmer, J., Tyler, M., & Nall, C. (2022).
    "Current research overstates American support for political violence."
    PNAS, 119(12), e2116870119.

LEARNING OBJECTIVES:
    1. Understand how to load data from Google Drive in Colab
    2. Explore survey data structure
    3. Understand the experimental design (vignettes + engagement checks)

DATA SOURCES:
    - Harvard Dataverse: doi:10.7910/DVN/ZEHO8E
    - Original data collected via Qualtrics and YouGov surveys in 2021

================================================================================
"""

# ==============================================================================
# STEP 1: INSTALL AND IMPORT REQUIRED PACKAGES
# ==============================================================================

# In Colab, we need to install gdown to download from Google Drive
# Uncomment the next line if running in Colab:
# !pip install gdown pandas numpy

import pandas as pd
import numpy as np

# For downloading from Google Drive
import gdown

# ==============================================================================
# STEP 2: DEFINE GOOGLE DRIVE URLs FOR DATA FILES
# ==============================================================================

# These URLs point to the publicly shared data files on Google Drive.
# The format uses gdown-compatible URLs: https://drive.google.com/uc?id=FILE_ID
#
# TO UPDATE THESE URLs:
# 1. Run upload_to_gdrive.py to upload files
# 2. Make files public (Share -> Anyone with link)
# 3. Copy the file IDs here

DATA_URLS = {
    # Studies 1 & 4: Qualtrics panel, January 2021
    # Study 1: Car-ramming vignette (violence against protesters)
    # Study 4: Sentencing task (proposed prison sentences)
    "study14": "https://drive.google.com/uc?id=YOUR_FILE_ID_HERE",

    # Studies 2 & 5: Qualtrics panel, April 2021
    # Study 2: Shooting vignette (violence at political rally)
    # Study 5: Incentive study (in appendix)
    "study25": "https://drive.google.com/uc?id=YOUR_FILE_ID_HERE",

    # Study 3: YouGov nationally representative sample, November 2021
    # Same shooting vignette as Study 2, but with survey weights
    "study3": "https://drive.google.com/uc?id=YOUR_FILE_ID_HERE",

    # Prior estimates: Kalmoe-Mason derived percentages from media coverage
    # Used to show how prior research inflated violence support estimates
    "priorestimates": "https://drive.google.com/uc?id=YOUR_FILE_ID_HERE",
}


# ==============================================================================
# STEP 3: HELPER FUNCTION TO LOAD DATA
# ==============================================================================

def load_from_gdrive(name: str, url: str) -> pd.DataFrame:
    """
    Download a CSV file from Google Drive and load it as a DataFrame.

    Parameters:
    -----------
    name : str
        A descriptive name for the dataset (for error messages)
    url : str
        The gdown-compatible Google Drive URL

    Returns:
    --------
    pd.DataFrame
        The loaded dataset

    Example:
    --------
    >>> df = load_from_gdrive("study1", "https://drive.google.com/uc?id=ABC123")
    """
    print(f"Loading {name}...")

    # gdown.download returns the output filename
    output_file = f"/tmp/{name}.csv"

    try:
        gdown.download(url, output_file, quiet=True)
        df = pd.read_csv(output_file)
        print(f"  Loaded {len(df):,} rows, {len(df.columns)} columns")
        return df
    except Exception as e:
        print(f"  ERROR: Could not load {name}: {e}")
        print(f"  Make sure the file is publicly shared on Google Drive")
        return None


# ==============================================================================
# STEP 4: LOAD ALL DATASETS
# ==============================================================================

def load_all_data():
    """
    Load all datasets needed for the replication.

    Returns a dictionary with all DataFrames.
    """
    data = {}

    for name, url in DATA_URLS.items():
        if "YOUR_FILE_ID" in url:
            print(f"Skipping {name}: URL not configured yet")
            continue
        data[name] = load_from_gdrive(name, url)

    return data


# ==============================================================================
# STEP 5: EXPLORE DATA STRUCTURE
# ==============================================================================

def explore_dataset(df: pd.DataFrame, name: str):
    """
    Print summary statistics for a dataset.

    This helps us understand:
    - How many respondents are in each study
    - What variables are available
    - Basic distributions of key variables
    """
    print(f"\n{'='*60}")
    print(f"DATASET: {name}")
    print(f"{'='*60}")

    print(f"\nShape: {df.shape[0]:,} rows x {df.shape[1]} columns")

    print(f"\nColumn names:")
    for i, col in enumerate(df.columns):
        print(f"  {i+1:2d}. {col}")

    print(f"\nFirst 3 rows:")
    print(df.head(3).to_string())


# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

if __name__ == "__main__":
    print("="*60)
    print("WESTWOOD ET AL. (2022) REPLICATION - DATA LOADING")
    print("="*60)

    # Load all datasets
    data = load_all_data()

    # Explore each dataset
    for name, df in data.items():
        if df is not None:
            explore_dataset(df, name)

    print("\n" + "="*60)
    print("DATA LOADING COMPLETE")
    print("="*60)
    print("\nNext steps:")
    print("  1. Run 02_preprocess.py to clean and recode variables")
    print("  2. Run 03_core_analysis.py to replicate main findings")
