# Datasets Directory

This directory contains your allocated datasets and simulates IBM mainframe dataset storage.

## Directory Structure

- **master_catalog.dat** - Central catalog of all allocated datasets
- **gdg/** - Generation Data Groups (versioned datasets)
- **vsam/** - VSAM (Virtual Storage Access Method) files
- **temp/** - Temporary datasets

## Usage

Use the dataset manager to work with datasets:

```bash
# Allocate a new dataset
./dataset_manager.sh allocate MY.DATA.SET PS 2048 100

# List all datasets
./dataset_manager.sh list

# Copy datasets
./dataset_manager.sh copy SOURCE.DATA TARGET.DATA

# Delete datasets
./dataset_manager.sh delete OLD.DATA
```

## Student Exercise

For the student exercise, you'll work with:
- STUDENT.INPUT.DATA - Your input dataset
- STUDENT.OUTPUT.DATA - Your output dataset

These will be created automatically when you run the exercise.
