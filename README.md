# JCL Simulation Framework

## Overview
This framework simulates IBM mainframe JCL (Job Control Language) concepts using open-source tools:
- Shell scripts for JCL parsing and execution
- Cron for job scheduling
- File system for dataset management
- COBOL program integration

## Components

### 1. Core Framework
- `jcl_parser.sh` - Parses JCL-like syntax
- `job_executor.sh` - Executes job steps
- `dataset_manager.sh` - Manages datasets (files)
- `scheduler.sh` - Job scheduling interface

### 2. JCL Syntax Support
- JOB statements - Job definition and parameters
- EXEC statements - Program execution
- DD statements - Dataset definitions
- PROC statements - Procedure calls
- IF/THEN/ELSE - Conditional execution

### 3. Dataset Types
- Sequential files (PS)
- VSAM-like indexed files
- Generation Data Groups (GDG) simulation
- Temporary datasets

### 4. Enterprise Features
- Job dependencies
- Return code checking
- SYSOUT capture
- Error handling
- Resource allocation

## Usage Examples

```jcl
//BANKJOB  JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC PGM=BATCH-VALIDATOR
//TRANSIN  DD   DSN=TRANSACTIONS.INPUT,DISP=SHR
//TRANSOUT DD   DSN=TRANSACTIONS.VALIDATED,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
//STEP2    EXEC PGM=ACCOUNT-UPDATE,COND=(0,NE,STEP1)
//ACCOUNTS DD   DSN=ACCOUNTS.MASTER,DISP=SHR
//SYSOUT   DD   SYSOUT=*
```

## Mapping to IBM Concepts

| Open Source | IBM Mainframe |
|-------------|---------------|
| Shell scripts | JCL |
| Cron | TWS/OPC |
| File system | VSAM/SMS |
| Process pipes | QSAM |
| Exit codes | Return codes |
| Log files | SYSOUT |