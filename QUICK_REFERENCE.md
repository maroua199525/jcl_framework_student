# Quick Reference Card

## Essential Commands

### Framework Management
```bash
./demo.sh                          # Run complete demo
./scheduler.sh init                 # Initialize scheduler
./dataset_manager.sh init          # Initialize dataset manager
```

### Dataset Operations
```bash
./dataset_manager.sh allocate NAME TYPE SIZE RECORDS
./dataset_manager.sh list [pattern]
./dataset_manager.sh copy SOURCE TARGET
./dataset_manager.sh delete NAME
```

### Job Operations
```bash
./scheduler.sh submit JCL_FILE [NAME] [PRIORITY]
./scheduler.sh status
./scheduler.sh process
./jcl_parser.sh JCL_FILE           # Parse JCL directly
```

### Student Exercise
```bash
./test_student_exercise.sh         # Test exercise setup
```

## JCL Syntax Quick Reference

```jcl
//JOBNAME  JOB  CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=PROGRAM-NAME
//INFILE   DD   DSN=INPUT.DATASET,DISP=SHR
//OUTFILE  DD   DSN=OUTPUT.DATASET,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
```

## Common Dataset Types

- **PS** - Sequential file
- **VSAM** - Indexed file
- **GDG** - Generation Data Group

## File Locations

- **Jobs:** `jobs/*.jcl`
- **Programs:** `programs/*.cbl`
- **Data:** `datasets/`
- **Logs:** `history/`
