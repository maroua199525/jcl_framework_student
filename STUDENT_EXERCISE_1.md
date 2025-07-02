# Student Exercise 1: File Processing with JCL and COBOL

## 🎯 Learning Objectives

After completing this exercise, you will be able to:
- Write a COBOL program that reads and writes files
- Create JCL scripts to manage datasets and execute programs
- Understand the relationship between JCL DD statements and COBOL file handling
- Use the JCL framework to simulate enterprise batch processing

## 📋 Exercise Overview

You're now ready to go beyond printing to the screen! We will use JCL to manage datasets and process files in a realistic batch environment.

**Your Mission:** Create a complete batch processing solution that reads data from an input file, processes it, and writes results to an output file.

## 🔧 What You Need to Build

### Part 1: COBOL Program
Write a COBOL program that:
- Reads lines from an input file called `INFILE`
- Writes those lines unchanged to an output file called `OUTFILE`
- Handles end-of-file conditions properly
- Provides status messages during processing

### Part 2: JCL Script
Create a JCL script that:
- Allocates a temporary input dataset with sample data
- Allocates a temporary output dataset
- Compiles the COBOL program (simulated)
- Executes the program
- Displays the contents of the output dataset in the job output

## 📝 Step-by-Step Instructions

### Step 1: Set Up Your Environment

```bash
# Navigate to the JCL framework directory
cd jcl_framework

# Create a programs directory for your COBOL code
mkdir -p programs

# Create a data directory for input files
mkdir -p data
```

### Step 2: Create the COBOL Program

Create a file called `programs/file_processor.cbl` with the following structure:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-PROCESSOR.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "INFILE"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  INPUT-RECORD            PIC X(80).
       
       FD  OUTFILE.
       01  OUTPUT-RECORD           PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       01  WS-RECORD-COUNT         PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting file processing..."
           
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           
           PERFORM READ-AND-WRITE-LOOP
               UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE INFILE
           CLOSE OUTFILE
           
           DISPLAY "Processing completed. Records processed: " 
               WS-RECORD-COUNT
           
           STOP RUN.
       
       READ-AND-WRITE-LOOP.
           READ INFILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
                   MOVE INPUT-RECORD TO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
           END-READ.
```

### Step 3: Create Sample Input Data

Create a file called `data/sample_input.txt` with some test data:

```
CUSTOMER001,John Smith,New York,Active
CUSTOMER002,Jane Doe,California,Active  
CUSTOMER003,Bob Johnson,Texas,Inactive
CUSTOMER004,Alice Brown,Florida,Active
CUSTOMER005,Charlie Wilson,Illinois,Active
```

### Step 4: Create the JCL Script

Create a file called `jobs/file_processing.jcl`:

```jcl
//FILEPROC JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* File Processing Exercise - Student Assignment
//* Demonstrates file I/O with COBOL and JCL dataset management
//*
//STEP1    EXEC PGM=FILE-PROCESSOR
//INFILE   DD   DSN=STUDENT.INPUT.DATA,DISP=SHR
//OUTFILE  DD   DSN=STUDENT.OUTPUT.DATA,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
//*
//STEP2    EXEC PGM=DISPLAY-OUTPUT
//DATASET  DD   DSN=STUDENT.OUTPUT.DATA,DISP=SHR
//SYSOUT   DD   SYSOUT=*
```

### Step 5: Test Your Solution

1. **Allocate the input dataset:**
```bash
./dataset_manager.sh allocate STUDENT.INPUT.DATA PS 1024 10
```

2. **Copy your sample data to the dataset:**
```bash
cp data/sample_input.txt datasets/student_input_data.dat
```

3. **Submit your job:**
```bash
./scheduler.sh submit jobs/file_processing.jcl
```

4. **Check the results:**
```bash
./scheduler.sh status
```

## 🎯 Expected Results

When your solution works correctly, you should see:

1. **Job submission confirmation**
2. **COBOL program execution messages**
3. **Record count display**
4. **Output file creation**
5. **Contents of output file displayed**

## 💡 Hints and Tips

### COBOL Programming Hints:
- Use `ORGANIZATION IS LINE SEQUENTIAL` for text files
- Always check for `AT END` condition when reading files
- Remember to `OPEN` files before using and `CLOSE` them when done
- Use meaningful variable names in WORKING-STORAGE

### JCL Hints:
- Use `//DD` statements to define your input and output datasets
- `DISP=SHR` means share an existing dataset
- `DISP=(NEW,CATLG)` creates a new dataset and catalogs it
- `SYSOUT=*` sends output to the job log

### Framework-Specific Tips:
- The framework simulates COBOL compilation automatically
- Dataset names are converted to file paths automatically
- Check the `datasets/` directory to see your actual files
- Use `./dataset_manager.sh list` to see all allocated datasets

## 🔍 Troubleshooting Guide

### Common Issues:

**Problem:** "Dataset not found"
**Solution:** Make sure you allocated the dataset first using `dataset_manager.sh`

**Problem:** "COBOL program not found"
**Solution:** Ensure your program is in the `programs/` directory with the correct name

**Problem:** "Permission denied"
**Solution:** Make sure all scripts are executable: `chmod +x *.sh`

**Problem:** "Empty output file"
**Solution:** Check that your input file has data and the COBOL logic is correct

## 🚀 Extension Challenges

Once you complete the basic exercise, try these advanced challenges:

### Challenge 1: Data Validation
Modify your COBOL program to:
- Count valid vs invalid records
- Write invalid records to a separate error file
- Display statistics at the end

### Challenge 2: Multiple Step Processing
Create a JCL job with multiple steps:
- Step 1: Validate input data
- Step 2: Process valid records
- Step 3: Generate summary report
- Step 4: Clean up temporary files

### Challenge 3: Conditional Processing
Add conditional execution to your JCL:
- Only run Step 2 if Step 1 succeeds (RC=0)
- Skip error processing if no errors found
- Use `COND` parameters effectively

### Challenge 4: Modern Integration
Enhance your solution with modern features:
- Add Docker containerization
- Create a CI/CD pipeline
- Add monitoring and alerting
- Implement cloud storage integration

## 📊 Assessment Criteria

Your solution will be evaluated on:

### Functionality (40%)
- [ ] COBOL program compiles without errors
- [ ] Program correctly reads input file
- [ ] Program correctly writes output file
- [ ] Proper error handling implemented

### JCL Design (30%)
- [ ] Correct DD statement usage
- [ ] Proper dataset allocation
- [ ] Appropriate job structure
- [ ] Good use of JCL features

### Code Quality (20%)
- [ ] Clean, readable COBOL code
- [ ] Proper variable naming
- [ ] Good comments and documentation
- [ ] Follows COBOL best practices

### Testing (10%)
- [ ] Solution works with provided test data
- [ ] Handles edge cases properly
- [ ] Produces expected output
- [ ] Error conditions handled gracefully

## 📚 Learning Resources

### COBOL File Handling:
- File organization types (Sequential, Indexed, Relative)
- File status codes and error handling
- Record processing patterns
- File I/O best practices

### JCL Dataset Management:
- DD statement parameters (DSN, DISP, SPACE, UNIT)
- Dataset allocation and cataloging
- Temporary vs permanent datasets
- Dataset naming conventions

### Framework Features:
- Dataset manager commands
- Job scheduler operations
- JCL parser capabilities
- Error handling and logging

## 🎉 Completion Checklist

- [ ] Created COBOL program in `programs/file_processor.cbl`
- [ ] Created sample input data in `data/sample_input.txt`
- [ ] Created JCL script in `jobs/file_processing.jcl`
- [ ] Allocated input dataset using dataset manager
- [ ] Copied sample data to allocated dataset
- [ ] Submitted job using scheduler
- [ ] Verified successful execution
- [ ] Checked output file contents
- [ ] Documented any issues encountered
- [ ] Completed at least one extension challenge

## 🏆 Success Indicators

You've successfully completed this exercise when:

1. **Your COBOL program runs without errors**
2. **Input data is correctly read and processed**
3. **Output file contains expected data**
4. **JCL job executes all steps successfully**
5. **You understand the relationship between COBOL and JCL**
6. **You can explain how datasets work in batch processing**

---

## 📞 Getting Help

If you encounter issues:

1. **Check the framework logs** in the `history/` directory
2. **Review the demo** by running `./demo.sh`
3. **Read the documentation** in `BASH_VS_JCL_GUIDE.md`
4. **Test individual components** before combining them
5. **Ask for help** with specific error messages

Remember: This exercise simulates real enterprise batch processing. The concepts you learn here apply directly to modern data processing, cloud batch services, and enterprise integration patterns!

**Good luck, and happy coding!** 🚀