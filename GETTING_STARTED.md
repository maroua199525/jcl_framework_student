# Getting Started with JCL Simulation Framework

## Quick Start

### 1. Run the Demo
```bash
cd jcl_framework
./demo.sh
```

### 2. Initialize the Framework
```bash
./scheduler.sh init
./dataset_manager.sh init
```

### 3. Try Basic Operations

**Create a dataset:**
```bash
./dataset_manager.sh allocate MY.TEST.DATA PS 2048 100
```

**Submit a job:**
```bash
./scheduler.sh submit jobs/banking_daily.jcl
```

**Process the job queue:**
```bash
./scheduler.sh process
```

**Check status:**
```bash
./scheduler.sh status
```

## Framework Structure

```
jcl_framework/
├── README.md              # Framework overview
├── GETTING_STARTED.md     # This file
├── IBM_MAPPING.md         # Maps to IBM concepts
├── demo.sh               # Quick demonstration
├── test_framework.sh     # Comprehensive test
├── jcl_parser.sh         # JCL syntax parser
├── scheduler.sh          # Job scheduler
├── dataset_manager.sh    # Dataset management
├── schedule.conf         # Job schedule configuration
├── jobs/                 # Sample JCL jobs
│   ├── banking_daily.jcl
│   ├── security_audit.jcl
│   └── monthly_report.jcl
├── queue/                # Job queue (created at runtime)
├── history/              # Job execution history
└── datasets/             # Dataset storage
    ├── master_catalog.dat
    ├── gdg/              # Generation Data Groups
    ├── vsam/             # VSAM datasets
    └── temp/             # Temporary datasets
```

## Key Concepts Learned

### 1. JCL Syntax
```jcl
//JOBNAME  JOB  CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=PROGRAM-NAME
//INPUT    DD   DSN=DATASET.NAME,DISP=SHR
//OUTPUT   DD   DSN=OUTPUT.DATASET,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
```

### 2. Job Dependencies
```jcl
//STEP2    EXEC PGM=NEXT-PROGRAM,COND=(0,NE,STEP1)
```
- STEP2 only runs if STEP1 completes successfully (RC=0)

### 3. Dataset Types
- **PS**: Partitioned Sequential (like text files)
- **VSAM**: Virtual Storage Access Method (indexed files)
- **GDG**: Generation Data Groups (versioned datasets)
- **TEMP**: Temporary datasets

### 4. Job Scheduling
- Time-based scheduling (cron-like)
- Job dependencies
- Priority queuing
- Return code checking

## Common Operations

### Dataset Management
```bash
# List all datasets
./dataset_manager.sh list

# Show dataset info
./dataset_manager.sh info DATASET.NAME

# Copy dataset
./dataset_manager.sh copy SOURCE.DATA TARGET.DATA

# Backup dataset
./dataset_manager.sh backup IMPORTANT.DATA

# Delete dataset
./dataset_manager.sh delete OLD.DATA
```

### Job Management
```bash
# Submit job with priority
./scheduler.sh submit my_job.jcl MYJOB 1

# Check job status
./scheduler.sh status

# Install cron jobs
./scheduler.sh install-cron

# Remove cron jobs
./scheduler.sh remove-cron
```

### JCL Development
```bash
# Parse JCL directly
./jcl_parser.sh my_job.jcl

# Check JCL syntax
./jcl_parser.sh --validate my_job.jcl
```

## Creating Your Own JCL Jobs

### 1. Basic Job Template
```jcl
//MYJOB    JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* Description of what this job does
//*
//STEP1    EXEC PGM=YOUR-PROGRAM
//INPUT    DD   DSN=INPUT.DATASET,DISP=SHR
//OUTPUT   DD   DSN=OUTPUT.DATASET,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
```

### 2. Multi-Step Job with Dependencies
```jcl
//MULTIJOB JOB  CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=VALIDATE-DATA
//INPUT    DD   DSN=RAW.DATA,DISP=SHR
//OUTPUT   DD   DSN=CLEAN.DATA,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
//*
//STEP2    EXEC PGM=PROCESS-DATA,COND=(0,NE,STEP1)
//INPUT    DD   DSN=CLEAN.DATA,DISP=SHR
//OUTPUT   DD   DSN=PROCESSED.DATA,DISP=(NEW,CATLG)
//SYSOUT   DD   SYSOUT=*
```

### 3. Add to Schedule
Edit `schedule.conf`:
```
SCHEDULE|MYJOB|jobs/my_job.jcl|0 6 * * *|
```

## Troubleshooting

### Common Issues

1. **"JCL file not found"**
   - Check file path relative to jcl_framework directory
   - Ensure .jcl extension

2. **"Dataset not found"**
   - Check dataset catalog: `./dataset_manager.sh list`
   - Verify dataset name spelling

3. **"Job failed with RC=8"**
   - Check job log in history/ directory
   - Verify COBOL program exists

4. **"Dependencies not satisfied"**
   - Check prerequisite jobs completed successfully
   - Review job history: `./scheduler.sh status`

### Debug Mode
Add debug output to any script:
```bash
set -x  # Enable debug mode
./jcl_parser.sh my_job.jcl
set +x  # Disable debug mode
```

## Learning Path

### Beginner (Week 1-2)
1. Run demo.sh to understand concepts
2. Create simple single-step jobs
3. Practice dataset operations
4. Learn JCL syntax basics

### Intermediate (Week 3-4)
1. Create multi-step jobs with dependencies
2. Use different dataset types (VSAM, GDG)
3. Set up scheduled jobs
4. Practice error handling

### Advanced (Week 5-6)
1. Create complex job streams
2. Implement custom procedures
3. Study IBM_MAPPING.md for real-world concepts
4. Design enterprise batch workflows

### Enterprise Ready
1. Understand IBM mainframe architecture
2. Learn z/OS specifics
3. Practice with IBM tools (Personal Edition)
4. Consider formal IBM training

## Next Steps

1. **Practice**: Create jobs for your COBOL banking programs
2. **Experiment**: Try different dataset types and job patterns
3. **Study**: Read IBM_MAPPING.md for enterprise concepts
4. **Expand**: Add your own programs to the framework
5. **Learn**: Explore real IBM JCL documentation

## Resources

- IBM JCL Reference: [IBM Knowledge Center](https://www.ibm.com/docs/en/zos)
- COBOL Programming: Use existing .cbl files in parent directory
- Mainframe Concepts: Study IBM_MAPPING.md
- Practice Environment: This framework!

## Support

This framework is designed for learning. For production IBM environments:
- Consult IBM documentation
- Get proper training
- Work with experienced mainframe professionals
- Consider IBM Cloud or Personal Edition for hands-on practice

Happy learning! 🚀