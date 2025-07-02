#!/bin/bash                                                                                                         │
│  # create_student_package.sh                                                                                         │
│                                                                                                                      │
│  # Create clean student package                                                                                      │
│  mkdir -p jcl_framework_student                                                                                      │
│  cp -r jcl_framework/* jcl_framework_student/                                                                        │
│                                                                                                                      │
│  # Remove internal files                                                                                             │
│  rm -f jcl_framework_student/JCL_VS_BASH_ANALYSIS.md                                                                 │
│  rm -f jcl_framework_student/test_framework.sh                                                                       │
│                                                                                                                      │
│  # Clean runtime data                                                                                                │
│  rm -rf jcl_framework_student/history/*                                                                              │
│  rm -rf jcl_framework_student/queue/*                                                                                │
│                                                                                                                      │
│  # Clean datasets but keep structure                                                                                 │
│  find jcl_framework_student/datasets -name "*.dat" -delete                                                           │
│  find jcl_framework_student/datasets -name "*.vsam*" -delete                                                         │
│  find jcl_framework_student/datasets -name "*.tmp" -delete                                                           │
│                                                                                                                      │
│  # Create clean catalog                                                                                              │
│  echo "# Master Catalog - Dataset Registry" > jcl_framework_student/datasets/master_catalog.dat                      │
│  echo "# Format: DSN|TYPE|LOCATION|STATUS|CREATED|SIZE|RECORDS" >> jcl_framework_student/datasets/master_catalog.da  │
│                                                                                                                      │
│  echo "Student package created in: jcl_framework_student/"                                                           │
│                                                                                                                 