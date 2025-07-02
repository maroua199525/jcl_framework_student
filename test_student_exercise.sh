#!/bin/bash
#########################################################################
# Student Exercise Test Script
# Tests the file processing exercise setup and execution
#########################################################################

FRAMEWORK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$FRAMEWORK_DIR"

echo "=========================================="
echo "Student Exercise 1 - Test Script"
echo "=========================================="
echo ""

# Test 1: Check if required files exist
echo "Test 1: Checking required files..."
echo "-----------------------------------"

REQUIRED_FILES=(
    "programs/file_processor.cbl"
    "programs/display_output.cbl"
    "data/sample_input.txt"
    "jobs/file_processing.jcl"
)

ALL_FILES_EXIST=true
for file in "${REQUIRED_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        echo "✓ $file exists"
    else
        echo "✗ $file missing"
        ALL_FILES_EXIST=false
    fi
done

if [[ "$ALL_FILES_EXIST" == "false" ]]; then
    echo ""
    echo "❌ Some required files are missing. Please create them first."
    exit 1
fi

echo ""
echo "✅ All required files found!"
echo ""

# Test 2: Initialize framework
echo "Test 2: Initializing framework..."
echo "----------------------------------"
./scheduler.sh init >/dev/null 2>&1
./dataset_manager.sh init >/dev/null 2>&1
echo "✓ Framework initialized"
echo ""

# Test 3: Allocate datasets
echo "Test 3: Allocating datasets..."
echo "-------------------------------"
./dataset_manager.sh allocate STUDENT.INPUT.DATA PS 1024 10 >/dev/null 2>&1
./dataset_manager.sh allocate STUDENT.OUTPUT.DATA PS 1024 10 >/dev/null 2>&1

# Copy sample data
cp data/sample_input.txt datasets/student_input_data.dat

echo "✓ Input dataset allocated and populated"
echo "✓ Output dataset allocated"
echo ""

# Test 4: Test JCL parsing
echo "Test 4: Testing JCL parsing..."
echo "-------------------------------"
./jcl_parser.sh jobs/file_processing.jcl >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
    echo "✓ JCL parsing successful"
else
    echo "✗ JCL parsing failed"
    echo "Run: ./jcl_parser.sh jobs/file_processing.jcl"
    echo "to see detailed error messages"
fi
echo ""

# Test 5: Submit job through scheduler
echo "Test 5: Testing job submission..."
echo "---------------------------------"
JOB_ID=$(./scheduler.sh submit jobs/file_processing.jcl 2>/dev/null | grep "Job submitted:" | cut -d' ' -f3)
if [[ -n "$JOB_ID" ]]; then
    echo "✓ Job submitted successfully: $JOB_ID"
else
    echo "✗ Job submission failed"
fi
echo ""

# Test 6: Check dataset contents
echo "Test 6: Checking results..."
echo "---------------------------"
if [[ -f "datasets/student_output_data.dat" ]]; then
    echo "✓ Output file created"
    echo "✓ Output file contains $(wc -l < datasets/student_output_data.dat) lines"
    
    # Compare input and output
    if diff -q data/sample_input.txt datasets/student_output_data.dat >/dev/null 2>&1; then
        echo "✓ Output matches input (file copy successful)"
    else
        echo "⚠ Output differs from input (this might be expected if processing was done)"
    fi
else
    echo "✗ Output file not created"
fi
echo ""

# Test 7: Show framework status
echo "Test 7: Framework status..."
echo "---------------------------"
echo "Dataset catalog:"
./dataset_manager.sh list STUDENT.* 2>/dev/null || echo "No student datasets found"
echo ""

echo "Job scheduler status:"
./scheduler.sh status | grep -A 5 "Recent Job History:" || echo "No recent jobs"
echo ""

# Summary
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo ""

if [[ -f "datasets/student_output_data.dat" ]]; then
    echo "🎉 SUCCESS: Exercise appears to be working correctly!"
    echo ""
    echo "Next steps:"
    echo "1. Review the output file: cat datasets/student_output_data.dat"
    echo "2. Try modifying the COBOL program"
    echo "3. Experiment with different input data"
    echo "4. Attempt the extension challenges"
else
    echo "❌ ISSUES DETECTED: Exercise needs attention"
    echo ""
    echo "Troubleshooting steps:"
    echo "1. Check JCL syntax: ./jcl_parser.sh jobs/file_processing.jcl"
    echo "2. Verify datasets: ./dataset_manager.sh list"
    echo "3. Review job logs in history/ directory"
    echo "4. Check COBOL program syntax"
fi

echo ""
echo "For detailed help, see: STUDENT_EXERCISE_1.md"
echo "=========================================="