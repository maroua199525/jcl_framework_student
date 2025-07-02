       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-VALIDATOR.
       AUTHOR. JCL-FRAMEWORK-DEMO.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RECORD-COUNT         PIC 9(5) VALUE 0.
       01  WS-VALID-COUNT          PIC 9(5) VALUE 0.
       01  WS-INVALID-COUNT        PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "SIMPLE-VALIDATOR: Starting data validation..."
           
           PERFORM SIMULATE-VALIDATION
           
           DISPLAY "SIMPLE-VALIDATOR: Validation completed"
           DISPLAY "SIMPLE-VALIDATOR: Total records: " WS-RECORD-COUNT
           DISPLAY "SIMPLE-VALIDATOR: Valid records: " WS-VALID-COUNT
           DISPLAY "SIMPLE-VALIDATOR: Invalid records: " WS-INVALID-COUNT
           
           STOP RUN.
       
       SIMULATE-VALIDATION.
           MOVE 100 TO WS-RECORD-COUNT
           MOVE 95 TO WS-VALID-COUNT
           MOVE 5 TO WS-INVALID-COUNT
           
           DISPLAY "SIMPLE-VALIDATOR: Processing record batch..."
           DISPLAY "SIMPLE-VALIDATOR: Checking data integrity..."
           DISPLAY "SIMPLE-VALIDATOR: Applying business rules...".