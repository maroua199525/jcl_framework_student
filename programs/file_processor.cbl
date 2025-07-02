       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-PROCESSOR.
       AUTHOR. STUDENT-EXERCISE.
       
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
       01  WS-ERROR-COUNT          PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "FILE-PROCESSOR: Starting file processing..."
           
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           
           PERFORM READ-AND-WRITE-LOOP
               UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE INFILE
           CLOSE OUTFILE
           
           DISPLAY "FILE-PROCESSOR: Processing completed."
           DISPLAY "FILE-PROCESSOR: Records processed: " 
               WS-RECORD-COUNT
           DISPLAY "FILE-PROCESSOR: Errors encountered: " 
               WS-ERROR-COUNT
           
           IF WS-ERROR-COUNT > 0
               DISPLAY "FILE-PROCESSOR: Job completed with warnings"
           ELSE
               DISPLAY "FILE-PROCESSOR: Job completed successfully"
           END-IF
           
           STOP RUN.
       
       READ-AND-WRITE-LOOP.
           READ INFILE INTO INPUT-RECORD
               AT END 
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
                   PERFORM PROCESS-RECORD
           END-READ.
       
       PROCESS-RECORD.
           IF INPUT-RECORD = SPACES
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY "FILE-PROCESSOR: Warning - Empty record skipped"
           ELSE
               MOVE INPUT-RECORD TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-IF.