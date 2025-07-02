       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-OUTPUT.
       AUTHOR. STUDENT-EXERCISE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATASET ASSIGN TO "DATASET"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  DATASET.
       01  DATASET-RECORD          PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       01  WS-LINE-COUNT           PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "DISPLAY-OUTPUT: Showing contents of output dataset"
           DISPLAY "=================================================="
           
           OPEN INPUT DATASET
           
           PERFORM READ-AND-DISPLAY-LOOP
               UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE DATASET
           
           DISPLAY "=================================================="
           DISPLAY "DISPLAY-OUTPUT: Total lines displayed: " 
               WS-LINE-COUNT
           
           STOP RUN.
       
       READ-AND-DISPLAY-LOOP.
           READ DATASET INTO DATASET-RECORD
               AT END 
                   MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-LINE-COUNT
                   DISPLAY DATASET-RECORD
           END-READ.