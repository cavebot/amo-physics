*     F06GTF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*
*     THIS IS THE CERTIFICATION PROGRAM FOR THE COMPLEX
*     SPARSE BLAS.  THE APPROACH USED TO CERTIFY THE SPARSE BLAS
*     IS AS FOLLOWS:
*
*     1.  READ IN USER SPECIFIED INPUT ON THRESHOLD VALUE
*         FOR TEST RATIO, AND THE SPECIFICATIONS FOR NZ, AND A.
*     2.  VERIFY THE CORRECTNESS OF THE USER SPECIFIED INPUT AND
*         ECHO TO THE OUTPUT UNIT.
*     3.  FOR EACH SUBPROGRAM PERFORM ALL THE USER SPECIFIED TESTS AND
*         PRINT A PASS/FAIL MESSAGE.  TESTS WHICH FAIL GENERATE
*         ADDITIONAL OUTPUT.
*
*     SPARSE BLAS SUBPROGRAMS WHICH ARE CERTIFIED BY THIS PROGRAM ARE
*
*         F06GRF/CDOTUI
*         F06GSF/CDOTCI
*         F06GTF/CAXPYI
*         F06GUF/CGTHR
*         F06GVF/CGTHRZ
*         F06GWF/CSCTR
*
*     THIS PROGRAM REQUIRES AN INPUT FILE ASSIGNED TO UNIT NIN
*     (CURRENTLY SET TO 5 BY A PARAMETER STATEMENT).  THE DATA ON
*     THIS INPUT FILE CONTROLS THE THRESHOLD VALUE
*     FOR THE NUMERICAL TESTING, AND THE SPECIFICATIONS FOR THE
*     TEST VALUES FOR THE LENGTH OF THE SPARSE VECTORS AND THE SCALARS
*     USED BY THE VARIOUS SUBPROGRAMS.
*
*     THIS PROGRAM WAS WRITTEN BY ROGER G. GRIMES, BOEING
*     COMPUTER SERVICES, BELLEVUE, WA. DURING APRIL, 1987.
*
*     --- PROBLEM SPECIFICATION PARAMETERS
*
*         NZMAX       MAXIMUM VALUE OF ANY SINGLE NZ
*         NNZMAX      MAXIMUM NUMBER OF VALUES OF NZ
*         NAMAX       MAXIMUM NUMBER OF VALUES OF A (-AXPYI
*                     SCALAR)
*
*     .. Parameters ..
      INTEGER          NIN, NOUT, NZMAX, NNZMAX, NAMAX
      PARAMETER        (NIN=5,NOUT=6,NZMAX=320,NNZMAX=24,NAMAX=7)
      INTEGER          NZMAX2
      PARAMETER        (NZMAX2=2*NZMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION EPSILN, THRESH
      INTEGER          ERRCNT, ERRMAX, I, NUMA, NUMNZ
*     .. Local Arrays ..
      COMPLEX*16       AVALUE(NAMAX), X(NZMAX2), XSAVE(NZMAX2),
     +                 XTRUE(NZMAX2), Y(NZMAX2), YSAVE(NZMAX2),
     +                 YTRUE(NZMAX2)
      INTEGER          INDX(NZMAX2), INDXT(NZMAX2), LIST(NZMAX2),
     +                 NZVALU(NNZMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         TCDTCI, TCDTUI, TCGTHR, TCGTHZ, TCSCTR, TCXPYI
*     .. Executable Statements ..
*
      ERRCNT = 0
*
*     --- READ USER-SPECIFIED INPUT
*
      READ (NIN,*)
      READ (NIN,*) ERRMAX
      READ (NIN,*) THRESH
      READ (NIN,*) NUMNZ
*
      IF (NUMNZ.GT.NNZMAX) THEN
         ERRCNT = 1
         WRITE (NOUT,99994) NUMNZ, NNZMAX
         GO TO 40
      END IF
*
      READ (NIN,*) (NZVALU(I),I=1,NUMNZ)
*
      READ (NIN,*) NUMA
*
      IF (NUMA.GT.NAMAX) THEN
         ERRCNT = 1
         WRITE (NOUT,99993) NUMA, NAMAX
         GO TO 40
      END IF
*
      READ (NIN,*) (AVALUE(I),I=1,NUMA)
*
*     --- PRINT USER SPECIFIED INPUT
*
      WRITE (NOUT,99999) ERRMAX, THRESH
      WRITE (NOUT,99998) NUMNZ
      WRITE (NOUT,99997) (NZVALU(I),I=1,NUMNZ)
      WRITE (NOUT,99996) NUMA
      WRITE (NOUT,99995) (AVALUE(I),I=1,NUMA)
*
*     --- VERIFY USER SPECIFIED INPUT
*
      IF (THRESH.LE.0.0D0) THEN
         WRITE (NOUT,99992) THRESH
         THRESH = 10.0D0
      END IF
*
      IF (NUMNZ.LE.0) THEN
         WRITE (NOUT,99991) NUMNZ
         ERRCNT = 1
      END IF
*
      DO 20 I = 1, NUMNZ
         IF (NZVALU(I).GT.NZMAX) THEN
            WRITE (NOUT,99990) I, NZVALU(I), NZMAX
            NZVALU(I) = NZMAX
         END IF
   20 CONTINUE
*
      IF (ERRCNT.NE.0) GO TO 40
*
*     --- COMPUTE MACHINE EPSILON
*
      EPSILN = X02AJF()
*
*     --- TEST THE COMPLEX SPARSE BLAS
*
*     --- CERTIFY F06GRF
*
      CALL TCDTUI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,
     +            Y,YSAVE,YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     --- CERTIFY F06GSF
*
      CALL TCDTCI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,
     +            Y,YSAVE,YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     --- CERTIFY F06GTF
*
      CALL TCXPYI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,NUMA,AVALUE,X,
     +            XSAVE,XTRUE,Y,YSAVE,YTRUE,INDX,INDXT,LIST,ERRCNT,
     +            ERRMAX)
*
*     --- CERTIFY F06GUF
*
      CALL TCGTHR(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,YTRUE,
     +            INDX,INDXT,ERRCNT,ERRMAX)
*
*     --- CERTIFY F06GVF
*
      CALL TCGTHZ(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,YTRUE,
     +            INDX,INDXT,ERRCNT,ERRMAX)
*
*     --- CERTIFY F06GWF
*
      CALL TCSCTR(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,YTRUE,
     +            INDX,INDXT,ERRCNT,ERRMAX)
*
*     --- PRINT GLOBAL PASS OR FAIL MESSAGE
*
   40 IF (ERRCNT.EQ.0) THEN
         WRITE (NOUT,99989)
      ELSE
         WRITE (NOUT,99988) ERRCNT
      END IF
*
      STOP
*
99999 FORMAT (' F06GTF Example Program Results',//1X,'MAX. NO. OF PRIN',
     +       'TED ERROR MESSAGES = ',I10,/1X,'THRESHOLD VALUE OF TEST ',
     +       'RATIO      = ',F10.1)
99998 FORMAT (/1X,'NUMBER OF VALUES OF NZ        = ',I10)
99997 FORMAT (/1X,'VALUES OF NZ = ',10I5,/16X,10I5,/16X,10I5)
99996 FORMAT (/1X,'NUMBER OF VALUES OF A         = ',I10)
99995 FORMAT (/1X,'VALUES OF A =',3X,'(',1P,D13.4,',',1P,D13.4,')',
     +       :/(17X,'(',1P,D13.4,',',1P,D13.4,')',:))
99994 FORMAT (/1X,'NUMBER OF VALUES OF NZ  EXCEEDS PROGRAM LIMIT.',/1X,
     +       'NUMBER SPECIFIED = ',I10,2X,'PROGRAM LIMIT =',I10)
99993 FORMAT (/1X,'NUMBER OF VALUES OF A EXCEEDS PROGRAM LIMIT.',/1X,
     +       'NUMBER SPECIFIED = ',I10,2X,'PROGRAM LIMIT =',I10)
99992 FORMAT (/1X,'VALUE FOR THRESHOLD IS ',1P,D15.5,' WHICH IS NONPOS',
     +       'ITIVE.  IT HAS BEEN RESET TO 10.')
99991 FORMAT (/1X,'NUMBER OF VALUES OF NZ IS ',I5,' WHICH IS NONPOSITI',
     +       'VE.  NO TESTING WILL OCCUR.')
99990 FORMAT (/1X,'THE ',I3,'-TH VALUE OF NZ IS ',I8,' WHICH IS TOO LA',
     +       'RGE.  IT HAS BEEN RESET TO ',I5)
99989 FORMAT (/1X,'ALL TESTS PASSED.')
99988 FORMAT (/1X,I10,' TESTS FAILED')
      END
      SUBROUTINE TCDTUI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,X,XSAVE,
     +                  XTRUE,Y,YSAVE,YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCDTUI  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GRF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPSILN, THRESH
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        X(*), XSAVE(*), XTRUE(*), Y(*), YSAVE(*),
     +                  YTRUE(*)
      INTEGER           INDX(*), INDXT(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        CLOBBR, V, W
      DOUBLE PRECISION  ERR, S, T
      INTEGER           COUNT, I, ICLOBR, J, KINDX, KNZ, N, NZ, NZTRUE
*     .. External Functions ..
      COMPLEX*16        F06GRF
      LOGICAL           CVSAME, IVSAME
      EXTERNAL          F06GRF, CVSAME, IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DFF, F06GFF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, COS, MAX, MIN, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 120 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH KIND OF INDX ARRAY
*
         DO 100 KINDX = 1, 5
*
            CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
*           --- GENERATE INPUT DATA
*
            I = MIN(N,N-NZTRUE)
            J = N - I + 1
            CALL F06GFF(NZTRUE,XSAVE,1,XTRUE,1)
            CALL F06HBF(I,CLOBBR,XTRUE(J),1)
            CALL F06HBF(N,CLOBBR,YTRUE,1)
*
            DO 40 I = 1, NZTRUE
               YTRUE(INDXT(I)) = YSAVE(INDXT(I))
   40       CONTINUE
*
*           --- COPY TRUE INPUT
*
            NZ = NZTRUE
*
            CALL F06GFF(N,YTRUE,1,Y,1)
            CALL F06GFF(N,XTRUE,1,X,1)
            CALL F06DFF(N,INDXT,1,INDX,1)
*
*           --- COMPUTE IN-LINE RESULT
*
            V = (0.0D0,0.0D0)
*
            DO 60 I = 1, NZTRUE
               V = V + XTRUE(I)*YTRUE(INDXT(I))
   60       CONTINUE
*
*           --- CALL F06GRF
*
            W = F06GRF(NZ,X,INDX,Y)
*
*           --- TEST ARGUMENTS OF F06GRF THAT ARE NOT
*               SUPPOSED TO CHANGE.
*
            IF (NZ.NE.NZTRUE) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99999) NZTRUE, KINDX, NZ
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,X,XTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99998) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99997) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,Y,YTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99996) NZTRUE, KINDX
               END IF
            END IF
*
*           --- TEST OUTPUT FROM F06GRF
*
            S = ABS(V-W)
*
            T = 0.0D0
            DO 80 I = 1, NZTRUE
               T = T + ABS(XTRUE(I)*YTRUE(INDXT(I)))
   80       CONTINUE
*
            IF (T.EQ.0.0D0) T = 1.0D0
*
            ERR = S/(EPSILN*T)
*
            IF (ERR.GT.THRESH) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99995) NZTRUE, KINDX, W, V, ERR
               END IF
            END IF
*
  100    CONTINUE
*
  120 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 140
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99994)
      GO TO 160
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  140 WRITE (NOUT,99993) COUNT
*
  160 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GRF ALTERED NZ FOR TEST WITH NZ = ',I5,' AND THE ',
     +       'INDX TYPE NO. ',I5,'.  ALTERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GRF ALTERED ARRAY X FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99997 FORMAT (1X,'F06GRF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5)
99996 FORMAT (1X,'F06GRF ALTERED ARRAY Y FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99995 FORMAT (1X,'F06GRF OUTPUT W IS INACCURATE FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5,/1X,'F06GRF HAS VALUE = (',1P,
     +       D15.5,',',1P,D15.5,') TRUE VALUE = (',1P,D15.5,',',1P,
     +       D15.5,') ERROR = ',1P,D12.1)
99994 FORMAT (/1X,'F06GRF PASSED ALL TESTS.')
99993 FORMAT (/1X,'F06GRF FAILED',I10,' TESTS.')
      END
      SUBROUTINE TCDTCI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,X,XSAVE,
     +                  XTRUE,Y,YSAVE,YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCDTCI  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GSF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPSILN, THRESH
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        X(*), XSAVE(*), XTRUE(*), Y(*), YSAVE(*),
     +                  YTRUE(*)
      INTEGER           INDX(*), INDXT(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        CLOBBR, V, W
      DOUBLE PRECISION  ERR, S, T
      INTEGER           COUNT, I, ICLOBR, J, KINDX, KNZ, N, NZ, NZTRUE
*     .. External Functions ..
      COMPLEX*16        F06GSF
      LOGICAL           CVSAME, IVSAME
      EXTERNAL          F06GSF, CVSAME, IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DFF, F06GFF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, DCONJG, COS, MAX, MIN, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 120 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH KIND OF INDX ARRAY
*
         DO 100 KINDX = 1, 5
*
            CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
*           --- GENERATE INPUT DATA
*
            I = MIN(N,N-NZTRUE)
            J = N - I + 1
            CALL F06GFF(NZTRUE,XSAVE,1,XTRUE,1)
            CALL F06HBF(I,CLOBBR,XTRUE(J),1)
            CALL F06HBF(N,CLOBBR,YTRUE,1)
*
            DO 40 I = 1, NZTRUE
               YTRUE(INDXT(I)) = YSAVE(INDXT(I))
   40       CONTINUE
*
*           --- COPY TRUE INPUT
*
            NZ = NZTRUE
*
            CALL F06GFF(N,YTRUE,1,Y,1)
            CALL F06GFF(N,XTRUE,1,X,1)
            CALL F06DFF(N,INDXT,1,INDX,1)
*
*           --- COMPUTE IN-LINE RESULT
*
            V = (0.0D0,0.0D0)
*
            DO 60 I = 1, NZTRUE
               V = V + DCONJG(XTRUE(I))*YTRUE(INDXT(I))
   60       CONTINUE
*
*           --- CALL F06GSF
*
            W = F06GSF(NZ,X,INDX,Y)
*
*           --- TEST ARGUMENTS OF F06GSF THAT ARE NOT
*               SUPPOSED TO CHANGE.
*
            IF (NZ.NE.NZTRUE) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99999) NZTRUE, KINDX, NZ
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,X,XTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99998) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99997) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,Y,YTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99996) NZTRUE, KINDX
               END IF
            END IF
*
*           --- TEST OUTPUT FROM F06GSF
*
            S = ABS(V-W)
*
            T = 0.0D0
            DO 80 I = 1, NZTRUE
               T = T + ABS(XTRUE(I)*YTRUE(INDXT(I)))
   80       CONTINUE
*
            IF (T.EQ.0.0D0) T = 1.0D0
*
            ERR = S/(EPSILN*T)
*
            IF (ERR.GT.THRESH) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99995) NZTRUE, KINDX, W, V, ERR
               END IF
            END IF
*
  100    CONTINUE
*
  120 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 140
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99994)
      GO TO 160
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  140 WRITE (NOUT,99993) COUNT
*
  160 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GSF ALTERED NZ FOR TEST WITH NZ = ',I5,' AND THE ',
     +       'INDX TYPE NO. ',I5,'.  ALTERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GSF ALTERED ARRAY X FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99997 FORMAT (1X,'F06GSF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5)
99996 FORMAT (1X,'F06GSF ALTERED ARRAY Y FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99995 FORMAT (1X,'F06GSF OUTPUT W IS INACCURATE FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5,/1X,'F06GSF HAS VALUE = (',1P,
     +       D15.5,',',1P,D15.5,') TRUE VALUE = (',1P,D15.5,',',1P,
     +       D15.5,') ERROR = ',1P,D12.1)
99994 FORMAT (/1X,'F06GSF PASSED ALL TESTS.')
99993 FORMAT (/1X,'F06GSF FAILED',I10,' TESTS.')
      END
      SUBROUTINE TCXPYI(NOUT,EPSILN,THRESH,NZMAX2,NUMNZ,NZVALU,NUMA,
     +                  AVALUE,X,XSAVE,XTRUE,Y,YSAVE,YTRUE,INDX,INDXT,
     +                  LIST,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCXPYI  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GTF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION  EPSILN, THRESH
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMA, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        AVALUE(*), X(*), XSAVE(*), XTRUE(*), Y(*),
     +                  YSAVE(*), YTRUE(*)
      INTEGER           INDX(*), INDXT(*), LIST(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        A, ATRUE, CLOBBR
      DOUBLE PRECISION  ERR, S, T
      INTEGER           COUNT, I, ICLOBR, J, KA, KINDX, KNZ, N, NZ,
     +                  NZTRUE
*     .. External Functions ..
      LOGICAL           CVSAME, IVSAME
      EXTERNAL          CVSAME, IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DBF, F06DFF, F06GFF, F06GTF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, COS, MAX, MIN, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 160 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH VALUE OF A
*
         DO 140 KA = 1, NUMA
*
            ATRUE = AVALUE(KA)
*
*           --- FOR EACH KIND OF INDX ARRAY
*
            DO 120 KINDX = 1, 5
*
               CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
               CALL F06DBF(N,-1,LIST,1)
*
               DO 40 I = 1, NZTRUE
                  LIST(INDXT(I)) = I
   40          CONTINUE
*
*              --- GENERATE INPUT DATA
*
               I = MIN(N,N-NZTRUE)
               J = N - I + 1
               CALL F06GFF(NZTRUE,XSAVE,1,XTRUE,1)
               CALL F06HBF(I,CLOBBR,XTRUE(J),1)
               CALL F06HBF(N,CLOBBR,YTRUE,1)
*
               DO 60 I = 1, NZTRUE
                  YTRUE(INDXT(I)) = YSAVE(INDXT(I))
   60          CONTINUE
*
*              --- COPY TRUE INPUT
*
               A = ATRUE
               NZ = NZTRUE
*
               CALL F06GFF(N,YTRUE,1,Y,1)
               CALL F06GFF(N,XTRUE,1,X,1)
               CALL F06DFF(N,INDXT,1,INDX,1)
*
*              --- COMPUTE IN-LINE RESULT
*
               DO 80 I = 1, NZTRUE
                  YTRUE(INDXT(I)) = YTRUE(INDXT(I)) + ATRUE*XTRUE(I)
   80          CONTINUE
*
*              --- CALL F06GTF
*
               CALL F06GTF(NZ,A,X,INDX,Y)
*
*              --- TEST ARGUMENTS OF F06GTF THAT ARE NOT
*                  SUPPOSED TO CHANGE.
*
               IF (NZ.NE.NZTRUE) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99999) NZTRUE, ATRUE, KINDX, NZ
                  END IF
               END IF
*
               IF (A.NE.ATRUE) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99998) NZTRUE, ATRUE, KINDX, A
                  END IF
               END IF
*
               IF ( .NOT. CVSAME(N,X,XTRUE)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99997) NZTRUE, ATRUE, KINDX
                  END IF
               END IF
*
               IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99996) NZTRUE, ATRUE, KINDX
                  END IF
               END IF
*
*              --- TEST OUTPUT FROM F06GTF
*
               DO 100 J = 1, N
                  IF (LIST(J).EQ.-1) THEN
                     IF (Y(J).NE.YTRUE(J)) THEN
                        COUNT = COUNT + 1
                        IF (COUNT.LE.ERRMAX) THEN
                           WRITE (NOUT,99995) NZTRUE, ATRUE, KINDX, J,
     +                       Y(J), YTRUE(J)
                        END IF
                     END IF
*
                  ELSE
*
                     S = ABS(Y(J)-YTRUE(J))
                     T = ABS(ATRUE)*ABS(XTRUE(LIST(J))) + ABS(YTRUE(J))
                     ERR = S/(EPSILN*T)
                     IF (ERR.GT.THRESH) THEN
                        COUNT = COUNT + 1
                        IF (COUNT.LE.ERRMAX) THEN
                           WRITE (NOUT,99994) NZTRUE, ATRUE, KINDX, J,
     +                       Y(J), YTRUE(J), ERR
                        END IF
                     END IF
*
                  END IF
*
  100          CONTINUE
*
  120       CONTINUE
*
  140    CONTINUE
*
  160 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 180
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99993)
      GO TO 200
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  180 WRITE (NOUT,99992) COUNT
*
  200 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GTF ALTERED NZ FOR TEST WITH NZ = ',I5,' A = (',1P,
     +       D15.5,',',1P,D15.5,') AND THE INDX TYPE NO. ',I5,/1X,'ALT',
     +       'ERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GTF ALTERED A FOR TEST WITH NZ = ',I5,' A = (',1P,
     +       D15.5,',',1P,D15.5,') AND THE INDX TYPE NO. ',I5,/1X,'ALT',
     +       'ERED VALUE OF A = (',1P,D15.5,',',1P,D15.5,')')
99997 FORMAT (1X,'F06GTF ALTERED ARRAY X FOR TEST WITH NZ = ',I5,' A =',
     +       ' (',1P,D15.5,',',1P,D15.5,') AND THE INDX TYPE NO. ',I5)
99996 FORMAT (1X,'F06GTF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' A = (',1P,D15.5,',',1P,D15.5,') AND THE INDX TYPE NO. ',
     +       I5)
99995 FORMAT (1X,'F06GTF OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH NZ ',
     +       '= ',I5,' A = (',1P,D15.5,',',1P,D15.5,') AND THE INDX TY',
     +       'PE NO. ',I5,/1X,'INCORRECT COMPONENT NO. ',I5,' HAS VALU',
     +       'E = (',1P,D15.5,',',1P,D15.5,') TRUE VALUE = (',1P,D15.5,
     +       ',',1P,D15.5,')')
99994 FORMAT (1X,'F06GTF OUTPUT ARRAY Y IS INACCURATE FOR TEST WITH NZ',
     +       ' = ',I5,' A = (',1P,D15.5,',',1P,D15.5,') AND THE INDX T',
     +       'YPE NO. ',I5,/1X,'INACCURATE COMPONENT NO. ',I5,' HAS VA',
     +       'LUE = (',1P,D15.5,',',1P,D15.5,') TRUE VALUE = (',1P,
     +       D15.5,',',1P,D15.5,')',/1X,'ERROR = ',1P,D12.1)
99993 FORMAT (/1X,'F06GTF PASSED ALL TESTS.')
99992 FORMAT (/1X,'F06GTF FAILED',I10,' TESTS.')
      END
      SUBROUTINE TCGTHR(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,
     +                  YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCGTHR  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GUF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        X(*), XSAVE(*), XTRUE(*), Y(*), YSAVE(*),
     +                  YTRUE(*)
      INTEGER           INDX(*), INDXT(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        CLOBBR
      INTEGER           COUNT, I, ICLOBR, KINDX, KNZ, N, NZ, NZTRUE
*     .. External Functions ..
      LOGICAL           CVSAME, IVSAME
      EXTERNAL          CVSAME, IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DFF, F06GFF, F06GUF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         DCMPLX, COS, MAX, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 120 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH KIND OF INDX ARRAY
*
         DO 100 KINDX = 1, 5
*
            CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
*           --- GENERATE INPUT DATA
*
            CALL F06HBF(N,CLOBBR,XTRUE,1)
            CALL F06HBF(N,CLOBBR,YTRUE,1)
*
            DO 40 I = 1, NZTRUE
               YTRUE(INDXT(I)) = YSAVE(INDXT(I))
   40       CONTINUE
*
*           --- COPY TRUE INPUT
*
            NZ = NZTRUE
*
            CALL F06GFF(N,YTRUE,1,Y,1)
            CALL F06GFF(N,XTRUE,1,X,1)
            CALL F06DFF(N,INDXT,1,INDX,1)
*
*           --- COMPUTE IN-LINE RESULT
*
            DO 60 I = 1, NZTRUE
               XTRUE(I) = YTRUE(INDXT(I))
   60       CONTINUE
*
*           --- CALL F06GUF
*
            CALL F06GUF(NZ,Y,X,INDX)
*
*           --- TEST ARGUMENTS OF F06GUF THAT ARE NOT
*               SUPPOSED TO CHANGE.
*
            IF (NZ.NE.NZTRUE) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99999) NZTRUE, KINDX, NZ
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,Y,YTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99998) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99997) NZTRUE, KINDX
               END IF
            END IF
*
*           --- TEST OUTPUT FROM F06GUF
*
            DO 80 I = 1, N
               IF (X(I).NE.XTRUE(I)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99996) NZTRUE, KINDX, I, X(I), XTRUE(I)
                  END IF
               END IF
   80       CONTINUE
*
  100    CONTINUE
*
  120 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 140
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99995)
      GO TO 160
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  140 WRITE (NOUT,99994) COUNT
*
  160 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GUF ALTERED NZ FOR TEST WITH NZ = ',I5,' AND THE ',
     +       'INDX TYPE NO. ',I5,'.  ALTERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GUF ALTERED ARRAY Y FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99997 FORMAT (1X,'F06GUF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5)
99996 FORMAT (1X,'F06GUF OUTPUT ARRAY X IS INCORRECT FOR TEST WITH NZ ',
     +       '= ',I5,' AND THE INDX TYPE NO. ',I5,/1X,'INACCURATE COMP',
     +       'ONENT NO. ',I5,' HAS VALUE = (',1P,D15.5,',',1P,D15.5,
     +       ') TRUE VALUE = (',1P,D15.5,',',1P,D15.5,')')
99995 FORMAT (/1X,'F06GUF PASSED ALL TESTS.')
99994 FORMAT (/1X,'F06GUF FAILED',I10,' TESTS.')
      END
      SUBROUTINE TCGTHZ(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,
     +                  YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCGTHZ  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GVF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        X(*), XSAVE(*), XTRUE(*), Y(*), YSAVE(*),
     +                  YTRUE(*)
      INTEGER           INDX(*), INDXT(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        CLOBBR
      INTEGER           COUNT, I, ICLOBR, KINDX, KNZ, N, NZ, NZTRUE
*     .. External Functions ..
      LOGICAL           IVSAME
      EXTERNAL          IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DFF, F06GFF, F06GVF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         DCMPLX, COS, MAX, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 120 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH KIND OF INDX ARRAY
*
         DO 100 KINDX = 1, 5
*
            CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
*           --- GENERATE INPUT DATA
*
            CALL F06HBF(N,CLOBBR,XTRUE,1)
            CALL F06HBF(N,CLOBBR,YTRUE,1)
*
            DO 40 I = 1, NZTRUE
               YTRUE(INDXT(I)) = YSAVE(INDXT(I))
   40       CONTINUE
*
*           --- COPY TRUE INPUT
*
            NZ = NZTRUE
*
            CALL F06GFF(N,YTRUE,1,Y,1)
            CALL F06GFF(N,XTRUE,1,X,1)
            CALL F06DFF(N,INDXT,1,INDX,1)
*
*           --- COMPUTE IN-LINE RESULT
*
            DO 60 I = 1, NZTRUE
               XTRUE(I) = YTRUE(INDXT(I))
               YTRUE(INDXT(I)) = (0.0D0,0.0D0)
   60       CONTINUE
*
*           --- CALL F06GVF
*
            CALL F06GVF(NZ,Y,X,INDX)
*
*           --- TEST ARGUMENTS OF F06GVF THAT ARE NOT
*               SUPPOSED TO CHANGE.
*
            IF (NZ.NE.NZTRUE) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99999) NZTRUE, KINDX, NZ
               END IF
            END IF
*
            IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99998) NZTRUE, KINDX
               END IF
            END IF
*
*           --- TEST OUTPUT FROM F06GVF
*
            DO 80 I = 1, N
*
               IF (X(I).NE.XTRUE(I)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99997) NZTRUE, KINDX, I, X(I), XTRUE(I)
                  END IF
               END IF
*
               IF (Y(I).NE.YTRUE(I)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99996) NZTRUE, KINDX, I, Y(I), YTRUE(I)
                  END IF
               END IF
*
   80       CONTINUE
*
  100    CONTINUE
*
  120 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 140
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99995)
      GO TO 160
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  140 WRITE (NOUT,99994) COUNT
  160 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GVF ALTERED NZ FOR TEST WITH NZ = ',I5,' AND THE ',
     +       'INDX TYPE NO. ',I5,'.  ALTERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GVF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5)
99997 FORMAT (1X,'F06GVF OUTPUT ARRAY X IS INCORRECT FOR TEST WITH NZ ',
     +       '= ',I5,' AND THE INDX TYPE NO. ',I5,/1X,'INACCURATE COMP',
     +       'ONENT NO. ',I5,' HAS VALUE = (',1P,D15.5,',',1P,D15.5,
     +       ') TRUE VALUE = (',1P,D15.5,',',1P,D15.5,')')
99996 FORMAT (1X,'F06GVF OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH NZ ',
     +       '= ',I5,' AND THE INDX TYPE NO. ',I5,/1X,'INACCURATE COMP',
     +       'ONENT NO. ',I5,' HAS VALUE = (',1P,D15.5,',',1P,D15.5,
     +       ') TRUE VALUE = (',1P,D15.5,',',1P,D15.5,')')
99995 FORMAT (/1X,'F06GVF PASSED ALL TESTS.')
99994 FORMAT (/1X,'F06GVF FAILED',I10,' TESTS.')
      END
      SUBROUTINE TCSCTR(NOUT,NZMAX2,NUMNZ,NZVALU,X,XSAVE,XTRUE,Y,YSAVE,
     +                  YTRUE,INDX,INDXT,ERRCNT,ERRMAX)
*
*     SUBROUTINE  TCSCTR  IS THE CERTIFICATION MODULE FOR THE SPARSE
*     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  F06GWF.
*
*     WRITTEN BY      ROGER G GRIMES
*                     APRIL 1987
*
*     .. Scalar Arguments ..
      INTEGER           ERRCNT, ERRMAX, NOUT, NUMNZ, NZMAX2
*     .. Array Arguments ..
      COMPLEX*16        X(*), XSAVE(*), XTRUE(*), Y(*), YSAVE(*),
     +                  YTRUE(*)
      INTEGER           INDX(*), INDXT(*), NZVALU(*)
*     .. Local Scalars ..
      COMPLEX*16        CLOBBR
      INTEGER           COUNT, I, ICLOBR, J, KINDX, KNZ, N, NZ, NZTRUE
*     .. External Functions ..
      LOGICAL           CVSAME, IVSAME
      EXTERNAL          CVSAME, IVSAME
*     .. External Subroutines ..
      EXTERNAL          F06DFF, F06GFF, F06GWF, F06HBF, GNINDX
*     .. Intrinsic Functions ..
      INTRINSIC         DCMPLX, COS, MAX, MIN, DBLE, SIN
*     .. Executable Statements ..
*
*     --- INITIALIZATION
*
      COUNT = 0
*
      CLOBBR = (-1.0D10,-1.0D10)
      ICLOBR = -10000000
*
*     --- GENERATE SOME VALUES FOR X AND Y
*
      DO 20 I = 1, NZMAX2
         XSAVE(I) = DCMPLX(COS(.6D0*DBLE(I)),SIN(.2D0*DBLE(I)))
         YSAVE(I) = DCMPLX(SIN(.7D0*DBLE(I)),COS(.9D0*DBLE(I)))
   20 CONTINUE
*
*     --- FOR EACH VALUE OF NZ
*
      DO 100 KNZ = 1, NUMNZ
*
         NZTRUE = NZVALU(KNZ)
         N = 2*MAX(NZTRUE,1)
*
*        --- FOR EACH KIND OF INDX ARRAY
*
         DO 80 KINDX = 1, 5
*
            CALL GNINDX(NZTRUE,N,ICLOBR,KINDX,INDXT)
*
*           --- GENERATE INPUT DATA
*
            I = MIN(N,N-NZTRUE)
            J = N - I + 1
            CALL F06GFF(NZTRUE,XSAVE,1,XTRUE,1)
            CALL F06HBF(I,CLOBBR,XTRUE(J),1)
            CALL F06HBF(N,CLOBBR,YTRUE,1)
*
*           --- COPY TRUE INPUT
*
            NZ = NZTRUE
*
            CALL F06GFF(N,YTRUE,1,Y,1)
            CALL F06GFF(N,XTRUE,1,X,1)
            CALL F06DFF(N,INDXT,1,INDX,1)
*
*           --- COMPUTE IN-LINE RESULT
*
            DO 40 I = 1, NZTRUE
               YTRUE(INDXT(I)) = XTRUE(I)
   40       CONTINUE
*
*           --- CALL F06GWF
*
            CALL F06GWF(NZ,X,INDX,Y)
*
*           --- TEST ARGUMENTS OF F06GWF THAT ARE NOT
*               SUPPOSED TO CHANGE.
*
            IF (NZ.NE.NZTRUE) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99999) NZTRUE, KINDX, NZ
               END IF
            END IF
*
            IF ( .NOT. CVSAME(N,X,XTRUE)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99998) NZTRUE, KINDX
               END IF
            END IF
*
            IF ( .NOT. IVSAME(N,INDX,INDXT)) THEN
               COUNT = COUNT + 1
               IF (COUNT.LE.ERRMAX) THEN
                  WRITE (NOUT,99997) NZTRUE, KINDX
               END IF
            END IF
*
*           --- TEST OUTPUT FROM F06GWF
*
            DO 60 I = 1, N
               IF (Y(I).NE.YTRUE(I)) THEN
                  COUNT = COUNT + 1
                  IF (COUNT.LE.ERRMAX) THEN
                     WRITE (NOUT,99996) NZTRUE, KINDX, I, Y(I), YTRUE(I)
                  END IF
               END IF
   60       CONTINUE
*
   80    CONTINUE
*
  100 CONTINUE
*
*     --- END OF TESTING
*
      ERRCNT = ERRCNT + COUNT
      IF (COUNT.NE.0) GO TO 120
*
*     --- WRITE PASSED MESSAGE AND RETURN
*
      WRITE (NOUT,99995)
      GO TO 140
*
*     --- WRITE FAILED MESSAGE AND RETURN
*
  120 WRITE (NOUT,99994) COUNT
*
  140 CONTINUE
      RETURN
*
99999 FORMAT (1X,'F06GWF ALTERED NZ FOR TEST WITH NZ = ',I5,' AND THE ',
     +       'INDX TYPE NO. ',I5,'.  ALTERED VALUE OF NZ = ',I5)
99998 FORMAT (1X,'F06GWF ALTERED ARRAY X FOR TEST WITH NZ = ',I5,' AND',
     +       ' THE INDX TYPE NO. ',I5)
99997 FORMAT (1X,'F06GWF ALTERED ARRAY INDX FOR TEST WITH NZ = ',I5,
     +       ' AND THE INDX TYPE NO. ',I5)
99996 FORMAT (1X,'F06GWF OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH NZ ',
     +       '= ',I5,' AND THE INDX TYPE NO. ',I5,/1X,'INACCURATE COMP',
     +       'ONENT NO. ',I5,' HAS VALUE = (',1P,D15.5,',',1P,D15.5,
     +       ') TRUE VALUE = (',1P,D15.5,',',1P,D15.5,')')
99995 FORMAT (/1X,'F06GWF PASSED ALL TESTS.')
99994 FORMAT (/1X,'F06GWF FAILED',I10,' TESTS.')
      END
      SUBROUTINE GNINDX(NZ,N,ICLOBR,KINDX,INDX)
*
*     GNINDX GENERATES VARIOUS PATTERNS FOR THE ARRAY INDX BASED
*     ON THE KEY KINDX.  THE GENERATED INDX ARRAY HAS NZ SIGNIFICANT
*     COMPONENTS.  THE REMAINING N-NZ COMPONENTS ARE SET TO
*     ICLOBR.
*
*     .. Scalar Arguments ..
      INTEGER           ICLOBR, KINDX, N, NZ
*     .. Array Arguments ..
      INTEGER           INDX(*)
*     .. Local Scalars ..
      INTEGER           I, L
*     .. External Subroutines ..
      EXTERNAL          F06DBF
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Executable Statements ..
*
      IF (N.LE.0) RETURN
*
      L = MAX(N,N-NZ)
      CALL F06DBF(L,ICLOBR,INDX,1)
*
      IF (NZ.LE.0) RETURN
*
*     --- BRANCH ON KINDX
*
      GO TO (20,60,100,140,180) KINDX
*
*     --- ASCENDING ORDER - 1, 2, ..., NZ
*
   20 DO 40 I = 1, NZ
         INDX(I) = I
   40 CONTINUE
      GO TO 240
*
*     --- ASCENDING ORDER - N-NZ+1, N-NZ, ..., N
*
   60 L = N - NZ
      DO 80 I = 1, NZ
         INDX(I) = L + I
   80 CONTINUE
      GO TO 240
*
*     --- DESCENDING ORDER - NZ, NZ-1, ..., 1
*
  100 L = NZ
      DO 120 I = 1, NZ
         INDX(I) = L
         L = L - 1
  120 CONTINUE
      GO TO 240
*
*     --- DESCENDING ORDER - N, N-1, ..., N-NZ+1
*
  140 L = N
      DO 160 I = 1, NZ
         INDX(I) = L
         L = L - 1
  160 CONTINUE
      GO TO 240
*
*     --- ALTERNATING ORDER WITH EVEN NUMBERS IN REVERSE ORDER
*
  180 DO 200 I = 1, NZ, 2
         INDX(I) = I
  200 CONTINUE
*
      L = N
      DO 220 I = 2, NZ, 2
         INDX(I) = L
         L = L - 2
  220 CONTINUE
      GO TO 240
*
  240 RETURN
      END
      LOGICAL FUNCTION IVSAME(N,IX,IY)
*
*     LOGICAL FUNCTION  IVSAME  DETERMINES IF THE VECTORS  IX  AND  IY
*     AGREE EXACTLY WITH EACH OTHER.
*
*     .. Scalar Arguments ..
      INTEGER                 N
*     .. Array Arguments ..
      INTEGER                 IX(*), IY(*)
*     .. Local Scalars ..
      INTEGER                 I
*     .. Executable Statements ..
*
      IVSAME = .TRUE.
*
      IF (N.LE.0) RETURN
*
      DO 20 I = 1, N
         IF (IX(I).NE.IY(I)) THEN
            IVSAME = .FALSE.
            GO TO 40
         END IF
   20 CONTINUE
*
   40 RETURN
*
      END
      LOGICAL FUNCTION CVSAME(N,CX,CY)
*
*     LOGICAL FUNCTION  CVSAME  DETERMINES IF THE VECTORS  CX  AND  CY
*     AGREE EXACTLY WITH EACH OTHER.
*
*     .. Scalar Arguments ..
      INTEGER                 N
*     .. Array Arguments ..
      COMPLEX*16              CX(*), CY(*)
*     .. Local Scalars ..
      INTEGER                 I
*     .. Executable Statements ..
*
      CVSAME = .TRUE.
*
      DO 20 I = 1, N
         IF (CX(I).NE.CY(I)) THEN
            CVSAME = .FALSE.
            GO TO 40
         END IF
   20 CONTINUE
*
   40 RETURN
      END
