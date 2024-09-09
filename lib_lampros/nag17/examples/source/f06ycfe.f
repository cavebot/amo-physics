*     F06YCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*
*     Test program for the REAL             Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NSUBS
      PARAMETER        (NSUBS=6)
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
      INTEGER          NMAX
      PARAMETER        (NMAX=65)
      INTEGER          NIDMAX, NALMAX, NBEMAX
      PARAMETER        (NIDMAX=9,NALMAX=7,NBEMAX=7)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
      CHARACTER*13     SRNAMT
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, ERR, THRESH
      INTEGER          I, ISNUM, J, N, NALF, NBET, NIDIM, NTRA
      LOGICAL          FATAL, LTESTT, REWI, SAME, SFATAL, TRACE, TSTERR
      CHARACTER        TRANSA, TRANSB
      CHARACTER*6      SNAMET
*     .. Local Arrays ..
      DOUBLE PRECISION AA(NMAX*NMAX), AB(NMAX,2*NMAX), ALF(NALMAX),
     +                 AS(NMAX*NMAX), BB(NMAX*NMAX), BET(NBEMAX),
     +                 BS(NMAX*NMAX), C(NMAX,NMAX), CC(NMAX*NMAX),
     +                 CS(NMAX*NMAX), CT(NMAX), G(NMAX), W(2*NMAX)
      INTEGER          IDIM(NIDMAX)
      LOGICAL          LTEST(NSUBS)
      CHARACTER*13     SNAMES(NSUBS)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      LOGICAL          LSE
      EXTERNAL         X02AJF, LSE
*     .. External Subroutines ..
      EXTERNAL         SCHK1, SCHK2, SCHK3, SCHK4, SCHK5, SCHKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
      COMMON           /SRNAMC/SRNAMT
*     .. Data statements ..
      DATA             SNAMES/'F06YAF/DGEMM ', 'F06YCF/DSYMM ',
     +                 'F06YFF/DTRMM ', 'F06YJF/DTRSM ',
     +                 'F06YPF/DSYRK ', 'F06YRF/DSYR2K'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F06YCF Example Program Results'
      READ (NIN,*)
      NOUTC = NOUT
      NTRA = NOUT
      REWI = .FALSE.
*
*     Read flags
*
*     Read the flag that directs tracing of execution
      READ (NIN,*) TRACE
*     Read the flag that directs stopping on any failure.
      READ (NIN,*) SFATAL
*     Read the flag that indicates whether error exits are to be tested.
      READ (NIN,*) TSTERR
*     Read the threshold value of the test ratio
      READ (NIN,*) THRESH
*
*     Read and check the parameter values for the tests.
*
*     Values of N
      READ (NIN,*) NIDIM
      IF (NIDIM.LT.1 .OR. NIDIM.GT.NIDMAX) THEN
         WRITE (NOUT,99997) 'N', NIDMAX
         GO TO 400
      END IF
      READ (NIN,*) (IDIM(I),I=1,NIDIM)
      DO 20 I = 1, NIDIM
         IF (IDIM(I).LT.0 .OR. IDIM(I).GT.NMAX) THEN
            WRITE (NOUT,99996) NMAX
            GO TO 400
         END IF
   20 CONTINUE
*     Values of ALPHA
      READ (NIN,*) NALF
      IF (NALF.LT.1 .OR. NALF.GT.NALMAX) THEN
         WRITE (NOUT,99997) 'ALPHA', NALMAX
         GO TO 400
      END IF
      READ (NIN,*) (ALF(I),I=1,NALF)
*     Values of BETA
      READ (NIN,*) NBET
      IF (NBET.LT.1 .OR. NBET.GT.NBEMAX) THEN
         WRITE (NOUT,99997) 'BETA', NBEMAX
         GO TO 400
      END IF
      READ (NIN,*) (BET(I),I=1,NBET)
*
*     Report values of parameters.
*
      WRITE (NOUT,99995)
      WRITE (NOUT,99994) (IDIM(I),I=1,NIDIM)
      WRITE (NOUT,99993) (ALF(I),I=1,NALF)
      WRITE (NOUT,99992) (BET(I),I=1,NBET)
      IF ( .NOT. TSTERR) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99984)
      END IF
      WRITE (NOUT,*)
      WRITE (NOUT,99999) THRESH
      WRITE (NOUT,*)
*
*     Read names of subroutines and flags which indicate
*     whether they are to be tested.
*
      DO 40 I = 1, NSUBS
         LTEST(I) = .FALSE.
   40 CONTINUE
   60 READ (NIN,99988,END=120) SNAMET, LTESTT
      DO 80 I = 1, NSUBS
         IF (SNAMET.EQ.SNAMES(I)(1:6)) GO TO 100
   80 CONTINUE
      WRITE (NOUT,99990) SNAMET
      STOP
  100 LTEST(I) = LTESTT
      GO TO 60
*
  120 CONTINUE
      CLOSE (NIN)
*
*     Compute EPS (the machine precision).
*
      EPS = X02AJF()
      WRITE (NOUT,99998) EPS
*
*     Check the reliability of SMMCH using exact data.
*
      N = MIN(32,NMAX)
      DO 160 J = 1, N
         DO 140 I = 1, N
            AB(I,J) = MAX(I-J+1,0)
  140    CONTINUE
         AB(J,NMAX+1) = J
         AB(1,NMAX+J) = J
         C(J,1) = ZERO
  160 CONTINUE
      DO 180 J = 1, N
         CC(J) = J*((J+1)*J)/2 - ((J+1)*J*(J-1))/3
  180 CONTINUE
*     CC holds the exact result. On exit from SMMCH CT holds
*     the result computed by SMMCH.
      TRANSA = 'N'
      TRANSB = 'N'
      CALL SMMCH(TRANSA,TRANSB,N,1,N,ONE,AB,NMAX,AB(1,NMAX+1),NMAX,ZERO,
     +           C,NMAX,CT,G,CC,NMAX,EPS,ERR,FATAL,NOUT,.TRUE.)
      SAME = LSE(CC,CT,N)
      IF ( .NOT. SAME .OR. ERR.NE.ZERO) THEN
         WRITE (NOUT,99989) TRANSA, TRANSB, SAME, ERR
         STOP
      END IF
      TRANSB = 'T'
      CALL SMMCH(TRANSA,TRANSB,N,1,N,ONE,AB,NMAX,AB(1,NMAX+1),NMAX,ZERO,
     +           C,NMAX,CT,G,CC,NMAX,EPS,ERR,FATAL,NOUT,.TRUE.)
      SAME = LSE(CC,CT,N)
      IF ( .NOT. SAME .OR. ERR.NE.ZERO) THEN
         WRITE (NOUT,99989) TRANSA, TRANSB, SAME, ERR
         STOP
      END IF
      DO 200 J = 1, N
         AB(J,NMAX+1) = N - J + 1
         AB(1,NMAX+J) = N - J + 1
  200 CONTINUE
      DO 220 J = 1, N
         CC(N-J+1) = J*((J+1)*J)/2 - ((J+1)*J*(J-1))/3
  220 CONTINUE
      TRANSA = 'T'
      TRANSB = 'N'
      CALL SMMCH(TRANSA,TRANSB,N,1,N,ONE,AB,NMAX,AB(1,NMAX+1),NMAX,ZERO,
     +           C,NMAX,CT,G,CC,NMAX,EPS,ERR,FATAL,NOUT,.TRUE.)
      SAME = LSE(CC,CT,N)
      IF ( .NOT. SAME .OR. ERR.NE.ZERO) THEN
         WRITE (NOUT,99989) TRANSA, TRANSB, SAME, ERR
         STOP
      END IF
      TRANSB = 'T'
      CALL SMMCH(TRANSA,TRANSB,N,1,N,ONE,AB,NMAX,AB(1,NMAX+1),NMAX,ZERO,
     +           C,NMAX,CT,G,CC,NMAX,EPS,ERR,FATAL,NOUT,.TRUE.)
      SAME = LSE(CC,CT,N)
      IF ( .NOT. SAME .OR. ERR.NE.ZERO) THEN
         WRITE (NOUT,99989) TRANSA, TRANSB, SAME, ERR
         STOP
      END IF
*
*     Test each subroutine in turn.
*
      DO 360 ISNUM = 1, NSUBS
         WRITE (NOUT,*)
         IF ( .NOT. LTEST(ISNUM)) THEN
*           Subprogram is not to be tested.
            WRITE (NOUT,99987) SNAMES(ISNUM) (1:6)
         ELSE
            SRNAMT = SNAMES(ISNUM)
*           Test error exits.
            IF (TSTERR) THEN
               CALL SCHKE(ISNUM,SNAMES(ISNUM),NOUT)
               WRITE (NOUT,*)
            END IF
*           Test computations.
            INFOT = 0
            OK = .TRUE.
            FATAL = .FALSE.
            GO TO (240,260,280,280,300,320) ISNUM
*           Test F06YAF, 01.
  240       CALL SCHK1(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,AB,AA,AS,
     +                 AB(1,NMAX+1),BB,BS,C,CC,CS,CT,G)
            GO TO 340
*           Test F06YCF, 02.
  260       CALL SCHK2(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,AB,AA,AS,
     +                 AB(1,NMAX+1),BB,BS,C,CC,CS,CT,G)
            GO TO 340
*           Test F06YFF, 03, F06YJF, 04.
  280       CALL SCHK3(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NMAX,AB,AA,AS,
     +                 AB(1,NMAX+1),BB,BS,CT,G,C)
            GO TO 340
*           Test F06YPF, 05.
  300       CALL SCHK4(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,AB,AA,AS,
     +                 AB(1,NMAX+1),BB,BS,C,CC,CS,CT,G)
            GO TO 340
*           Test F06YRF, 06.
  320       CALL SCHK5(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,AB,AA,AS,
     +                 BB,BS,C,CC,CS,CT,G,W)
            GO TO 340
*
  340       IF (FATAL .AND. SFATAL) GO TO 380
         END IF
  360 CONTINUE
      WRITE (NOUT,99986)
      GO TO 420
*
  380 CONTINUE
      WRITE (NOUT,99985)
      GO TO 420
*
  400 CONTINUE
      WRITE (NOUT,99991)
*
  420 CONTINUE
      STOP
*
*
*     End of SBLAT3.
*
99999 FORMAT (' ROUTINES PASS COMPUTATIONAL TESTS IF TEST RATIO IS LES',
     +       'S THAN',F8.2)
99998 FORMAT (' RELATIVE MACHINE PRECISION IS TAKEN TO BE',1P,D9.1)
99997 FORMAT (' NUMBER OF VALUES OF ',A,' IS LESS THAN 1 OR GREATER TH',
     +       'AN ',I2)
99996 FORMAT (' VALUE OF N IS LESS THAN 0 OR GREATER THAN ',I2)
99995 FORMAT (' TESTS OF THE REAL             LEVEL 3 BLAS',//' THE FO',
     +       'LLOWING PARAMETER VALUES WILL BE USED:')
99994 FORMAT ('   FOR N              ',9I6)
99993 FORMAT ('   FOR ALPHA          ',7F6.1)
99992 FORMAT ('   FOR BETA           ',7F6.1)
99991 FORMAT (' AMEND DATA FILE OR INCREASE ARRAY SIZES IN PROGRAM',
     +       /' ******* TESTS ABANDONED *******')
99990 FORMAT (' SUBPROGRAM NAME ',A6,' NOT RECOGNIZED',/' ******* TEST',
     +       'S ABANDONED *******')
99989 FORMAT (' ERROR IN SMMCH -  IN-LINE DOT PRODUCTS ARE BEING EVALU',
     +       'ATED WRONGLY.',/' SMMCH WAS CALLED WITH TRANSA = ',A1,
     +       ' AND TRANSB = ',A1,/' AND RETURNED SAME = ',L1,' AND ERR',
     +       ' = ',F12.3,'.',/' THIS MAY BE DUE TO FAULTS IN THE ARITH',
     +       'METIC OR THE COMPILER.',/' ******* TESTS ABANDONED *****',
     +       '**')
99988 FORMAT (A6,L2)
99987 FORMAT (1X,A6,' WAS NOT TESTED')
99986 FORMAT (/' END OF TESTS')
99985 FORMAT (/' ******* FATAL ERROR - TESTS ABANDONED *******')
99984 FORMAT (' ERROR-EXITS WILL NOT BE TESTED')
      END
      SUBROUTINE SCHK1(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,A,AA,AS,B,BB,
     +                 BS,C,CC,CS,CT,G)
*
*     Tests F06YAF.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          NALF, NBET, NIDIM, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      DOUBLE PRECISION A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), B(NMAX,NMAX), BB(NMAX*NMAX),
     +                 BET(NBET), BS(NMAX*NMAX), C(NMAX,NMAX),
     +                 CC(NMAX*NMAX), CS(NMAX*NMAX), CT(NMAX), G(NMAX)
      INTEGER          IDIM(NIDIM)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ALS, BETA, BLS, ERR, ERRMAX
      INTEGER          I, IA, IB, ICA, ICB, IK, IM, IN, K, KS, LAA, LBB,
     +                 LCC, LDA, LDAS, LDB, LDBS, LDC, LDCS, M, MA, MB,
     +                 MS, N, NA, NARGS, NB, NC, NS
      LOGICAL          NULL, RESET, SAME, TRANA, TRANB
      CHARACTER        TRANAS, TRANBS, TRANSA, TRANSB
      CHARACTER*3      ICH
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LSE, LSERES
      EXTERNAL         LSE, LSERES
*     .. External Subroutines ..
      EXTERNAL         F06YAF, SMAKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICH/'NTC'/
*     .. Executable Statements ..
*
      NARGS = 13
      NC = 0
      RESET = .TRUE.
      ERRMAX = ZERO
*
      DO 220 IM = 1, NIDIM
         M = IDIM(IM)
*
         DO 200 IN = 1, NIDIM
            N = IDIM(IN)
*           Set LDC to 1 more than minimum value if room.
            LDC = M
            IF (LDC.LT.NMAX) LDC = LDC + 1
*           Skip tests if not enough room.
            IF (LDC.GT.NMAX) GO TO 200
            LCC = LDC*N
            NULL = N .LE. 0 .OR. M .LE. 0
*
            DO 180 IK = 1, NIDIM
               K = IDIM(IK)
*
               DO 160 ICA = 1, 3
                  TRANSA = ICH(ICA:ICA)
                  TRANA = TRANSA .EQ. 'T' .OR. TRANSA .EQ. 'C'
*
                  IF (TRANA) THEN
                     MA = K
                     NA = M
                  ELSE
                     MA = M
                     NA = K
                  END IF
*                 Set LDA to 1 more than minimum value if room.
                  LDA = MA
                  IF (LDA.LT.NMAX) LDA = LDA + 1
*                 Skip tests if not enough room.
                  IF (LDA.GT.NMAX) GO TO 160
                  LAA = LDA*NA
*
*                 Generate the matrix A.
*
                  CALL SMAKE('GE',' ',' ',MA,NA,A,NMAX,AA,LDA,RESET,
     +                       ZERO)
*
                  DO 140 ICB = 1, 3
                     TRANSB = ICH(ICB:ICB)
                     TRANB = TRANSB .EQ. 'T' .OR. TRANSB .EQ. 'C'
*
                     IF (TRANB) THEN
                        MB = N
                        NB = K
                     ELSE
                        MB = K
                        NB = N
                     END IF
*                    Set LDB to 1 more than minimum value if room.
                     LDB = MB
                     IF (LDB.LT.NMAX) LDB = LDB + 1
*                    Skip tests if not enough room.
                     IF (LDB.GT.NMAX) GO TO 140
                     LBB = LDB*NB
*
*                    Generate the matrix B.
*
                     CALL SMAKE('GE',' ',' ',MB,NB,B,NMAX,BB,LDB,RESET,
     +                          ZERO)
*
                     DO 120 IA = 1, NALF
                        ALPHA = ALF(IA)
*
                        DO 100 IB = 1, NBET
                           BETA = BET(IB)
*
*                          Generate the matrix C.
*
                           CALL SMAKE('GE',' ',' ',M,N,C,NMAX,CC,LDC,
     +                                RESET,ZERO)
*
                           NC = NC + 1
*
*                          Save every datum before calling the
*                          subroutine.
*
                           TRANAS = TRANSA
                           TRANBS = TRANSB
                           MS = M
                           NS = N
                           KS = K
                           ALS = ALPHA
                           DO 20 I = 1, LAA
                              AS(I) = AA(I)
   20                      CONTINUE
                           LDAS = LDA
                           DO 40 I = 1, LBB
                              BS(I) = BB(I)
   40                      CONTINUE
                           LDBS = LDB
                           BLS = BETA
                           DO 60 I = 1, LCC
                              CS(I) = CC(I)
   60                      CONTINUE
                           LDCS = LDC
*
*                          Call the subroutine.
*
                           IF (TRACE) WRITE (NTRA,99995) NC, SNAME(1:6),
     +                         TRANSA, TRANSB, M, N, K, ALPHA, LDA, LDB,
     +                         BETA, LDC
                           CALL F06YAF(TRANSA,TRANSB,M,N,K,ALPHA,AA,LDA,
     +                                 BB,LDB,BETA,CC,LDC)
*
*                          Check if error-exit was taken incorrectly.
*
                           IF ( .NOT. OK) THEN
                              WRITE (NOUT,99994)
                              FATAL = .TRUE.
                              GO TO 240
                           END IF
*
*                          See what data changed inside subroutines.
*
                           ISAME(1) = TRANSA .EQ. TRANAS
                           ISAME(2) = TRANSB .EQ. TRANBS
                           ISAME(3) = MS .EQ. M
                           ISAME(4) = NS .EQ. N
                           ISAME(5) = KS .EQ. K
                           ISAME(6) = ALS .EQ. ALPHA
                           ISAME(7) = LSE(AS,AA,LAA)
                           ISAME(8) = LDAS .EQ. LDA
                           ISAME(9) = LSE(BS,BB,LBB)
                           ISAME(10) = LDBS .EQ. LDB
                           ISAME(11) = BLS .EQ. BETA
                           IF (NULL) THEN
                              ISAME(12) = LSE(CS,CC,LCC)
                           ELSE
                              ISAME(12) = LSERES('GE',' ',M,N,CS,CC,LDC)
                           END IF
                           ISAME(13) = LDCS .EQ. LDC
*
*                          If data was incorrectly changed, report
*                          and return.
*
                           SAME = .TRUE.
                           DO 80 I = 1, NARGS
                              SAME = SAME .AND. ISAME(I)
                              IF ( .NOT. ISAME(I))
     +                            WRITE (NOUT,99998) I
   80                      CONTINUE
                           IF ( .NOT. SAME) THEN
                              FATAL = .TRUE.
                              GO TO 240
                           END IF
*
                           IF ( .NOT. NULL) THEN
*
*                             Check the result.
*
                              CALL SMMCH(TRANSA,TRANSB,M,N,K,ALPHA,A,
     +                                   NMAX,B,NMAX,BETA,C,NMAX,CT,G,
     +                                   CC,LDC,EPS,ERR,FATAL,NOUT,
     +                                   .TRUE.)
                              ERRMAX = MAX(ERRMAX,ERR)
*                             If got really bad answer, report and
*                             return.
                              IF (FATAL) GO TO 240
                           END IF
*
  100                   CONTINUE
*
  120                CONTINUE
*
  140             CONTINUE
*
  160          CONTINUE
*
  180       CONTINUE
*
  200    CONTINUE
*
  220 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 260
*
  240 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99995) NC, SNAME(1:6), TRANSA, TRANSB, M, N, K, ALPHA,
     +  LDA, LDB, BETA, LDC
*
  260 CONTINUE
      RETURN
*
*
*     End of SCHK1.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(''',A1,''',''',A1,''',',3(I3,','),F4.1,
     +       ', A,',I3,', B,',I3,',',F4.1,', C,',I3,').')
99994 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE SCHK2(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,A,AA,AS,B,BB,
     +                 BS,C,CC,CS,CT,G)
*
*     Tests F06YCF.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          NALF, NBET, NIDIM, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      DOUBLE PRECISION A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), B(NMAX,NMAX), BB(NMAX*NMAX),
     +                 BET(NBET), BS(NMAX*NMAX), C(NMAX,NMAX),
     +                 CC(NMAX*NMAX), CS(NMAX*NMAX), CT(NMAX), G(NMAX)
      INTEGER          IDIM(NIDIM)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ALS, BETA, BLS, ERR, ERRMAX
      INTEGER          I, IA, IB, ICS, ICU, IM, IN, LAA, LBB, LCC, LDA,
     +                 LDAS, LDB, LDBS, LDC, LDCS, M, MS, N, NA, NARGS,
     +                 NC, NS
      LOGICAL          LEFT, NULL, RESET, SAME
      CHARACTER        SIDE, SIDES, UPLO, UPLOS
      CHARACTER*2      ICHS, ICHU
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LSE, LSERES
      EXTERNAL         LSE, LSERES
*     .. External Subroutines ..
      EXTERNAL         F06YCF, SMAKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICHS/'LR'/, ICHU/'UL'/
*     .. Executable Statements ..
*
      NARGS = 12
      NC = 0
      RESET = .TRUE.
      ERRMAX = ZERO
*
      DO 200 IM = 1, NIDIM
         M = IDIM(IM)
*
         DO 180 IN = 1, NIDIM
            N = IDIM(IN)
*           Set LDC to 1 more than minimum value if room.
            LDC = M
            IF (LDC.LT.NMAX) LDC = LDC + 1
*           Skip tests if not enough room.
            IF (LDC.GT.NMAX) GO TO 180
            LCC = LDC*N
            NULL = N .LE. 0 .OR. M .LE. 0
*
*           Set LDB to 1 more than minimum value if room.
            LDB = M
            IF (LDB.LT.NMAX) LDB = LDB + 1
*           Skip tests if not enough room.
            IF (LDB.GT.NMAX) GO TO 180
            LBB = LDB*N
*
*           Generate the matrix B.
*
            CALL SMAKE('GE',' ',' ',M,N,B,NMAX,BB,LDB,RESET,ZERO)
*
            DO 160 ICS = 1, 2
               SIDE = ICHS(ICS:ICS)
               LEFT = SIDE .EQ. 'L'
*
               IF (LEFT) THEN
                  NA = M
               ELSE
                  NA = N
               END IF
*              Set LDA to 1 more than minimum value if room.
               LDA = NA
               IF (LDA.LT.NMAX) LDA = LDA + 1
*              Skip tests if not enough room.
               IF (LDA.GT.NMAX) GO TO 160
               LAA = LDA*NA
*
               DO 140 ICU = 1, 2
                  UPLO = ICHU(ICU:ICU)
*
*                 Generate the symmetric matrix A.
*
                  CALL SMAKE('SY',UPLO,' ',NA,NA,A,NMAX,AA,LDA,RESET,
     +                       ZERO)
*
                  DO 120 IA = 1, NALF
                     ALPHA = ALF(IA)
*
                     DO 100 IB = 1, NBET
                        BETA = BET(IB)
*
*                       Generate the matrix C.
*
                        CALL SMAKE('GE',' ',' ',M,N,C,NMAX,CC,LDC,RESET,
     +                             ZERO)
*
                        NC = NC + 1
*
*                       Save every datum before calling the
*                       subroutine.
*
                        SIDES = SIDE
                        UPLOS = UPLO
                        MS = M
                        NS = N
                        ALS = ALPHA
                        DO 20 I = 1, LAA
                           AS(I) = AA(I)
   20                   CONTINUE
                        LDAS = LDA
                        DO 40 I = 1, LBB
                           BS(I) = BB(I)
   40                   CONTINUE
                        LDBS = LDB
                        BLS = BETA
                        DO 60 I = 1, LCC
                           CS(I) = CC(I)
   60                   CONTINUE
                        LDCS = LDC
*
*                       Call the subroutine.
*
                        IF (TRACE) WRITE (NTRA,99995) NC, SNAME(1:6),
     +                      SIDE, UPLO, M, N, ALPHA, LDA, LDB, BETA, LDC
                        CALL F06YCF(SIDE,UPLO,M,N,ALPHA,AA,LDA,BB,LDB,
     +                              BETA,CC,LDC)
*
*                       Check if error-exit was taken incorrectly.
*
                        IF ( .NOT. OK) THEN
                           WRITE (NOUT,99994)
                           FATAL = .TRUE.
                           GO TO 220
                        END IF
*
*                       See what data changed inside subroutines.
*
                        ISAME(1) = SIDES .EQ. SIDE
                        ISAME(2) = UPLOS .EQ. UPLO
                        ISAME(3) = MS .EQ. M
                        ISAME(4) = NS .EQ. N
                        ISAME(5) = ALS .EQ. ALPHA
                        ISAME(6) = LSE(AS,AA,LAA)
                        ISAME(7) = LDAS .EQ. LDA
                        ISAME(8) = LSE(BS,BB,LBB)
                        ISAME(9) = LDBS .EQ. LDB
                        ISAME(10) = BLS .EQ. BETA
                        IF (NULL) THEN
                           ISAME(11) = LSE(CS,CC,LCC)
                        ELSE
                           ISAME(11) = LSERES('GE',' ',M,N,CS,CC,LDC)
                        END IF
                        ISAME(12) = LDCS .EQ. LDC
*
*                       If data was incorrectly changed, report and
*                       return.
*
                        SAME = .TRUE.
                        DO 80 I = 1, NARGS
                           SAME = SAME .AND. ISAME(I)
                           IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   80                   CONTINUE
                        IF ( .NOT. SAME) THEN
                           FATAL = .TRUE.
                           GO TO 220
                        END IF
*
                        IF ( .NOT. NULL) THEN
*
*                          Check the result.
*
                           IF (LEFT) THEN
                              CALL SMMCH('N','N',M,N,M,ALPHA,A,NMAX,B,
     +                                   NMAX,BETA,C,NMAX,CT,G,CC,LDC,
     +                                   EPS,ERR,FATAL,NOUT,.TRUE.)
                           ELSE
                              CALL SMMCH('N','N',M,N,N,ALPHA,B,NMAX,A,
     +                                   NMAX,BETA,C,NMAX,CT,G,CC,LDC,
     +                                   EPS,ERR,FATAL,NOUT,.TRUE.)
                           END IF
                           ERRMAX = MAX(ERRMAX,ERR)
*                          If got really bad answer, report and
*                          return.
                           IF (FATAL) GO TO 220
                        END IF
*
  100                CONTINUE
*
  120             CONTINUE
*
  140          CONTINUE
*
  160       CONTINUE
*
  180    CONTINUE
*
  200 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 240
*
  220 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99995) NC, SNAME(1:6), SIDE, UPLO, M, N, ALPHA, LDA,
     +  LDB, BETA, LDC
*
  240 CONTINUE
      RETURN
*
*
*     End of SCHK2.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(',2('''',A1,''','),2(I3,','),F4.1,', A,',
     +       I3,', B,',I3,',',F4.1,', C,',I3,')    .')
99994 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE SCHK3(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NMAX,A,AA,AS,B,BB,BS,CT,G,C)
*
*     Tests F06YFF and F06YJF.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          NALF, NIDIM, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      DOUBLE PRECISION A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), B(NMAX,NMAX), BB(NMAX*NMAX),
     +                 BS(NMAX*NMAX), C(NMAX,NMAX), CT(NMAX), G(NMAX)
      INTEGER          IDIM(NIDIM)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ALS, ERR, ERRMAX
      INTEGER          I, IA, ICD, ICS, ICT, ICU, IM, IN, J, LAA, LBB,
     +                 LDA, LDAS, LDB, LDBS, M, MS, N, NA, NARGS, NC, NS
      LOGICAL          LEFT, NULL, RESET, SAME
      CHARACTER        DIAG, DIAGS, SIDE, SIDES, TRANAS, TRANSA, UPLO,
     +                 UPLOS
      CHARACTER*2      ICHD, ICHS, ICHU
      CHARACTER*3      ICHT
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LSE, LSERES
      EXTERNAL         LSE, LSERES
*     .. External Subroutines ..
      EXTERNAL         F06YFF, F06YJF, SMAKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICHU/'UL'/, ICHT/'NTC'/, ICHD/'UN'/, ICHS/'LR'/
*     .. Executable Statements ..
*
      NARGS = 11
      NC = 0
      RESET = .TRUE.
      ERRMAX = ZERO
*     Set up zero matrix for SMMCH.
      DO 40 J = 1, NMAX
         DO 20 I = 1, NMAX
            C(I,J) = ZERO
   20    CONTINUE
   40 CONTINUE
*
      DO 280 IM = 1, NIDIM
         M = IDIM(IM)
*
         DO 260 IN = 1, NIDIM
            N = IDIM(IN)
*           Set LDB to 1 more than minimum value if room.
            LDB = M
            IF (LDB.LT.NMAX) LDB = LDB + 1
*           Skip tests if not enough room.
            IF (LDB.GT.NMAX) GO TO 260
            LBB = LDB*N
            NULL = M .LE. 0 .OR. N .LE. 0
*
            DO 240 ICS = 1, 2
               SIDE = ICHS(ICS:ICS)
               LEFT = SIDE .EQ. 'L'
               IF (LEFT) THEN
                  NA = M
               ELSE
                  NA = N
               END IF
*              Set LDA to 1 more than minimum value if room.
               LDA = NA
               IF (LDA.LT.NMAX) LDA = LDA + 1
*              Skip tests if not enough room.
               IF (LDA.GT.NMAX) GO TO 260
               LAA = LDA*NA
*
               DO 220 ICU = 1, 2
                  UPLO = ICHU(ICU:ICU)
*
                  DO 200 ICT = 1, 3
                     TRANSA = ICHT(ICT:ICT)
*
                     DO 180 ICD = 1, 2
                        DIAG = ICHD(ICD:ICD)
*
                        DO 160 IA = 1, NALF
                           ALPHA = ALF(IA)
*
*                          Generate the matrix A.
*
                           CALL SMAKE('TR',UPLO,DIAG,NA,NA,A,NMAX,AA,
     +                                LDA,RESET,ZERO)
*
*                          Generate the matrix B.
*
                           CALL SMAKE('GE',' ',' ',M,N,B,NMAX,BB,LDB,
     +                                RESET,ZERO)
*
                           NC = NC + 1
*
*                          Save every datum before calling the
*                          subroutine.
*
                           SIDES = SIDE
                           UPLOS = UPLO
                           TRANAS = TRANSA
                           DIAGS = DIAG
                           MS = M
                           NS = N
                           ALS = ALPHA
                           DO 60 I = 1, LAA
                              AS(I) = AA(I)
   60                      CONTINUE
                           LDAS = LDA
                           DO 80 I = 1, LBB
                              BS(I) = BB(I)
   80                      CONTINUE
                           LDBS = LDB
*
*                          Call the subroutine.
*
                           IF (SNAME(11:12).EQ.'MM') THEN
                              IF (TRACE) WRITE (NTRA,99995) NC,
     +                            SNAME(1:6), SIDE, UPLO, TRANSA, DIAG,
     +                            M, N, ALPHA, LDA, LDB
                              CALL F06YFF(SIDE,UPLO,TRANSA,DIAG,M,N,
     +                                    ALPHA,AA,LDA,BB,LDB)
                           ELSE IF (SNAME(11:12).EQ.'SM') THEN
                              IF (TRACE) WRITE (NTRA,99995) NC,
     +                            SNAME(1:6), SIDE, UPLO, TRANSA, DIAG,
     +                            M, N, ALPHA, LDA, LDB
                              CALL F06YJF(SIDE,UPLO,TRANSA,DIAG,M,N,
     +                                    ALPHA,AA,LDA,BB,LDB)
                           END IF
*
*                          Check if error-exit was taken incorrectly.
*
                           IF ( .NOT. OK) THEN
                              WRITE (NOUT,99994)
                              FATAL = .TRUE.
                              GO TO 300
                           END IF
*
*                          See what data changed inside subroutines.
*
                           ISAME(1) = SIDES .EQ. SIDE
                           ISAME(2) = UPLOS .EQ. UPLO
                           ISAME(3) = TRANAS .EQ. TRANSA
                           ISAME(4) = DIAGS .EQ. DIAG
                           ISAME(5) = MS .EQ. M
                           ISAME(6) = NS .EQ. N
                           ISAME(7) = ALS .EQ. ALPHA
                           ISAME(8) = LSE(AS,AA,LAA)
                           ISAME(9) = LDAS .EQ. LDA
                           IF (NULL) THEN
                              ISAME(10) = LSE(BS,BB,LBB)
                           ELSE
                              ISAME(10) = LSERES('GE',' ',M,N,BS,BB,LDB)
                           END IF
                           ISAME(11) = LDBS .EQ. LDB
*
*                          If data was incorrectly changed, report and
*                          return.
*
                           SAME = .TRUE.
                           DO 100 I = 1, NARGS
                              SAME = SAME .AND. ISAME(I)
                              IF ( .NOT. ISAME(I))
     +                            WRITE (NOUT,99998) I
  100                      CONTINUE
                           IF ( .NOT. SAME) THEN
                              FATAL = .TRUE.
                              GO TO 300
                           END IF
*
                           IF ( .NOT. NULL) THEN
                              IF (SNAME(11:12).EQ.'MM') THEN
*
*                                Check the result.
*
                                 IF (LEFT) THEN
                                    CALL SMMCH(TRANSA,'N',M,N,M,ALPHA,A,
     +                                         NMAX,B,NMAX,ZERO,C,NMAX,
     +                                         CT,G,BB,LDB,EPS,ERR,
     +                                         FATAL,NOUT,.TRUE.)
                                 ELSE
                                    CALL SMMCH('N',TRANSA,M,N,N,ALPHA,B,
     +                                         NMAX,A,NMAX,ZERO,C,NMAX,
     +                                         CT,G,BB,LDB,EPS,ERR,
     +                                         FATAL,NOUT,.TRUE.)
                                 END IF
                              ELSE IF (SNAME(11:12).EQ.'SM') THEN
*
*                                Compute approximation to original
*                                matrix.
*
                                 DO 140 J = 1, N
                                    DO 120 I = 1, M
                                       C(I,J) = BB(I+(J-1)*LDB)
                                       BB(I+(J-1)*LDB) = ALPHA*B(I,J)
  120                               CONTINUE
  140                            CONTINUE
*
                                 IF (LEFT) THEN
                                    CALL SMMCH(TRANSA,'N',M,N,M,ONE,A,
     +                                         NMAX,C,NMAX,ZERO,B,NMAX,
     +                                         CT,G,BB,LDB,EPS,ERR,
     +                                         FATAL,NOUT,.FALSE.)
                                 ELSE
                                    CALL SMMCH('N',TRANSA,M,N,N,ONE,C,
     +                                         NMAX,A,NMAX,ZERO,B,NMAX,
     +                                         CT,G,BB,LDB,EPS,ERR,
     +                                         FATAL,NOUT,.FALSE.)
                                 END IF
                              END IF
                              ERRMAX = MAX(ERRMAX,ERR)
*                             If got really bad answer, report and
*                             return.
                              IF (FATAL) GO TO 300
                           END IF
*
  160                   CONTINUE
*
  180                CONTINUE
*
  200             CONTINUE
*
  220          CONTINUE
*
  240       CONTINUE
*
  260    CONTINUE
*
  280 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 320
*
  300 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99995) NC, SNAME(1:6), SIDE, UPLO, TRANSA, DIAG, M, N,
     +  ALPHA, LDA, LDB
*
  320 CONTINUE
      RETURN
*
*
*     End of SCHK3.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(',4('''',A1,''','),2(I3,','),F4.1,', A,',
     +       I3,', B,',I3,')        .')
99994 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE SCHK4(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,A,AA,AS,B,BB,
     +                 BS,C,CC,CS,CT,G)
*
*     Tests F06YPF.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          NALF, NBET, NIDIM, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      DOUBLE PRECISION A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), B(NMAX,NMAX), BB(NMAX*NMAX),
     +                 BET(NBET), BS(NMAX*NMAX), C(NMAX,NMAX),
     +                 CC(NMAX*NMAX), CS(NMAX*NMAX), CT(NMAX), G(NMAX)
      INTEGER          IDIM(NIDIM)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ALS, BETA, BETS, ERR, ERRMAX
      INTEGER          I, IA, IB, ICT, ICU, IK, IN, J, JC, JJ, K, KS,
     +                 LAA, LCC, LDA, LDAS, LDC, LDCS, LJ, MA, N, NA,
     +                 NARGS, NC, NS
      LOGICAL          NULL, RESET, SAME, TRAN, UPPER
      CHARACTER        TRANS, TRANSS, UPLO, UPLOS
      CHARACTER*2      ICHU
      CHARACTER*3      ICHT
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LSE, LSERES
      EXTERNAL         LSE, LSERES
*     .. External Subroutines ..
      EXTERNAL         F06YPF, SMAKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICHT/'NTC'/, ICHU/'UL'/
*     .. Executable Statements ..
*
      NARGS = 10
      NC = 0
      RESET = .TRUE.
      ERRMAX = ZERO
*
      DO 200 IN = 1, NIDIM
         N = IDIM(IN)
*        Set LDC to 1 more than minimum value if room.
         LDC = N
         IF (LDC.LT.NMAX) LDC = LDC + 1
*        Skip tests if not enough room.
         IF (LDC.GT.NMAX) GO TO 200
         LCC = LDC*N
         NULL = N .LE. 0
*
         DO 180 IK = 1, NIDIM
            K = IDIM(IK)
*
            DO 160 ICT = 1, 3
               TRANS = ICHT(ICT:ICT)
               TRAN = TRANS .EQ. 'T' .OR. TRANS .EQ. 'C'
               IF (TRAN) THEN
                  MA = K
                  NA = N
               ELSE
                  MA = N
                  NA = K
               END IF
*              Set LDA to 1 more than minimum value if room.
               LDA = MA
               IF (LDA.LT.NMAX) LDA = LDA + 1
*              Skip tests if not enough room.
               IF (LDA.GT.NMAX) GO TO 160
               LAA = LDA*NA
*
*              Generate the matrix A.
*
               CALL SMAKE('GE',' ',' ',MA,NA,A,NMAX,AA,LDA,RESET,ZERO)
*
               DO 140 ICU = 1, 2
                  UPLO = ICHU(ICU:ICU)
                  UPPER = UPLO .EQ. 'U'
*
                  DO 120 IA = 1, NALF
                     ALPHA = ALF(IA)
*
                     DO 100 IB = 1, NBET
                        BETA = BET(IB)
*
*                       Generate the matrix C.
*
                        CALL SMAKE('SY',UPLO,' ',N,N,C,NMAX,CC,LDC,
     +                             RESET,ZERO)
*
                        NC = NC + 1
*
*                       Save every datum before calling the subroutine.
*
                        UPLOS = UPLO
                        TRANSS = TRANS
                        NS = N
                        KS = K
                        ALS = ALPHA
                        DO 20 I = 1, LAA
                           AS(I) = AA(I)
   20                   CONTINUE
                        LDAS = LDA
                        BETS = BETA
                        DO 40 I = 1, LCC
                           CS(I) = CC(I)
   40                   CONTINUE
                        LDCS = LDC
*
*                       Call the subroutine.
*
                        IF (TRACE) WRITE (NTRA,99994) NC, SNAME(1:6),
     +                      UPLO, TRANS, N, K, ALPHA, LDA, BETA, LDC
                        CALL F06YPF(UPLO,TRANS,N,K,ALPHA,AA,LDA,BETA,CC,
     +                              LDC)
*
*                       Check if error-exit was taken incorrectly.
*
                        IF ( .NOT. OK) THEN
                           WRITE (NOUT,99993)
                           FATAL = .TRUE.
                           GO TO 240
                        END IF
*
*                       See what data changed inside subroutines.
*
                        ISAME(1) = UPLOS .EQ. UPLO
                        ISAME(2) = TRANSS .EQ. TRANS
                        ISAME(3) = NS .EQ. N
                        ISAME(4) = KS .EQ. K
                        ISAME(5) = ALS .EQ. ALPHA
                        ISAME(6) = LSE(AS,AA,LAA)
                        ISAME(7) = LDAS .EQ. LDA
                        ISAME(8) = BETS .EQ. BETA
                        IF (NULL) THEN
                           ISAME(9) = LSE(CS,CC,LCC)
                        ELSE
                           ISAME(9) = LSERES('SY',UPLO,N,N,CS,CC,LDC)
                        END IF
                        ISAME(10) = LDCS .EQ. LDC
*
*                       If data was incorrectly changed, report and
*                       return.
*
                        SAME = .TRUE.
                        DO 60 I = 1, NARGS
                           SAME = SAME .AND. ISAME(I)
                           IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   60                   CONTINUE
                        IF ( .NOT. SAME) THEN
                           FATAL = .TRUE.
                           GO TO 240
                        END IF
*
                        IF ( .NOT. NULL) THEN
*
*                          Check the result column by column.
*
                           JC = 1
                           DO 80 J = 1, N
                              IF (UPPER) THEN
                                 JJ = 1
                                 LJ = J
                              ELSE
                                 JJ = J
                                 LJ = N - J + 1
                              END IF
                              IF (TRAN) THEN
                                 CALL SMMCH('T','N',LJ,1,K,ALPHA,A(1,JJ)
     +                                      ,NMAX,A(1,J),NMAX,BETA,
     +                                      C(JJ,J),NMAX,CT,G,CC(JC),
     +                                      LDC,EPS,ERR,FATAL,NOUT,
     +                                      .TRUE.)
                              ELSE
                                 CALL SMMCH('N','T',LJ,1,K,ALPHA,A(JJ,1)
     +                                      ,NMAX,A(J,1),NMAX,BETA,
     +                                      C(JJ,J),NMAX,CT,G,CC(JC),
     +                                      LDC,EPS,ERR,FATAL,NOUT,
     +                                      .TRUE.)
                              END IF
                              IF (UPPER) THEN
                                 JC = JC + LDC
                              ELSE
                                 JC = JC + LDC + 1
                              END IF
                              ERRMAX = MAX(ERRMAX,ERR)
*                             If got really bad answer, report and
*                             return.
                              IF (FATAL) GO TO 220
   80                      CONTINUE
                        END IF
*
  100                CONTINUE
*
  120             CONTINUE
*
  140          CONTINUE
*
  160       CONTINUE
*
  180    CONTINUE
*
  200 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 260
*
  220 CONTINUE
      IF (N.GT.1) WRITE (NOUT,99995) J
*
  240 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, TRANS, N, K, ALPHA, LDA,
     +  BETA, LDC
*
  260 CONTINUE
      RETURN
*
*
*     End of SCHK4.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
99994 FORMAT (1X,I6,': ',A6,'(',2('''',A1,''','),2(I3,','),F4.1,', A,',
     +       I3,',',F4.1,', C,',I3,')           .')
99993 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE SCHK5(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NBET,BET,NMAX,AB,AA,AS,BB,BS,
     +                 C,CC,CS,CT,G,W)
*
*     Tests F06YRF.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          NALF, NBET, NIDIM, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      DOUBLE PRECISION AA(NMAX*NMAX), AB(2*NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), BB(NMAX*NMAX), BET(NBET),
     +                 BS(NMAX*NMAX), C(NMAX,NMAX), CC(NMAX*NMAX),
     +                 CS(NMAX*NMAX), CT(NMAX), G(NMAX), W(2*NMAX)
      INTEGER          IDIM(NIDIM)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ALS, BETA, BETS, ERR, ERRMAX
      INTEGER          I, IA, IB, ICT, ICU, IK, IN, J, JC, JJ, JJAB, K,
     +                 KS, LAA, LBB, LCC, LDA, LDAS, LDB, LDBS, LDC,
     +                 LDCS, LJ, MA, N, NA, NARGS, NC, NS
      LOGICAL          NULL, RESET, SAME, TRAN, UPPER
      CHARACTER        TRANS, TRANSS, UPLO, UPLOS
      CHARACTER*2      ICHU
      CHARACTER*3      ICHT
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LSE, LSERES
      EXTERNAL         LSE, LSERES
*     .. External Subroutines ..
      EXTERNAL         F06YRF, SMAKE, SMMCH
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICHT/'NTC'/, ICHU/'UL'/
*     .. Executable Statements ..
*
      NARGS = 12
      NC = 0
      RESET = .TRUE.
      ERRMAX = ZERO
*
      DO 260 IN = 1, NIDIM
         N = IDIM(IN)
*        Set LDC to 1 more than minimum value if room.
         LDC = N
         IF (LDC.LT.NMAX) LDC = LDC + 1
*        Skip tests if not enough room.
         IF (LDC.GT.NMAX) GO TO 260
         LCC = LDC*N
         NULL = N .LE. 0
*
         DO 240 IK = 1, NIDIM
            K = IDIM(IK)
*
            DO 220 ICT = 1, 3
               TRANS = ICHT(ICT:ICT)
               TRAN = TRANS .EQ. 'T' .OR. TRANS .EQ. 'C'
               IF (TRAN) THEN
                  MA = K
                  NA = N
               ELSE
                  MA = N
                  NA = K
               END IF
*              Set LDA to 1 more than minimum value if room.
               LDA = MA
               IF (LDA.LT.NMAX) LDA = LDA + 1
*              Skip tests if not enough room.
               IF (LDA.GT.NMAX) GO TO 220
               LAA = LDA*NA
*
*              Generate the matrix A.
*
               IF (TRAN) THEN
                  CALL SMAKE('GE',' ',' ',MA,NA,AB,2*NMAX,AA,LDA,RESET,
     +                       ZERO)
               ELSE
                  CALL SMAKE('GE',' ',' ',MA,NA,AB,NMAX,AA,LDA,RESET,
     +                       ZERO)
               END IF
*
*              Generate the matrix B.
*
               LDB = LDA
               LBB = LAA
               IF (TRAN) THEN
                  CALL SMAKE('GE',' ',' ',MA,NA,AB(K+1),2*NMAX,BB,LDB,
     +                       RESET,ZERO)
               ELSE
                  CALL SMAKE('GE',' ',' ',MA,NA,AB(K*NMAX+1),NMAX,BB,
     +                       LDB,RESET,ZERO)
               END IF
*
               DO 200 ICU = 1, 2
                  UPLO = ICHU(ICU:ICU)
                  UPPER = UPLO .EQ. 'U'
*
                  DO 180 IA = 1, NALF
                     ALPHA = ALF(IA)
*
                     DO 160 IB = 1, NBET
                        BETA = BET(IB)
*
*                       Generate the matrix C.
*
                        CALL SMAKE('SY',UPLO,' ',N,N,C,NMAX,CC,LDC,
     +                             RESET,ZERO)
*
                        NC = NC + 1
*
*                       Save every datum before calling the subroutine.
*
                        UPLOS = UPLO
                        TRANSS = TRANS
                        NS = N
                        KS = K
                        ALS = ALPHA
                        DO 20 I = 1, LAA
                           AS(I) = AA(I)
   20                   CONTINUE
                        LDAS = LDA
                        DO 40 I = 1, LBB
                           BS(I) = BB(I)
   40                   CONTINUE
                        LDBS = LDB
                        BETS = BETA
                        DO 60 I = 1, LCC
                           CS(I) = CC(I)
   60                   CONTINUE
                        LDCS = LDC
*
*                       Call the subroutine.
*
                        IF (TRACE) WRITE (NTRA,99994) NC, SNAME(1:6),
     +                      UPLO, TRANS, N, K, ALPHA, LDA, LDB, BETA,
     +                      LDC
                        CALL F06YRF(UPLO,TRANS,N,K,ALPHA,AA,LDA,BB,LDB,
     +                              BETA,CC,LDC)
*
*                       Check if error-exit was taken incorrectly.
*
                        IF ( .NOT. OK) THEN
                           WRITE (NOUT,99993)
                           FATAL = .TRUE.
                           GO TO 300
                        END IF
*
*                       See what data changed inside subroutines.
*
                        ISAME(1) = UPLOS .EQ. UPLO
                        ISAME(2) = TRANSS .EQ. TRANS
                        ISAME(3) = NS .EQ. N
                        ISAME(4) = KS .EQ. K
                        ISAME(5) = ALS .EQ. ALPHA
                        ISAME(6) = LSE(AS,AA,LAA)
                        ISAME(7) = LDAS .EQ. LDA
                        ISAME(8) = LSE(BS,BB,LBB)
                        ISAME(9) = LDBS .EQ. LDB
                        ISAME(10) = BETS .EQ. BETA
                        IF (NULL) THEN
                           ISAME(11) = LSE(CS,CC,LCC)
                        ELSE
                           ISAME(11) = LSERES('SY',UPLO,N,N,CS,CC,LDC)
                        END IF
                        ISAME(12) = LDCS .EQ. LDC
*
*                       If data was incorrectly changed, report and
*                       return.
*
                        SAME = .TRUE.
                        DO 80 I = 1, NARGS
                           SAME = SAME .AND. ISAME(I)
                           IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   80                   CONTINUE
                        IF ( .NOT. SAME) THEN
                           FATAL = .TRUE.
                           GO TO 300
                        END IF
*
                        IF ( .NOT. NULL) THEN
*
*                          Check the result column by column.
*
                           JJAB = 1
                           JC = 1
                           DO 140 J = 1, N
                              IF (UPPER) THEN
                                 JJ = 1
                                 LJ = J
                              ELSE
                                 JJ = J
                                 LJ = N - J + 1
                              END IF
                              IF (TRAN) THEN
                                 DO 100 I = 1, K
                                    W(I) = AB((J-1)*2*NMAX+K+I)
                                    W(K+I) = AB((J-1)*2*NMAX+I)
  100                            CONTINUE
                                 CALL SMMCH('T','N',LJ,1,2*K,ALPHA,
     +                                      AB(JJAB),2*NMAX,W,2*NMAX,
     +                                      BETA,C(JJ,J),NMAX,CT,G,
     +                                      CC(JC),LDC,EPS,ERR,FATAL,
     +                                      NOUT,.TRUE.)
                              ELSE
                                 DO 120 I = 1, K
                                    W(I) = AB((K+I-1)*NMAX+J)
                                    W(K+I) = AB((I-1)*NMAX+J)
  120                            CONTINUE
                                 CALL SMMCH('N','N',LJ,1,2*K,ALPHA,
     +                                      AB(JJ),NMAX,W,2*NMAX,BETA,
     +                                      C(JJ,J),NMAX,CT,G,CC(JC),
     +                                      LDC,EPS,ERR,FATAL,NOUT,
     +                                      .TRUE.)
                              END IF
                              IF (UPPER) THEN
                                 JC = JC + LDC
                              ELSE
                                 JC = JC + LDC + 1
                                 IF (TRAN) JJAB = JJAB + 2*NMAX
                              END IF
                              ERRMAX = MAX(ERRMAX,ERR)
*                             If got really bad answer, report and
*                             return.
                              IF (FATAL) GO TO 280
  140                      CONTINUE
                        END IF
*
  160                CONTINUE
*
  180             CONTINUE
*
  200          CONTINUE
*
  220       CONTINUE
*
  240    CONTINUE
*
  260 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 320
*
  280 CONTINUE
      IF (N.GT.1) WRITE (NOUT,99995) J
*
  300 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, TRANS, N, K, ALPHA, LDA,
     +  LDB, BETA, LDC
*
  320 CONTINUE
      RETURN
*
*
*     End of SCHK5.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
99994 FORMAT (1X,I6,': ',A6,'(',2('''',A1,''','),2(I3,','),F4.1,', A,',
     +       I3,', B,',I3,',',F4.1,', C,',I3,')    .')
99993 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE SCHKE(ISNUM,SRNAMT,NOUT)
*
*     Tests the error exits from the Level 3 Blas.
*     Requires a special version of the error-handling routine F06AAZ.
*     ALPHA, BETA, A, B and C should not need to be defined.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER          ISNUM, NOUT
      CHARACTER*13     SRNAMT
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, BETA
*     .. Local Arrays ..
      DOUBLE PRECISION A(2,1), B(2,1), C(2,1)
*     .. External Subroutines ..
      EXTERNAL         CHKXER, F06YAF, F06YCF, F06YFF, F06YJF, F06YPF,
     +                 F06YRF
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Executable Statements ..
*     OK is set to .FALSE. by the special version of F06AAZ or by CHKXER
*     if anything is wrong.
      OK = .TRUE.
*     LERR is set to .TRUE. by the special version of F06AAZ each time
*     it is called, and is then tested and re-set by CHKXER.
      LERR = .FALSE.
      GO TO (20,40,60,80,100,120) ISNUM
   20 INFOT = 1
      CALL F06YAF('/','N',0,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 1
      CALL F06YAF('/','T',0,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YAF('N','/',0,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YAF('T','/',0,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YAF('N','N',-1,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YAF('N','T',-1,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YAF('T','N',-1,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YAF('T','T',-1,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YAF('N','N',0,-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YAF('N','T',0,-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YAF('T','N',0,-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YAF('T','T',0,-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YAF('N','N',0,0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YAF('N','T',0,0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YAF('T','N',0,0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YAF('T','T',0,0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06YAF('N','N',2,0,0,ALPHA,A,1,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06YAF('N','T',2,0,0,ALPHA,A,1,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06YAF('T','N',0,0,2,ALPHA,A,1,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06YAF('T','T',0,0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YAF('N','N',0,0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YAF('T','N',0,0,2,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YAF('N','T',0,2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YAF('T','T',0,2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 13
      CALL F06YAF('N','N',2,0,0,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 13
      CALL F06YAF('N','T',2,0,0,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 13
      CALL F06YAF('T','N',2,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 13
      CALL F06YAF('T','T',2,0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 140
   40 INFOT = 1
      CALL F06YCF('/','U',0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YCF('L','/',0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YCF('L','U',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YCF('R','U',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YCF('L','L',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YCF('R','L',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YCF('L','U',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YCF('R','U',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YCF('L','L',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YCF('R','L',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YCF('L','U',2,0,ALPHA,A,1,B,2,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YCF('R','U',0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YCF('L','L',2,0,ALPHA,A,1,B,2,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YCF('R','L',0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YCF('L','U',2,0,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YCF('R','U',2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YCF('L','L',2,0,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YCF('R','L',2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YCF('L','U',2,0,ALPHA,A,2,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YCF('R','U',2,0,ALPHA,A,1,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YCF('L','L',2,0,ALPHA,A,2,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YCF('R','L',2,0,ALPHA,A,1,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 140
   60 INFOT = 1
      CALL F06YFF('/','U','N','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YFF('L','/','N','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YFF('L','U','/','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YFF('L','U','N','/',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('L','U','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('L','U','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('R','U','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('R','U','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('L','L','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('L','L','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('R','L','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YFF('R','L','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('L','U','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('L','U','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('R','U','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('R','U','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('L','L','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('L','L','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('R','L','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YFF('R','L','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('L','U','N','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('L','U','T','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('R','U','N','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('R','U','T','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('L','L','N','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('L','L','T','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('R','L','N','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YFF('R','L','T','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('L','U','N','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('L','U','T','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('R','U','N','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('R','U','T','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('L','L','N','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('L','L','T','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('R','L','N','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YFF('R','L','T','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 140
   80 INFOT = 1
      CALL F06YJF('/','U','N','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YJF('L','/','N','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YJF('L','U','/','N',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YJF('L','U','N','/',0,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('L','U','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('L','U','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('R','U','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('R','U','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('L','L','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('L','L','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('R','L','N','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06YJF('R','L','T','N',-1,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('L','U','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('L','U','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('R','U','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('R','U','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('L','L','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('L','L','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('R','L','N','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06YJF('R','L','T','N',0,-1,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('L','U','N','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('L','U','T','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('R','U','N','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('R','U','T','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('L','L','N','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('L','L','T','N',2,0,ALPHA,A,1,B,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('R','L','N','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YJF('R','L','T','N',0,2,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('L','U','N','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('L','U','T','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('R','U','N','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('R','U','T','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('L','L','N','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('L','L','T','N',2,0,ALPHA,A,2,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('R','L','N','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06YJF('R','L','T','N',2,0,ALPHA,A,1,B,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 140
  100 INFOT = 1
      CALL F06YPF('/','N',0,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YPF('U','/',0,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YPF('U','N',-1,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YPF('U','T',-1,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YPF('L','N',-1,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YPF('L','T',-1,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YPF('U','N',0,-1,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YPF('U','T',0,-1,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YPF('L','N',0,-1,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YPF('L','T',0,-1,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YPF('U','N',2,0,ALPHA,A,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YPF('U','T',0,2,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YPF('L','N',2,0,ALPHA,A,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YPF('L','T',0,2,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YPF('U','N',2,0,ALPHA,A,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YPF('U','T',2,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YPF('L','N',2,0,ALPHA,A,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06YPF('L','T',2,0,ALPHA,A,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 140
  120 INFOT = 1
      CALL F06YRF('/','N',0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06YRF('U','/',0,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YRF('U','N',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YRF('U','T',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YRF('L','N',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06YRF('L','T',-1,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YRF('U','N',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YRF('U','T',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YRF('L','N',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06YRF('L','T',0,-1,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YRF('U','N',2,0,ALPHA,A,1,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YRF('U','T',0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YRF('L','N',2,0,ALPHA,A,1,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06YRF('L','T',0,2,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YRF('U','N',2,0,ALPHA,A,2,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YRF('U','T',0,2,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YRF('L','N',2,0,ALPHA,A,2,B,1,BETA,C,2)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06YRF('L','T',0,2,ALPHA,A,2,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YRF('U','N',2,0,ALPHA,A,2,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YRF('U','T',2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YRF('L','N',2,0,ALPHA,A,2,B,2,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 12
      CALL F06YRF('L','T',2,0,ALPHA,A,1,B,1,BETA,C,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
*
  140 IF (OK) THEN
         WRITE (NOUT,99999) SRNAMT
      ELSE
         WRITE (NOUT,99998) SRNAMT
      END IF
      RETURN
*
*
*     End of SCHKE.
*
99999 FORMAT (' ',A6,' PASSED THE TESTS OF ERROR-EXITS')
99998 FORMAT (' ******* ',A6,' FAILED THE TESTS OF ERROR-EXITS *******')
      END
      SUBROUTINE SMAKE(TYPE,UPLO,DIAG,M,N,A,NMAX,AA,LDA,RESET,TRANSL)
*
*     Generates values for an M by N matrix A.
*     Stores the values in the array AA in the data structure required
*     by the routine, with unwanted elements set to rogue value.
*
*     TYPE is 'GE', 'SY' or 'TR'.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
      DOUBLE PRECISION ROGUE
      PARAMETER        (ROGUE=-1.0D10)
*     .. Scalar Arguments ..
      DOUBLE PRECISION TRANSL
      INTEGER          LDA, M, N, NMAX
      LOGICAL          RESET
      CHARACTER        DIAG, UPLO
      CHARACTER*2      TYPE
*     .. Array Arguments ..
      DOUBLE PRECISION A(NMAX,*), AA(*)
*     .. Local Scalars ..
      INTEGER          I, IBEG, IEND, J
      LOGICAL          GEN, LOWER, SYM, TRI, UNIT, UPPER
*     .. External Functions ..
      DOUBLE PRECISION SBEG
      EXTERNAL         SBEG
*     .. Executable Statements ..
      GEN = TYPE .EQ. 'GE'
      SYM = TYPE .EQ. 'SY'
      TRI = TYPE .EQ. 'TR'
      UPPER = (SYM .OR. TRI) .AND. UPLO .EQ. 'U'
      LOWER = (SYM .OR. TRI) .AND. UPLO .EQ. 'L'
      UNIT = TRI .AND. DIAG .EQ. 'U'
*
*     Generate data in array A.
*
      DO 40 J = 1, N
         DO 20 I = 1, M
            IF (GEN .OR. (UPPER .AND. I.LE.J) .OR. (LOWER .AND. I.GE.J))
     +          THEN
               A(I,J) = SBEG(RESET) + TRANSL
               IF (I.NE.J) THEN
*                 Set some elements to zero
                  IF (N.GT.3 .AND. J.EQ.N/2) A(I,J) = ZERO
                  IF (SYM) THEN
                     A(J,I) = A(I,J)
                  ELSE IF (TRI) THEN
                     A(J,I) = ZERO
                  END IF
               END IF
            END IF
   20    CONTINUE
         IF (TRI) A(J,J) = A(J,J) + ONE
         IF (UNIT) A(J,J) = ONE
   40 CONTINUE
*
*     Store elements in array AS in data structure required by routine.
*
      IF (TYPE.EQ.'GE') THEN
         DO 100 J = 1, N
            DO 60 I = 1, M
               AA(I+(J-1)*LDA) = A(I,J)
   60       CONTINUE
            DO 80 I = M + 1, LDA
               AA(I+(J-1)*LDA) = ROGUE
   80       CONTINUE
  100    CONTINUE
      ELSE IF (TYPE.EQ.'SY' .OR. TYPE.EQ.'TR') THEN
         DO 180 J = 1, N
            IF (UPPER) THEN
               IBEG = 1
               IF (UNIT) THEN
                  IEND = J - 1
               ELSE
                  IEND = J
               END IF
            ELSE
               IF (UNIT) THEN
                  IBEG = J + 1
               ELSE
                  IBEG = J
               END IF
               IEND = N
            END IF
            DO 120 I = 1, IBEG - 1
               AA(I+(J-1)*LDA) = ROGUE
  120       CONTINUE
            DO 140 I = IBEG, IEND
               AA(I+(J-1)*LDA) = A(I,J)
  140       CONTINUE
            DO 160 I = IEND + 1, LDA
               AA(I+(J-1)*LDA) = ROGUE
  160       CONTINUE
  180    CONTINUE
      END IF
      RETURN
*
*     End of SMAKE.
*
      END
      SUBROUTINE SMMCH(TRANSA,TRANSB,M,N,KK,ALPHA,A,LDA,B,LDB,BETA,C,
     +                 LDC,CT,G,CC,LDCC,EPS,ERR,FATAL,NOUT,MV)
*
*     Checks the results of the computational tests.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, BETA, EPS, ERR
      INTEGER          KK, LDA, LDB, LDC, LDCC, M, N, NOUT
      LOGICAL          FATAL, MV
      CHARACTER        TRANSA, TRANSB
*     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*), CC(LDCC,*), CT(*),
     +                 G(*)
*     .. Local Scalars ..
      DOUBLE PRECISION ERRI
      INTEGER          I, J, K
      LOGICAL          TRANA, TRANB
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, SQRT
*     .. Executable Statements ..
      TRANA = TRANSA .EQ. 'T' .OR. TRANSA .EQ. 'C'
      TRANB = TRANSB .EQ. 'T' .OR. TRANSB .EQ. 'C'
*
*     Compute expected result, one column at a time, in CT using data
*     in A, B and C.
*     Compute gauges in G.
*
      DO 240 J = 1, N
*
         DO 20 I = 1, M
            CT(I) = ZERO
            G(I) = ZERO
   20    CONTINUE
         IF ( .NOT. TRANA .AND. .NOT. TRANB) THEN
            DO 60 K = 1, KK
               DO 40 I = 1, M
                  CT(I) = CT(I) + A(I,K)*B(K,J)
                  G(I) = G(I) + ABS(A(I,K))*ABS(B(K,J))
   40          CONTINUE
   60       CONTINUE
         ELSE IF (TRANA .AND. .NOT. TRANB) THEN
            DO 100 K = 1, KK
               DO 80 I = 1, M
                  CT(I) = CT(I) + A(K,I)*B(K,J)
                  G(I) = G(I) + ABS(A(K,I))*ABS(B(K,J))
   80          CONTINUE
  100       CONTINUE
         ELSE IF ( .NOT. TRANA .AND. TRANB) THEN
            DO 140 K = 1, KK
               DO 120 I = 1, M
                  CT(I) = CT(I) + A(I,K)*B(J,K)
                  G(I) = G(I) + ABS(A(I,K))*ABS(B(J,K))
  120          CONTINUE
  140       CONTINUE
         ELSE IF (TRANA .AND. TRANB) THEN
            DO 180 K = 1, KK
               DO 160 I = 1, M
                  CT(I) = CT(I) + A(K,I)*B(J,K)
                  G(I) = G(I) + ABS(A(K,I))*ABS(B(J,K))
  160          CONTINUE
  180       CONTINUE
         END IF
         DO 200 I = 1, M
            CT(I) = ALPHA*CT(I) + BETA*C(I,J)
            G(I) = ABS(ALPHA)*G(I) + ABS(BETA)*ABS(C(I,J))
  200    CONTINUE
*
*        Compute the error ratio for this result.
*
         ERR = ZERO
         DO 220 I = 1, M
            ERRI = ABS(CT(I)-CC(I,J))/EPS
            IF (G(I).NE.ZERO) ERRI = ERRI/G(I)
            ERR = MAX(ERR,ERRI)
            IF (ERR*SQRT(EPS).GE.ONE) GO TO 260
  220    CONTINUE
*
  240 CONTINUE
*
*     If the loop completes, all results are at least half accurate.
      GO TO 300
*
*     Report fatal error.
*
  260 FATAL = .TRUE.
      WRITE (NOUT,99999)
      DO 280 I = 1, M
         IF (MV) THEN
            WRITE (NOUT,99998) I, CT(I), CC(I,J)
         ELSE
            WRITE (NOUT,99998) I, CC(I,J), CT(I)
         END IF
  280 CONTINUE
      IF (N.GT.1) WRITE (NOUT,99997) J
*
  300 CONTINUE
      RETURN
*
*
*     End of SMMCH.
*
99999 FORMAT (' ******* FATAL ERROR - COMPUTED RESULT IS LESS THAN HAL',
     +       'F ACCURATE *******',/'           EXPECTED RESULT   COMPU',
     +       'TED RESULT')
99998 FORMAT (1X,I7,2G18.6)
99997 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
      END
      LOGICAL FUNCTION LSE(RI,RJ,LR)
*
*     Tests if two arrays are identical.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER              LR
*     .. Array Arguments ..
      DOUBLE PRECISION     RI(*), RJ(*)
*     .. Local Scalars ..
      INTEGER              I
*     .. Executable Statements ..
      DO 20 I = 1, LR
         IF (RI(I).NE.RJ(I)) GO TO 40
   20 CONTINUE
      LSE = .TRUE.
      GO TO 60
   40 CONTINUE
      LSE = .FALSE.
   60 RETURN
*
*     End of LSE.
*
      END
      LOGICAL FUNCTION LSERES(TYPE,UPLO,M,N,AA,AS,LDA)
*
*     Tests if selected elements in two arrays are equal.
*
*     TYPE is 'GE' or 'SY'.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER                 LDA, M, N
      CHARACTER               UPLO
      CHARACTER*2             TYPE
*     .. Array Arguments ..
      DOUBLE PRECISION        AA(LDA,*), AS(LDA,*)
*     .. Local Scalars ..
      INTEGER                 I, IBEG, IEND, J
      LOGICAL                 UPPER
*     .. Executable Statements ..
      UPPER = UPLO .EQ. 'U'
      IF (TYPE.EQ.'GE') THEN
         DO 40 J = 1, N
            DO 20 I = M + 1, LDA
               IF (AA(I,J).NE.AS(I,J)) GO TO 140
   20       CONTINUE
   40    CONTINUE
      ELSE IF (TYPE.EQ.'SY') THEN
         DO 100 J = 1, N
            IF (UPPER) THEN
               IBEG = 1
               IEND = J
            ELSE
               IBEG = J
               IEND = N
            END IF
            DO 60 I = 1, IBEG - 1
               IF (AA(I,J).NE.AS(I,J)) GO TO 140
   60       CONTINUE
            DO 80 I = IEND + 1, LDA
               IF (AA(I,J).NE.AS(I,J)) GO TO 140
   80       CONTINUE
  100    CONTINUE
      END IF
*
  120 CONTINUE
      LSERES = .TRUE.
      GO TO 160
  140 CONTINUE
      LSERES = .FALSE.
  160 RETURN
*
*     End of LSERES.
*
      END
      DOUBLE PRECISION FUNCTION SBEG(RESET)
*
*     Generates random numbers uniformly distributed between -0.5 and
*     0.5.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      LOGICAL                        RESET
*     .. Local Scalars ..
      INTEGER                        I, IC, MI
*     .. Save statement ..
      SAVE                           I, IC, MI
*     .. Executable Statements ..
      IF (RESET) THEN
*        Initialize local variables.
         MI = 891
         I = 7
         IC = 0
         RESET = .FALSE.
      END IF
*
*     The sequence of values of I is bounded between 1 and 999.
*     If initial I = 1,2,3,6,7 or 9, the period will be 50.
*     If initial I = 4 or 8, the period will be 25.
*     If initial I = 5, the period will be 10.
*     IC is used to break up the period by skipping 1 value of I in 6.
*
      IC = IC + 1
   20 I = I*MI
      I = I - 1000*(I/1000)
      IF (IC.GE.5) THEN
         IC = 0
         GO TO 20
      END IF
      SBEG = (I-500)/1001.0D0
      RETURN
*
*     End of SBEG.
*
      END
      DOUBLE PRECISION FUNCTION SDIFF(X,Y)
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION                X, Y
*     .. Executable Statements ..
      SDIFF = X - Y
      RETURN
*
*     End of SDIFF.
*
      END
      SUBROUTINE CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
*
*     Tests whether F06AAZ has detected an error when it should.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER           INFOT, NOUT
      LOGICAL           LERR, OK
      CHARACTER*13      SRNAMT
*     .. Executable Statements ..
      IF ( .NOT. LERR) THEN
         WRITE (NOUT,99999) INFOT, SRNAMT(8:13)
         OK = .FALSE.
      END IF
      LERR = .FALSE.
      RETURN
*
*
*     End of CHKXER.
*
99999 FORMAT (' ***** ILLEGAL VALUE OF PARAMETER NUMBER ',I2,' NOT DET',
     +       'ECTED BY ',A6,' *****')
      END
      SUBROUTINE F06AAZ(SRNAME,INFO)
*
*     This is a special version of F06AAZ to be used only as part of
*     the test program for testing error exits from the Level 3 BLAS
*     routines.
*
*     F06AAZ  is an error handler for the Level 3 BLAS routines.
*
*     It is called by the Level 3 BLAS routines if an input parameter is
*     invalid.
*
*     Auxiliary routine for test program for Level 3 Blas.
*
*     -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     .. Scalar Arguments ..
      INTEGER           INFO
      CHARACTER*13      SRNAME
*     .. Scalars in Common ..
      INTEGER           INFOT, NOUT
      LOGICAL           LERR, OK
      CHARACTER*13      SRNAMT
*     .. Common blocks ..
      COMMON            /INFOC/INFOT, NOUT, OK, LERR
      COMMON            /SRNAMC/SRNAMT
*     .. Executable Statements ..
      LERR = .TRUE.
      IF (INFO.NE.INFOT) THEN
         IF (INFOT.NE.0) THEN
            WRITE (NOUT,99999) INFO, INFOT
         ELSE
            WRITE (NOUT,99997) INFO
         END IF
         OK = .FALSE.
      END IF
      IF (SRNAME.NE.SRNAMT) THEN
         WRITE (NOUT,99998) SRNAME, SRNAMT
         OK = .FALSE.
      END IF
      RETURN
*
*
*     End of F06AAZ
*
99999 FORMAT (' ******* F06AAZ WAS CALLED WITH INFO = ',I6,' INSTEAD O',
     +       'F ',I2,' *******')
99998 FORMAT (' ******* F06AAZ WAS CALLED WITH SRNAME = ',A13,' INSTEA',
     +       'D OF ',A13,' *******')
99997 FORMAT (' ******* F06AAZ WAS CALLED WITH INFO = ',I6,' *******')
      END
