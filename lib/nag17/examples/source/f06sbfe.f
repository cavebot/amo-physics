*     F06SBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*
*     Test program for the COMPLEX          Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NSUBS
      PARAMETER        (NSUBS=17)
      COMPLEX*16       ZERO, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),ONE=(1.0D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
      INTEGER          NMAX, INCMAX
      PARAMETER        (NMAX=65,INCMAX=2)
      INTEGER          NINMAX, NIDMAX, NKBMAX, NALMAX, NBEMAX
      PARAMETER        (NINMAX=7,NIDMAX=9,NKBMAX=7,NALMAX=7,NBEMAX=7)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
      CHARACTER*13     SRNAMT
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, ERR, THRESH
      INTEGER          I, ISNUM, J, N, NALF, NBET, NIDIM, NINC, NKB,
     +                 NTRA
      LOGICAL          FATAL, LTESTT, REWI, SAME, SFATAL, TRACE, TSTERR
      CHARACTER*1      TRANS
      CHARACTER*6      SNAMET
*     .. Local Arrays ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALMAX),
     +                 AS(NMAX*NMAX), BET(NBEMAX), X(NMAX),
     +                 XS(NMAX*INCMAX), XX(NMAX*INCMAX), Y(NMAX),
     +                 YS(NMAX*INCMAX), YT(NMAX), YY(NMAX*INCMAX),
     +                 Z(2*NMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDMAX), INC(NINMAX), KB(NKBMAX)
      LOGICAL          LTEST(NSUBS)
      CHARACTER*13     SNAMES(NSUBS)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      LOGICAL          LCE
      EXTERNAL         X02AJF, LCE
*     .. External Subroutines ..
      EXTERNAL         CCHK1, CCHK2, CCHK3, CCHK4, CCHK5, CCHK6, CCHKE,
     +                 CMVCH
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, MIN
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
      COMMON           /SRNAMC/SRNAMT
*     .. Data statements ..
      DATA             SNAMES/'F06SAF/ZGEMV ', 'F06SBF/ZGBMV ',
     +                 'F06SCF/ZHEMV ', 'F06SDF/ZHBMV ',
     +                 'F06SEF/ZHPMV ', 'F06SFF/ZTRMV ',
     +                 'F06SGF/ZTBMV ', 'F06SHF/ZTPMV ',
     +                 'F06SJF/ZTRSV ', 'F06SKF/ZTBSV ',
     +                 'F06SLF/ZTPSV ', 'F06SNF/ZGERC ',
     +                 'F06SMF/ZGERU ', 'F06SPF/ZHER  ',
     +                 'F06SQF/ZHPR  ', 'F06SRF/ZHER2 ',
     +                 'F06SSF/ZHPR2 '/
*     .. Executable Statements ..
      WRITE (NOUT,99979)
      READ (NIN,'()')
      NOUTC = NOUT
      NTRA = NOUT
      REWI = .FALSE.
*
*     Read flags
*
*     Read the flag that directs tracing of execution.
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
         GO TO 420
      END IF
      READ (NIN,*) (IDIM(I),I=1,NIDIM)
      DO 20 I = 1, NIDIM
         IF (IDIM(I).LT.0 .OR. IDIM(I).GT.NMAX) THEN
            WRITE (NOUT,99996) NMAX
            GO TO 420
         END IF
   20 CONTINUE
*     Values of K
      READ (NIN,*) NKB
      IF (NKB.LT.1 .OR. NKB.GT.NKBMAX) THEN
         WRITE (NOUT,99997) 'K', NKBMAX
         GO TO 420
      END IF
      READ (NIN,*) (KB(I),I=1,NKB)
      DO 40 I = 1, NKB
         IF (KB(I).LT.0) THEN
            WRITE (NOUT,99995)
            GO TO 420
         END IF
   40 CONTINUE
*     Values of INCX and INCY
      READ (NIN,*) NINC
      IF (NINC.LT.1 .OR. NINC.GT.NINMAX) THEN
         WRITE (NOUT,99997) 'INCX AND INCY', NINMAX
         GO TO 420
      END IF
      READ (NIN,*) (INC(I),I=1,NINC)
      DO 60 I = 1, NINC
         IF (INC(I).EQ.0 .OR. ABS(INC(I)).GT.INCMAX) THEN
            WRITE (NOUT,99994) INCMAX
            GO TO 420
         END IF
   60 CONTINUE
*     Values of ALPHA
      READ (NIN,*) NALF
      IF (NALF.LT.1 .OR. NALF.GT.NALMAX) THEN
         WRITE (NOUT,99997) 'ALPHA', NALMAX
         GO TO 420
      END IF
      READ (NIN,*) (ALF(I),I=1,NALF)
*     Values of BETA
      READ (NIN,*) NBET
      IF (NBET.LT.1 .OR. NBET.GT.NBEMAX) THEN
         WRITE (NOUT,99997) 'BETA', NBEMAX
         GO TO 420
      END IF
      READ (NIN,*) (BET(I),I=1,NBET)
*
*     Report values of parameters.
*
      WRITE (NOUT,99993)
      WRITE (NOUT,99992) (IDIM(I),I=1,NIDIM)
      WRITE (NOUT,99991) (KB(I),I=1,NKB)
      WRITE (NOUT,99990) (INC(I),I=1,NINC)
      WRITE (NOUT,99989) (ALF(I),I=1,NALF)
      WRITE (NOUT,99988) (BET(I),I=1,NBET)
      IF ( .NOT. TSTERR) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99980)
      END IF
      WRITE (NOUT,*)
      WRITE (NOUT,99999) THRESH
      WRITE (NOUT,*)
*
*     Read names of subroutines and flags which indicate
*     whether they are to be tested.
*
      DO 80 I = 1, NSUBS
         LTEST(I) = .FALSE.
   80 CONTINUE
  100 READ (NIN,99984,END=160) SNAMET, LTESTT
      DO 120 I = 1, NSUBS
         IF (SNAMET.EQ.SNAMES(I)(1:6)) GO TO 140
  120 CONTINUE
      WRITE (NOUT,99986) SNAMET
      STOP
  140 LTEST(I) = LTESTT
      GO TO 100
*
  160 CONTINUE
*
*     Compute EPS (the machine precision).
*
      EPS = X02AJF()
      WRITE (NOUT,99998) EPS
*
*     Check the reliability of CMVCH using exact data.
*
      N = MIN(32,NMAX)
      DO 200 J = 1, N
         DO 180 I = 1, N
            A(I,J) = MAX(I-J+1,0)
  180    CONTINUE
         X(J) = J
         Y(J) = ZERO
  200 CONTINUE
      DO 220 J = 1, N
         YY(J) = J*((J+1)*J)/2 - ((J+1)*J*(J-1))/3
  220 CONTINUE
*     YY holds the exact result. On exit from CMVCH YT holds
*     the result computed by CMVCH.
      TRANS = 'N'
      CALL CMVCH(TRANS,N,N,ONE,A,NMAX,X,1,ZERO,Y,1,YT,G,YY,EPS,ERR,
     +           FATAL,NOUT,.TRUE.)
      SAME = LCE(YY,YT,N)
      IF ( .NOT. SAME .OR. ERR.NE.RZERO) THEN
         WRITE (NOUT,99985) TRANS, SAME, ERR
         STOP
      END IF
      TRANS = 'T'
      CALL CMVCH(TRANS,N,N,ONE,A,NMAX,X,-1,ZERO,Y,-1,YT,G,YY,EPS,ERR,
     +           FATAL,NOUT,.TRUE.)
      SAME = LCE(YY,YT,N)
      IF ( .NOT. SAME .OR. ERR.NE.RZERO) THEN
         WRITE (NOUT,99985) TRANS, SAME, ERR
         STOP
      END IF
*
*     Test each subroutine in turn.
*
      DO 380 ISNUM = 1, NSUBS
         WRITE (NOUT,*)
         IF ( .NOT. LTEST(ISNUM)) THEN
*           Subprogram is not to be tested.
            WRITE (NOUT,99983) SNAMES(ISNUM) (1:6)
         ELSE
            SRNAMT = SNAMES(ISNUM)
*           Test error exits.
            IF (TSTERR) THEN
               CALL CCHKE(ISNUM,SNAMES(ISNUM),NOUT)
               WRITE (NOUT,*)
            END IF
*           Test computations.
            INFOT = 0
            OK = .TRUE.
            FATAL = .FALSE.
            GO TO (240,240,260,260,260,280,280,280,
     +             280,280,280,300,300,320,320,340,
     +             340) ISNUM
*           Test F06SAF, 01, and F06SBF, 02.
  240       CALL CCHK1(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NKB,KB,NALF,ALF,NBET,BET,NINC,
     +                 INC,NMAX,INCMAX,A,AA,AS,X,XX,XS,Y,YY,YS,YT,G)
            GO TO 360
*           Test F06SCF, 03, F06SDF, 04, and F06SEF, 05.
  260       CALL CCHK2(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NKB,KB,NALF,ALF,NBET,BET,NINC,
     +                 INC,NMAX,INCMAX,A,AA,AS,X,XX,XS,Y,YY,YS,YT,G)
            GO TO 360
*           Test F06SFF, 06, F06SGF, 07, F06SHF, 08,
*           F06SJF, 09, F06SKF, 10, and F06SLF, 11.
  280       CALL CCHK3(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NKB,KB,NINC,INC,NMAX,INCMAX,A,
     +                 AA,AS,Y,YY,YS,YT,G,Z)
            GO TO 360
*           Test F06SNF, 12, F06SMF, 13.
  300       CALL CCHK4(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,
     +                 AA,AS,X,XX,XS,Y,YY,YS,YT,G,Z)
            GO TO 360
*           Test F06SPF, 14, and F06SQF, 15.
  320       CALL CCHK5(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,
     +                 AA,AS,X,XX,XS,Y,YY,YS,YT,G,Z)
            GO TO 360
*           Test F06SRF, 16, and F06SSF, 17.
  340       CALL CCHK6(SNAMES(ISNUM),EPS,THRESH,NOUT,NTRA,TRACE,REWI,
     +                 FATAL,NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,
     +                 AA,AS,X,XX,XS,Y,YY,YS,YT,G,Z)
*
  360       IF (FATAL .AND. SFATAL) GO TO 400
         END IF
  380 CONTINUE
      WRITE (NOUT,99982)
      GO TO 440
*
  400 CONTINUE
      WRITE (NOUT,99981)
      GO TO 440
*
  420 CONTINUE
      WRITE (NOUT,99987)
*
  440 CONTINUE
      STOP
*
*
*     End of CBLAT2.
*
99999 FORMAT (' ROUTINES PASS COMPUTATIONAL TESTS IF TEST RATIO IS LES',
     +       'S THAN',F8.2)
99998 FORMAT (' RELATIVE MACHINE PRECISION IS TAKEN TO BE',1P,D9.1)
99997 FORMAT (' NUMBER OF VALUES OF ',A,' IS LESS THAN 1 OR GREATER TH',
     +       'AN ',I2)
99996 FORMAT (' VALUE OF N IS LESS THAN 0 OR GREATER THAN ',I2)
99995 FORMAT (' VALUE OF K IS LESS THAN 0')
99994 FORMAT (' ABSOLUTE VALUE OF INCX OR INCY IS 0 OR GREATER THAN ',
     +       I2)
99993 FORMAT (' TESTS OF THE COMPLEX          LEVEL 2 BLAS',//' THE FO',
     +       'LLOWING PARAMETER VALUES WILL BE USED:')
99992 FORMAT ('   FOR N              ',9I6)
99991 FORMAT ('   FOR K              ',7I6)
99990 FORMAT ('   FOR INCX AND INCY  ',7I6)
99989 FORMAT ('   FOR ALPHA          ',7('(',F4.1,',',F4.1,')  ',:))
99988 FORMAT ('   FOR BETA           ',7('(',F4.1,',',F4.1,')  ',:))
99987 FORMAT (' AMEND DATA FILE OR INCREASE ARRAY SIZES IN PROGRAM',
     +       /' ******* TESTS ABANDONED *******')
99986 FORMAT (' SUBPROGRAM NAME ',A6,' NOT RECOGNIZED',/' ******* TEST',
     +       'S ABANDONED *******')
99985 FORMAT (' ERROR IN CMVCH -  IN-LINE DOT PRODUCTS ARE BEING EVALU',
     +       'ATED WRONGLY.',/' CMVCH WAS CALLED WITH TRANS = ',A1,' A',
     +       'ND RETURNED SAME = ',L1,' AND ERR = ',F12.3,'.',/' THIS ',
     +       'MAY BE DUE TO FAULTS IN THE ARITHMETIC OR THE COMPILER.',
     +       /' ******* TESTS ABANDONED *******')
99984 FORMAT (A6,L2)
99983 FORMAT (1X,A6,' WAS NOT TESTED')
99982 FORMAT (/' END OF TESTS')
99981 FORMAT (/' ******* FATAL ERROR - TESTS ABANDONED *******')
99980 FORMAT (' ERROR-EXITS WILL NOT BE TESTED')
99979 FORMAT (' F06SBF Example Program Results',/1X)
      END
      SUBROUTINE CCHK1(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NKB,KB,NALF,ALF,NBET,BET,NINC,INC,
     +                 NMAX,INCMAX,A,AA,AS,X,XX,XS,Y,YY,YS,YT,G)
*
*     Tests F06SAF and F06SBF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NALF, NBET, NIDIM, NINC, NKB, NMAX, NOUT,
     +                 NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), BET(NBET), X(NMAX),
     +                 XS(NMAX*INCMAX), XX(NMAX*INCMAX), Y(NMAX),
     +                 YS(NMAX*INCMAX), YT(NMAX), YY(NMAX*INCMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC), KB(NKB)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, ALS, BETA, BLS, TRANSL
      DOUBLE PRECISION ERR, ERRMAX
      INTEGER          I, IA, IB, IC, IKU, IM, IN, INCX, INCXS, INCY,
     +                 INCYS, IX, IY, KL, KLS, KU, KUS, LAA, LDA, LDAS,
     +                 LX, LY, M, ML, MS, N, NARGS, NC, ND, NK, NL, NS
      LOGICAL          BANDED, FULL, NULL, RESET, SAME, TRAN
      CHARACTER*1      TRANS, TRANSS
      CHARACTER*3      ICH
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SAF, F06SBF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX, MIN
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICH/'NTC'/
*     .. Executable Statements ..
      FULL = SNAME(10:10) .EQ. 'E'
      BANDED = SNAME(10:10) .EQ. 'B'
*     Define the number of arguments.
      IF (FULL) THEN
         NARGS = 11
      ELSE IF (BANDED) THEN
         NARGS = 13
      END IF
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*
      DO 240 IN = 1, NIDIM
         N = IDIM(IN)
         ND = N/2 + 1
*
         DO 220 IM = 1, 2
            IF (IM.EQ.1) M = MAX(N-ND,0)
            IF (IM.EQ.2) M = MIN(N+ND,NMAX)
*
            IF (BANDED) THEN
               NK = NKB
            ELSE
               NK = 1
            END IF
            DO 200 IKU = 1, NK
               IF (BANDED) THEN
                  KU = KB(IKU)
                  KL = MAX(KU-1,0)
               ELSE
                  KU = N - 1
                  KL = M - 1
               END IF
*              Set LDA to 1 more than minimum value if room.
               IF (BANDED) THEN
                  LDA = KL + KU + 1
               ELSE
                  LDA = M
               END IF
               IF (LDA.LT.NMAX) LDA = LDA + 1
*              Skip tests if not enough room.
               IF (LDA.GT.NMAX) GO TO 200
               LAA = LDA*N
               NULL = N .LE. 0 .OR. M .LE. 0
*
*              Generate the matrix A.
*
               TRANSL = ZERO
               CALL CMAKE(SNAME(9:10),' ',' ',M,N,A,NMAX,AA,LDA,KL,KU,
     +                    RESET,TRANSL)
*
               DO 180 IC = 1, 3
                  TRANS = ICH(IC:IC)
                  TRAN = TRANS .EQ. 'T' .OR. TRANS .EQ. 'C'
*
                  IF (TRAN) THEN
                     ML = N
                     NL = M
                  ELSE
                     ML = M
                     NL = N
                  END IF
*
                  DO 160 IX = 1, NINC
                     INCX = INC(IX)
                     LX = ABS(INCX)*NL
*
*                    Generate the vector X.
*
                     TRANSL = HALF
                     CALL CMAKE('GE',' ',' ',1,NL,X,1,XX,ABS(INCX),0,
     +                          NL-1,RESET,TRANSL)
                     IF (NL.GT.1) THEN
                        X(NL/2) = ZERO
                        XX(1+ABS(INCX)*(NL/2-1)) = ZERO
                     END IF
*
                     DO 140 IY = 1, NINC
                        INCY = INC(IY)
                        LY = ABS(INCY)*ML
*
                        DO 120 IA = 1, NALF
                           ALPHA = ALF(IA)
*
                           DO 100 IB = 1, NBET
                              BETA = BET(IB)
*
*                             Generate the vector Y.
*
                              TRANSL = ZERO
                              CALL CMAKE('GE',' ',' ',1,ML,Y,1,YY,
     +                                   ABS(INCY),0,ML-1,RESET,TRANSL)
*
                              NC = NC + 1
*
*                             Save every datum before calling the
*                             subroutine.
*
                              TRANSS = TRANS
                              MS = M
                              NS = N
                              KLS = KL
                              KUS = KU
                              ALS = ALPHA
                              DO 20 I = 1, LAA
                                 AS(I) = AA(I)
   20                         CONTINUE
                              LDAS = LDA
                              DO 40 I = 1, LX
                                 XS(I) = XX(I)
   40                         CONTINUE
                              INCXS = INCX
                              BLS = BETA
                              DO 60 I = 1, LY
                                 YS(I) = YY(I)
   60                         CONTINUE
                              INCYS = INCY
*
*                             Call the subroutine.
*
                              IF (FULL) THEN
                                 IF (TRACE) WRITE (NTRA,99994) NC,
     +                               SNAME(1:6), TRANS, M, N, ALPHA,
     +                               LDA, INCX, BETA, INCY
                                 CALL F06SAF(TRANS,M,N,ALPHA,AA,LDA,XX,
     +                                       INCX,BETA,YY,INCY)
                              ELSE IF (BANDED) THEN
                                 IF (TRACE) WRITE (NTRA,99995) NC,
     +                               SNAME(1:6), TRANS, M, N, KL, KU,
     +                               ALPHA, LDA, INCX, BETA, INCY
                                 CALL F06SBF(TRANS,M,N,KL,KU,ALPHA,AA,
     +                                       LDA,XX,INCX,BETA,YY,INCY)
                              END IF
*
*                             Check if error-exit was taken incorrectly.
*
                              IF ( .NOT. OK) THEN
                                 WRITE (NOUT,99993)
                                 FATAL = .TRUE.
                                 GO TO 260
                              END IF
*
*                             See what data changed inside subroutines.
*
                              ISAME(1) = TRANS .EQ. TRANSS
                              ISAME(2) = MS .EQ. M
                              ISAME(3) = NS .EQ. N
                              IF (FULL) THEN
                                 ISAME(4) = ALS .EQ. ALPHA
                                 ISAME(5) = LCE(AS,AA,LAA)
                                 ISAME(6) = LDAS .EQ. LDA
                                 ISAME(7) = LCE(XS,XX,LX)
                                 ISAME(8) = INCXS .EQ. INCX
                                 ISAME(9) = BLS .EQ. BETA
                                 IF (NULL) THEN
                                    ISAME(10) = LCE(YS,YY,LY)
                                 ELSE
                                    ISAME(10) = LCERES('GE',' ',1,ML,YS,
     +                                          YY,ABS(INCY))
                                 END IF
                                 ISAME(11) = INCYS .EQ. INCY
                              ELSE IF (BANDED) THEN
                                 ISAME(4) = KLS .EQ. KL
                                 ISAME(5) = KUS .EQ. KU
                                 ISAME(6) = ALS .EQ. ALPHA
                                 ISAME(7) = LCE(AS,AA,LAA)
                                 ISAME(8) = LDAS .EQ. LDA
                                 ISAME(9) = LCE(XS,XX,LX)
                                 ISAME(10) = INCXS .EQ. INCX
                                 ISAME(11) = BLS .EQ. BETA
                                 IF (NULL) THEN
                                    ISAME(12) = LCE(YS,YY,LY)
                                 ELSE
                                    ISAME(12) = LCERES('GE',' ',1,ML,YS,
     +                                          YY,ABS(INCY))
                                 END IF
                                 ISAME(13) = INCYS .EQ. INCY
                              END IF
*
*                             If data was incorrectly changed, report
*                             and return.
*
                              SAME = .TRUE.
                              DO 80 I = 1, NARGS
                                 SAME = SAME .AND. ISAME(I)
                                 IF ( .NOT. ISAME(I))
     +                               WRITE (NOUT,99998) I
   80                         CONTINUE
                              IF ( .NOT. SAME) THEN
                                 FATAL = .TRUE.
                                 GO TO 260
                              END IF
*
                              IF ( .NOT. NULL) THEN
*
*                                Check the result.
*
                                 CALL CMVCH(TRANS,M,N,ALPHA,A,NMAX,X,
     +                                      INCX,BETA,Y,INCY,YT,G,YY,
     +                                      EPS,ERR,FATAL,NOUT,.TRUE.)
                                 ERRMAX = MAX(ERRMAX,ERR)
*                                If got really bad answer, report and
*                                return.
                                 IF (FATAL) GO TO 260
                              ELSE
*                                Avoid repeating tests with M.le.0 or
*                                N.le.0.
                                 GO TO 220
                              END IF
*
  100                      CONTINUE
*
  120                   CONTINUE
*
  140                CONTINUE
*
  160             CONTINUE
*
  180          CONTINUE
*
  200       CONTINUE
*
  220    CONTINUE
*
  240 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 280
*
  260 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      IF (FULL) THEN
         WRITE (NOUT,99994) NC, SNAME(1:6), TRANS, M, N, ALPHA, LDA,
     +     INCX, BETA, INCY
      ELSE IF (BANDED) THEN
         WRITE (NOUT,99995) NC, SNAME(1:6), TRANS, M, N, KL, KU, ALPHA,
     +     LDA, INCX, BETA, INCY
      END IF
*
  280 CONTINUE
      RETURN
*
*
*     End of CCHK1.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(''',A1,''',',4(I3,','),'(',F4.1,',',F4.1,
     +       '), A,',I3,', X,',I2,',(',F4.1,',',F4.1,'), Y,',I2,') .')
99994 FORMAT (1X,I6,': ',A6,'(''',A1,''',',2(I3,','),'(',F4.1,',',F4.1,
     +       '), A,',I3,', X,',I2,',(',F4.1,',',F4.1,'), Y,',I2,')    ',
     +       '     .')
99993 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHK2(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NKB,KB,NALF,ALF,NBET,BET,NINC,INC,
     +                 NMAX,INCMAX,A,AA,AS,X,XX,XS,Y,YY,YS,YT,G)
*
*     Tests F06SCF, F06SDF and F06SEF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NALF, NBET, NIDIM, NINC, NKB, NMAX, NOUT,
     +                 NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), BET(NBET), X(NMAX),
     +                 XS(NMAX*INCMAX), XX(NMAX*INCMAX), Y(NMAX),
     +                 YS(NMAX*INCMAX), YT(NMAX), YY(NMAX*INCMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC), KB(NKB)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, ALS, BETA, BLS, TRANSL
      DOUBLE PRECISION ERR, ERRMAX
      INTEGER          I, IA, IB, IC, IK, IN, INCX, INCXS, INCY, INCYS,
     +                 IX, IY, K, KS, LAA, LDA, LDAS, LX, LY, N, NARGS,
     +                 NC, NK, NS
      LOGICAL          BANDED, FULL, NULL, PACKED, RESET, SAME
      CHARACTER*1      UPLO, UPLOS
      CHARACTER*2      ICH
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SCF, F06SDF, F06SEF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICH/'UL'/
*     .. Executable Statements ..
      FULL = SNAME(10:10) .EQ. 'E'
      BANDED = SNAME(10:10) .EQ. 'B'
      PACKED = SNAME(10:10) .EQ. 'P'
*     Define the number of arguments.
      IF (FULL) THEN
         NARGS = 10
      ELSE IF (BANDED) THEN
         NARGS = 11
      ELSE IF (PACKED) THEN
         NARGS = 9
      END IF
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*
      DO 220 IN = 1, NIDIM
         N = IDIM(IN)
*
         IF (BANDED) THEN
            NK = NKB
         ELSE
            NK = 1
         END IF
         DO 200 IK = 1, NK
            IF (BANDED) THEN
               K = KB(IK)
            ELSE
               K = N - 1
            END IF
*           Set LDA to 1 more than minimum value if room.
            IF (BANDED) THEN
               LDA = K + 1
            ELSE
               LDA = N
            END IF
            IF (LDA.LT.NMAX) LDA = LDA + 1
*           Skip tests if not enough room.
            IF (LDA.GT.NMAX) GO TO 200
            IF (PACKED) THEN
               LAA = (N*(N+1))/2
            ELSE
               LAA = LDA*N
            END IF
            NULL = N .LE. 0
*
            DO 180 IC = 1, 2
               UPLO = ICH(IC:IC)
*
*              Generate the matrix A.
*
               TRANSL = ZERO
               CALL CMAKE(SNAME(9:10),UPLO,' ',N,N,A,NMAX,AA,LDA,K,K,
     +                    RESET,TRANSL)
*
               DO 160 IX = 1, NINC
                  INCX = INC(IX)
                  LX = ABS(INCX)*N
*
*                 Generate the vector X.
*
                  TRANSL = HALF
                  CALL CMAKE('GE',' ',' ',1,N,X,1,XX,ABS(INCX),0,N-1,
     +                       RESET,TRANSL)
                  IF (N.GT.1) THEN
                     X(N/2) = ZERO
                     XX(1+ABS(INCX)*(N/2-1)) = ZERO
                  END IF
*
                  DO 140 IY = 1, NINC
                     INCY = INC(IY)
                     LY = ABS(INCY)*N
*
                     DO 120 IA = 1, NALF
                        ALPHA = ALF(IA)
*
                        DO 100 IB = 1, NBET
                           BETA = BET(IB)
*
*                          Generate the vector Y.
*
                           TRANSL = ZERO
                           CALL CMAKE('GE',' ',' ',1,N,Y,1,YY,ABS(INCY),
     +                                0,N-1,RESET,TRANSL)
*
                           NC = NC + 1
*
*                          Save every datum before calling the
*                          subroutine.
*
                           UPLOS = UPLO
                           NS = N
                           KS = K
                           ALS = ALPHA
                           DO 20 I = 1, LAA
                              AS(I) = AA(I)
   20                      CONTINUE
                           LDAS = LDA
                           DO 40 I = 1, LX
                              XS(I) = XX(I)
   40                      CONTINUE
                           INCXS = INCX
                           BLS = BETA
                           DO 60 I = 1, LY
                              YS(I) = YY(I)
   60                      CONTINUE
                           INCYS = INCY
*
*                          Call the subroutine.
*
                           IF (FULL) THEN
                              IF (TRACE) WRITE (NTRA,99993) NC,
     +                            SNAME(1:6), UPLO, N, ALPHA, LDA, INCX,
     +                            BETA, INCY
                              CALL F06SCF(UPLO,N,ALPHA,AA,LDA,XX,INCX,
     +                                    BETA,YY,INCY)
                           ELSE IF (BANDED) THEN
                              IF (TRACE) WRITE (NTRA,99994) NC,
     +                            SNAME(1:6), UPLO, N, K, ALPHA, LDA,
     +                            INCX, BETA, INCY
                              CALL F06SDF(UPLO,N,K,ALPHA,AA,LDA,XX,INCX,
     +                                    BETA,YY,INCY)
                           ELSE IF (PACKED) THEN
                              IF (TRACE) WRITE (NTRA,99995) NC,
     +                            SNAME(1:6), UPLO, N, ALPHA, INCX,
     +                            BETA, INCY
                              CALL F06SEF(UPLO,N,ALPHA,AA,XX,INCX,BETA,
     +                                    YY,INCY)
                           END IF
*
*                          Check if error-exit was taken incorrectly.
*
                           IF ( .NOT. OK) THEN
                              WRITE (NOUT,99992)
                              FATAL = .TRUE.
                              GO TO 240
                           END IF
*
*                          See what data changed inside subroutines.
*
                           ISAME(1) = UPLO .EQ. UPLOS
                           ISAME(2) = NS .EQ. N
                           IF (FULL) THEN
                              ISAME(3) = ALS .EQ. ALPHA
                              ISAME(4) = LCE(AS,AA,LAA)
                              ISAME(5) = LDAS .EQ. LDA
                              ISAME(6) = LCE(XS,XX,LX)
                              ISAME(7) = INCXS .EQ. INCX
                              ISAME(8) = BLS .EQ. BETA
                              IF (NULL) THEN
                                 ISAME(9) = LCE(YS,YY,LY)
                              ELSE
                                 ISAME(9) = LCERES('GE',' ',1,N,YS,YY,
     +                                      ABS(INCY))
                              END IF
                              ISAME(10) = INCYS .EQ. INCY
                           ELSE IF (BANDED) THEN
                              ISAME(3) = KS .EQ. K
                              ISAME(4) = ALS .EQ. ALPHA
                              ISAME(5) = LCE(AS,AA,LAA)
                              ISAME(6) = LDAS .EQ. LDA
                              ISAME(7) = LCE(XS,XX,LX)
                              ISAME(8) = INCXS .EQ. INCX
                              ISAME(9) = BLS .EQ. BETA
                              IF (NULL) THEN
                                 ISAME(10) = LCE(YS,YY,LY)
                              ELSE
                                 ISAME(10) = LCERES('GE',' ',1,N,YS,YY,
     +                                       ABS(INCY))
                              END IF
                              ISAME(11) = INCYS .EQ. INCY
                           ELSE IF (PACKED) THEN
                              ISAME(3) = ALS .EQ. ALPHA
                              ISAME(4) = LCE(AS,AA,LAA)
                              ISAME(5) = LCE(XS,XX,LX)
                              ISAME(6) = INCXS .EQ. INCX
                              ISAME(7) = BLS .EQ. BETA
                              IF (NULL) THEN
                                 ISAME(8) = LCE(YS,YY,LY)
                              ELSE
                                 ISAME(8) = LCERES('GE',' ',1,N,YS,YY,
     +                                      ABS(INCY))
                              END IF
                              ISAME(9) = INCYS .EQ. INCY
                           END IF
*
*                          If data was incorrectly changed, report and
*                          return.
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
                              CALL CMVCH('N',N,N,ALPHA,A,NMAX,X,INCX,
     +                                   BETA,Y,INCY,YT,G,YY,EPS,ERR,
     +                                   FATAL,NOUT,.TRUE.)
                              ERRMAX = MAX(ERRMAX,ERR)
*                             If got really bad answer, report and
*                             return.
                              IF (FATAL) GO TO 240
                           ELSE
*                             Avoid repeating tests with N.le.0
                              GO TO 220
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
      IF (FULL) THEN
         WRITE (NOUT,99993) NC, SNAME(1:6), UPLO, N, ALPHA, LDA, INCX,
     +     BETA, INCY
      ELSE IF (BANDED) THEN
         WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, N, K, ALPHA, LDA,
     +     INCX, BETA, INCY
      ELSE IF (PACKED) THEN
         WRITE (NOUT,99995) NC, SNAME(1:6), UPLO, N, ALPHA, INCX, BETA,
     +     INCY
      END IF
*
  260 CONTINUE
      RETURN
*
*
*     End of CCHK2.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',(',F4.1,',',F4.1,'), A',
     +       'P, X,',I2,',(',F4.1,',',F4.1,'), Y,',I2,')              ',
     +       '  .')
99994 FORMAT (1X,I6,': ',A6,'(''',A1,''',',2(I3,','),'(',F4.1,',',F4.1,
     +       '), A,',I3,', X,',I2,',(',F4.1,',',F4.1,'), Y,',I2,')    ',
     +       '     .')
99993 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',(',F4.1,',',F4.1,
     +       '), A,',I3,', X,',I2,',(',F4.1,',',F4.1,'), Y,',I2,')    ',
     +       '         .')
99992 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHK3(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NKB,KB,NINC,INC,NMAX,INCMAX,A,AA,AS,X,
     +                 XX,XS,XT,G,Z)
*
*     Tests F06SFF, F06SGF, F06SHF, F06SJF, F06SKF and F06SLF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0),
     +                 ONE=(1.0D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NIDIM, NINC, NKB, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), AS(NMAX*NMAX),
     +                 X(NMAX), XS(NMAX*INCMAX), XT(NMAX),
     +                 XX(NMAX*INCMAX), Z(NMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC), KB(NKB)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       TRANSL
      DOUBLE PRECISION ERR, ERRMAX
      INTEGER          I, ICD, ICT, ICU, IK, IN, INCX, INCXS, IX, K, KS,
     +                 LAA, LDA, LDAS, LX, N, NARGS, NC, NK, NS
      LOGICAL          BANDED, FULL, NULL, PACKED, RESET, SAME
      CHARACTER*1      DIAG, DIAGS, TRANS, TRANSS, UPLO, UPLOS
      CHARACTER*2      ICHD, ICHU
      CHARACTER*3      ICHT
*     .. Local Arrays ..
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SFF, F06SGF, F06SHF, F06SJF,
     +                 F06SKF, F06SLF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICHU/'UL'/, ICHT/'NTC'/, ICHD/'UN'/
*     .. Executable Statements ..
      FULL = SNAME(10:10) .EQ. 'R'
      BANDED = SNAME(10:10) .EQ. 'B'
      PACKED = SNAME(10:10) .EQ. 'P'
*     Define the number of arguments.
      IF (FULL) THEN
         NARGS = 8
      ELSE IF (BANDED) THEN
         NARGS = 9
      ELSE IF (PACKED) THEN
         NARGS = 7
      END IF
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*     Set up zero vector for CMVCH.
      DO 20 I = 1, NMAX
         Z(I) = ZERO
   20 CONTINUE
*
      DO 220 IN = 1, NIDIM
         N = IDIM(IN)
*
         IF (BANDED) THEN
            NK = NKB
         ELSE
            NK = 1
         END IF
         DO 200 IK = 1, NK
            IF (BANDED) THEN
               K = KB(IK)
            ELSE
               K = N - 1
            END IF
*           Set LDA to 1 more than minimum value if room.
            IF (BANDED) THEN
               LDA = K + 1
            ELSE
               LDA = N
            END IF
            IF (LDA.LT.NMAX) LDA = LDA + 1
*           Skip tests if not enough room.
            IF (LDA.GT.NMAX) GO TO 200
            IF (PACKED) THEN
               LAA = (N*(N+1))/2
            ELSE
               LAA = LDA*N
            END IF
            NULL = N .LE. 0
*
            DO 180 ICU = 1, 2
               UPLO = ICHU(ICU:ICU)
*
               DO 160 ICT = 1, 3
                  TRANS = ICHT(ICT:ICT)
*
                  DO 140 ICD = 1, 2
                     DIAG = ICHD(ICD:ICD)
*
*                    Generate the matrix A.
*
                     TRANSL = ZERO
                     CALL CMAKE(SNAME(9:10),UPLO,DIAG,N,N,A,NMAX,AA,LDA,
     +                          K,K,RESET,TRANSL)
*
                     DO 120 IX = 1, NINC
                        INCX = INC(IX)
                        LX = ABS(INCX)*N
*
*                       Generate the vector X.
*
                        TRANSL = HALF
                        CALL CMAKE('GE',' ',' ',1,N,X,1,XX,ABS(INCX),0,
     +                             N-1,RESET,TRANSL)
                        IF (N.GT.1) THEN
                           X(N/2) = ZERO
                           XX(1+ABS(INCX)*(N/2-1)) = ZERO
                        END IF
*
                        NC = NC + 1
*
*                       Save every datum before calling the subroutine.
*
                        UPLOS = UPLO
                        TRANSS = TRANS
                        DIAGS = DIAG
                        NS = N
                        KS = K
                        DO 40 I = 1, LAA
                           AS(I) = AA(I)
   40                   CONTINUE
                        LDAS = LDA
                        DO 60 I = 1, LX
                           XS(I) = XX(I)
   60                   CONTINUE
                        INCXS = INCX
*
*                       Call the subroutine.
*
                        IF (SNAME(11:12).EQ.'MV') THEN
                           IF (FULL) THEN
                              IF (TRACE) WRITE (NTRA,99993) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, LDA,
     +                            INCX
                              CALL F06SFF(UPLO,TRANS,DIAG,N,AA,LDA,XX,
     +                                    INCX)
                           ELSE IF (BANDED) THEN
                              IF (TRACE) WRITE (NTRA,99994) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, K,
     +                            LDA, INCX
                              CALL F06SGF(UPLO,TRANS,DIAG,N,K,AA,LDA,XX,
     +                                    INCX)
                           ELSE IF (PACKED) THEN
                              IF (TRACE) WRITE (NTRA,99995) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, INCX
                              CALL F06SHF(UPLO,TRANS,DIAG,N,AA,XX,INCX)
                           END IF
                        ELSE IF (SNAME(11:12).EQ.'SV') THEN
                           IF (FULL) THEN
                              IF (TRACE) WRITE (NTRA,99993) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, LDA,
     +                            INCX
                              CALL F06SJF(UPLO,TRANS,DIAG,N,AA,LDA,XX,
     +                                    INCX)
                           ELSE IF (BANDED) THEN
                              IF (TRACE) WRITE (NTRA,99994) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, K,
     +                            LDA, INCX
                              CALL F06SKF(UPLO,TRANS,DIAG,N,K,AA,LDA,XX,
     +                                    INCX)
                           ELSE IF (PACKED) THEN
                              IF (TRACE) WRITE (NTRA,99995) NC,
     +                            SNAME(1:6), UPLO, TRANS, DIAG, N, INCX
                              CALL F06SLF(UPLO,TRANS,DIAG,N,AA,XX,INCX)
                           END IF
                        END IF
*
*                       Check if error-exit was taken incorrectly.
*
                        IF ( .NOT. OK) THEN
                           WRITE (NOUT,99992)
                           FATAL = .TRUE.
                           GO TO 240
                        END IF
*
*                       See what data changed inside subroutines.
*
                        ISAME(1) = UPLO .EQ. UPLOS
                        ISAME(2) = TRANS .EQ. TRANSS
                        ISAME(3) = DIAG .EQ. DIAGS
                        ISAME(4) = NS .EQ. N
                        IF (FULL) THEN
                           ISAME(5) = LCE(AS,AA,LAA)
                           ISAME(6) = LDAS .EQ. LDA
                           IF (NULL) THEN
                              ISAME(7) = LCE(XS,XX,LX)
                           ELSE
                              ISAME(7) = LCERES('GE',' ',1,N,XS,XX,
     +                                   ABS(INCX))
                           END IF
                           ISAME(8) = INCXS .EQ. INCX
                        ELSE IF (BANDED) THEN
                           ISAME(5) = KS .EQ. K
                           ISAME(6) = LCE(AS,AA,LAA)
                           ISAME(7) = LDAS .EQ. LDA
                           IF (NULL) THEN
                              ISAME(8) = LCE(XS,XX,LX)
                           ELSE
                              ISAME(8) = LCERES('GE',' ',1,N,XS,XX,
     +                                   ABS(INCX))
                           END IF
                           ISAME(9) = INCXS .EQ. INCX
                        ELSE IF (PACKED) THEN
                           ISAME(5) = LCE(AS,AA,LAA)
                           IF (NULL) THEN
                              ISAME(6) = LCE(XS,XX,LX)
                           ELSE
                              ISAME(6) = LCERES('GE',' ',1,N,XS,XX,
     +                                   ABS(INCX))
                           END IF
                           ISAME(7) = INCXS .EQ. INCX
                        END IF
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
                           GO TO 240
                        END IF
*
                        IF ( .NOT. NULL) THEN
                           IF (SNAME(11:12).EQ.'MV') THEN
*
*                             Check the result.
*
                              CALL CMVCH(TRANS,N,N,ONE,A,NMAX,X,INCX,
     +                                   ZERO,Z,INCX,XT,G,XX,EPS,ERR,
     +                                   FATAL,NOUT,.TRUE.)
                           ELSE IF (SNAME(11:12).EQ.'SV') THEN
*
*                             Compute approximation to original vector.
*
                              DO 100 I = 1, N
                                 Z(I) = XX(1+(I-1)*ABS(INCX))
                                 XX(1+(I-1)*ABS(INCX)) = X(I)
  100                         CONTINUE
                              CALL CMVCH(TRANS,N,N,ONE,A,NMAX,Z,INCX,
     +                                   ZERO,X,INCX,XT,G,XX,EPS,ERR,
     +                                   FATAL,NOUT,.FALSE.)
                           END IF
                           ERRMAX = MAX(ERRMAX,ERR)
*                          If got really bad answer, report and return.
                           IF (FATAL) GO TO 240
                        ELSE
*                          Avoid repeating tests with N.le.0.
                           GO TO 220
                        END IF
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
      IF (FULL) THEN
         WRITE (NOUT,99993) NC, SNAME(1:6), UPLO, TRANS, DIAG, N, LDA,
     +     INCX
      ELSE IF (BANDED) THEN
         WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, TRANS, DIAG, N, K,
     +     LDA, INCX
      ELSE IF (PACKED) THEN
         WRITE (NOUT,99995) NC, SNAME(1:6), UPLO, TRANS, DIAG, N, INCX
      END IF
*
  260 CONTINUE
      RETURN
*
*
*     End of CCHK3.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT (1X,I6,': ',A6,'(',3('''',A1,''','),I3,', AP, X,',I2,')  ',
     +       '                                    .')
99994 FORMAT (1X,I6,': ',A6,'(',3('''',A1,''','),2(I3,','),' A,',I3,
     +       ', X,',I2,')                               .')
99993 FORMAT (1X,I6,': ',A6,'(',3('''',A1,''','),I3,', A,',I3,', X,',I2,
     +       ')                                   .')
99992 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHK4(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,AA,AS,
     +                 X,XX,XS,Y,YY,YS,YT,G,Z)
*
*     Tests F06SNF and F06SMF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0),
     +                 ONE=(1.0D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NALF, NIDIM, NINC, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), X(NMAX), XS(NMAX*INCMAX),
     +                 XX(NMAX*INCMAX), Y(NMAX), YS(NMAX*INCMAX),
     +                 YT(NMAX), YY(NMAX*INCMAX), Z(NMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, ALS, TRANSL
      DOUBLE PRECISION ERR, ERRMAX
      INTEGER          I, IA, IM, IN, INCX, INCXS, INCY, INCYS, IX, IY,
     +                 J, LAA, LDA, LDAS, LX, LY, M, MS, N, NARGS, NC,
     +                 ND, NS
      LOGICAL          CONJ, NULL, RESET, SAME
*     .. Local Arrays ..
      COMPLEX*16       W(1)
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SMF, F06SNF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCONJG, MAX, MIN
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Executable Statements ..
      CONJ = SNAME(12:12) .EQ. 'C'
*     Define the number of arguments.
      NARGS = 9
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*
      DO 240 IN = 1, NIDIM
         N = IDIM(IN)
         ND = N/2 + 1
*
         DO 220 IM = 1, 2
            IF (IM.EQ.1) M = MAX(N-ND,0)
            IF (IM.EQ.2) M = MIN(N+ND,NMAX)
*
*           Set LDA to 1 more than minimum value if room.
            LDA = M
            IF (LDA.LT.NMAX) LDA = LDA + 1
*           Skip tests if not enough room.
            IF (LDA.GT.NMAX) GO TO 220
            LAA = LDA*N
            NULL = N .LE. 0 .OR. M .LE. 0
*
            DO 200 IX = 1, NINC
               INCX = INC(IX)
               LX = ABS(INCX)*M
*
*              Generate the vector X.
*
               TRANSL = HALF
               CALL CMAKE('GE',' ',' ',1,M,X,1,XX,ABS(INCX),0,M-1,RESET,
     +                    TRANSL)
               IF (M.GT.1) THEN
                  X(M/2) = ZERO
                  XX(1+ABS(INCX)*(M/2-1)) = ZERO
               END IF
*
               DO 180 IY = 1, NINC
                  INCY = INC(IY)
                  LY = ABS(INCY)*N
*
*                 Generate the vector Y.
*
                  TRANSL = ZERO
                  CALL CMAKE('GE',' ',' ',1,N,Y,1,YY,ABS(INCY),0,N-1,
     +                       RESET,TRANSL)
                  IF (N.GT.1) THEN
                     Y(N/2) = ZERO
                     YY(1+ABS(INCY)*(N/2-1)) = ZERO
                  END IF
*
                  DO 160 IA = 1, NALF
                     ALPHA = ALF(IA)
*
*                    Generate the matrix A.
*
                     TRANSL = ZERO
                     CALL CMAKE(SNAME(9:10),' ',' ',M,N,A,NMAX,AA,LDA,
     +                          M-1,N-1,RESET,TRANSL)
*
                     NC = NC + 1
*
*                    Save every datum before calling the subroutine.
*
                     MS = M
                     NS = N
                     ALS = ALPHA
                     DO 20 I = 1, LAA
                        AS(I) = AA(I)
   20                CONTINUE
                     LDAS = LDA
                     DO 40 I = 1, LX
                        XS(I) = XX(I)
   40                CONTINUE
                     INCXS = INCX
                     DO 60 I = 1, LY
                        YS(I) = YY(I)
   60                CONTINUE
                     INCYS = INCY
*
*                    Call the subroutine.
*
                     IF (TRACE) WRITE (NTRA,99994) NC, SNAME(1:6), M, N,
     +                   ALPHA, INCX, INCY, LDA
                     IF (CONJ) THEN
                        CALL F06SNF(M,N,ALPHA,XX,INCX,YY,INCY,AA,LDA)
                     ELSE
                        CALL F06SMF(M,N,ALPHA,XX,INCX,YY,INCY,AA,LDA)
                     END IF
*
*                    Check if error-exit was taken incorrectly.
*
                     IF ( .NOT. OK) THEN
                        WRITE (NOUT,99993)
                        FATAL = .TRUE.
                        GO TO 280
                     END IF
*
*                    See what data changed inside subroutine.
*
                     ISAME(1) = MS .EQ. M
                     ISAME(2) = NS .EQ. N
                     ISAME(3) = ALS .EQ. ALPHA
                     ISAME(4) = LCE(XS,XX,LX)
                     ISAME(5) = INCXS .EQ. INCX
                     ISAME(6) = LCE(YS,YY,LY)
                     ISAME(7) = INCYS .EQ. INCY
                     IF (NULL) THEN
                        ISAME(8) = LCE(AS,AA,LAA)
                     ELSE
                        ISAME(8) = LCERES('GE',' ',M,N,AS,AA,LDA)
                     END IF
                     ISAME(9) = LDAS .EQ. LDA
*
*                    If data was incorrectly changed, report and return.
*
                     SAME = .TRUE.
                     DO 80 I = 1, NARGS
                        SAME = SAME .AND. ISAME(I)
                        IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   80                CONTINUE
                     IF ( .NOT. SAME) THEN
                        FATAL = .TRUE.
                        GO TO 280
                     END IF
*
                     IF ( .NOT. NULL) THEN
*
*                       Check the result column by column.
*
                        IF (INCX.GT.0) THEN
                           DO 100 I = 1, M
                              Z(I) = X(I)
  100                      CONTINUE
                        ELSE
                           DO 120 I = 1, M
                              Z(I) = X(M-I+1)
  120                      CONTINUE
                        END IF
                        DO 140 J = 1, N
                           IF (INCY.GT.0) THEN
                              W(1) = Y(J)
                           ELSE
                              W(1) = Y(N-J+1)
                           END IF
                           IF (CONJ) W(1) = DCONJG(W(1))
                           CALL CMVCH('N',M,1,ALPHA,Z,NMAX,W,1,ONE,
     +                                A(1,J),1,YT,G,AA(1+(J-1)*LDA),EPS,
     +                                ERR,FATAL,NOUT,.TRUE.)
                           ERRMAX = MAX(ERRMAX,ERR)
*                          If got really bad answer, report and return.
                           IF (FATAL) GO TO 260
  140                   CONTINUE
                     ELSE
*                       Avoid repeating tests with M.le.0 or N.le.0.
                        GO TO 220
                     END IF
*
  160             CONTINUE
*
  180          CONTINUE
*
  200       CONTINUE
*
  220    CONTINUE
*
  240 CONTINUE
*
*     Report result.
*
      IF (ERRMAX.LT.THRESH) THEN
         WRITE (NOUT,99999) SNAME(1:6), NC
      ELSE
         WRITE (NOUT,99997) SNAME(1:6), NC, ERRMAX
      END IF
      GO TO 300
*
  260 CONTINUE
      WRITE (NOUT,99995) J
*
  280 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      WRITE (NOUT,99994) NC, SNAME(1:6), M, N, ALPHA, INCX, INCY, LDA
*
  300 CONTINUE
      RETURN
*
*
*     End of CCHK4.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
99994 FORMAT (1X,I6,': ',A6,'(',2(I3,','),'(',F4.1,',',F4.1,'), X,',I2,
     +       ', Y,',I2,', A,',I3,')                         .')
99993 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHK5(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,AA,AS,
     +                 X,XX,XS,Y,YY,YS,YT,G,Z)
*
*     Tests F06SPF and F06SQF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0),
     +                 ONE=(1.0D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NALF, NIDIM, NINC, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), X(NMAX), XS(NMAX*INCMAX),
     +                 XX(NMAX*INCMAX), Y(NMAX), YS(NMAX*INCMAX),
     +                 YT(NMAX), YY(NMAX*INCMAX), Z(NMAX)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, TRANSL
      DOUBLE PRECISION ERR, ERRMAX, RALPHA, RALS
      INTEGER          I, IA, IC, IN, INCX, INCXS, IX, J, JA, JJ, LAA,
     +                 LDA, LDAS, LJ, LX, N, NARGS, NC, NS
      LOGICAL          FULL, NULL, PACKED, RESET, SAME, UPPER
      CHARACTER*1      UPLO, UPLOS
      CHARACTER*2      ICH
*     .. Local Arrays ..
      COMPLEX*16       W(1)
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SPF, F06SQF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCMPLX, DCONJG, MAX, DBLE
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICH/'UL'/
*     .. Executable Statements ..
      FULL = SNAME(10:10) .EQ. 'E'
      PACKED = SNAME(10:10) .EQ. 'P'
*     Define the number of arguments.
      IF (FULL) THEN
         NARGS = 7
      ELSE IF (PACKED) THEN
         NARGS = 6
      END IF
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*
      DO 200 IN = 1, NIDIM
         N = IDIM(IN)
*        Set LDA to 1 more than minimum value if room.
         LDA = N
         IF (LDA.LT.NMAX) LDA = LDA + 1
*        Skip tests if not enough room.
         IF (LDA.GT.NMAX) GO TO 200
         IF (PACKED) THEN
            LAA = (N*(N+1))/2
         ELSE
            LAA = LDA*N
         END IF
*
         DO 180 IC = 1, 2
            UPLO = ICH(IC:IC)
            UPPER = UPLO .EQ. 'U'
*
            DO 160 IX = 1, NINC
               INCX = INC(IX)
               LX = ABS(INCX)*N
*
*              Generate the vector X.
*
               TRANSL = HALF
               CALL CMAKE('GE',' ',' ',1,N,X,1,XX,ABS(INCX),0,N-1,RESET,
     +                    TRANSL)
               IF (N.GT.1) THEN
                  X(N/2) = ZERO
                  XX(1+ABS(INCX)*(N/2-1)) = ZERO
               END IF
*
               DO 140 IA = 1, NALF
                  RALPHA = DBLE(ALF(IA))
                  ALPHA = DCMPLX(RALPHA,RZERO)
                  NULL = N .LE. 0 .OR. RALPHA .EQ. RZERO
*
*                 Generate the matrix A.
*
                  TRANSL = ZERO
                  CALL CMAKE(SNAME(9:10),UPLO,' ',N,N,A,NMAX,AA,LDA,N-1,
     +                       N-1,RESET,TRANSL)
*
                  NC = NC + 1
*
*                 Save every datum before calling the subroutine.
*
                  UPLOS = UPLO
                  NS = N
                  RALS = RALPHA
                  DO 20 I = 1, LAA
                     AS(I) = AA(I)
   20             CONTINUE
                  LDAS = LDA
                  DO 40 I = 1, LX
                     XS(I) = XX(I)
   40             CONTINUE
                  INCXS = INCX
*
*                 Call the subroutine.
*
                  IF (FULL) THEN
                     IF (TRACE) WRITE (NTRA,99993) NC, SNAME(1:6), UPLO,
     +                   N, RALPHA, INCX, LDA
                     CALL F06SPF(UPLO,N,RALPHA,XX,INCX,AA,LDA)
                  ELSE IF (PACKED) THEN
                     IF (TRACE) WRITE (NTRA,99994) NC, SNAME(1:6), UPLO,
     +                   N, RALPHA, INCX
                     CALL F06SQF(UPLO,N,RALPHA,XX,INCX,AA)
                  END IF
*
*                 Check if error-exit was taken incorrectly.
*
                  IF ( .NOT. OK) THEN
                     WRITE (NOUT,99992)
                     FATAL = .TRUE.
                     GO TO 240
                  END IF
*
*                 See what data changed inside subroutines.
*
                  ISAME(1) = UPLO .EQ. UPLOS
                  ISAME(2) = NS .EQ. N
                  ISAME(3) = RALS .EQ. RALPHA
                  ISAME(4) = LCE(XS,XX,LX)
                  ISAME(5) = INCXS .EQ. INCX
                  IF (NULL) THEN
                     ISAME(6) = LCE(AS,AA,LAA)
                  ELSE
                     ISAME(6) = LCERES(SNAME(9:10),UPLO,N,N,AS,AA,LDA)
                  END IF
                  IF ( .NOT. PACKED) THEN
                     ISAME(7) = LDAS .EQ. LDA
                  END IF
*
*                 If data was incorrectly changed, report and return.
*
                  SAME = .TRUE.
                  DO 60 I = 1, NARGS
                     SAME = SAME .AND. ISAME(I)
                     IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   60             CONTINUE
                  IF ( .NOT. SAME) THEN
                     FATAL = .TRUE.
                     GO TO 240
                  END IF
*
                  IF ( .NOT. NULL) THEN
*
*                    Check the result column by column.
*
                     IF (INCX.GT.0) THEN
                        DO 80 I = 1, N
                           Z(I) = X(I)
   80                   CONTINUE
                     ELSE
                        DO 100 I = 1, N
                           Z(I) = X(N-I+1)
  100                   CONTINUE
                     END IF
                     JA = 1
                     DO 120 J = 1, N
                        W(1) = DCONJG(Z(J))
                        IF (UPPER) THEN
                           JJ = 1
                           LJ = J
                        ELSE
                           JJ = J
                           LJ = N - J + 1
                        END IF
                        CALL CMVCH('N',LJ,1,ALPHA,Z(JJ),LJ,W,1,ONE,
     +                             A(JJ,J),1,YT,G,AA(JA),EPS,ERR,FATAL,
     +                             NOUT,.TRUE.)
                        IF (FULL) THEN
                           IF (UPPER) THEN
                              JA = JA + LDA
                           ELSE
                              JA = JA + LDA + 1
                           END IF
                        ELSE
                           JA = JA + LJ
                        END IF
                        ERRMAX = MAX(ERRMAX,ERR)
*                       If got really bad answer, report and return.
                        IF (FATAL) GO TO 220
  120                CONTINUE
                  ELSE
*                    Avoid repeating tests if N.le.0.
                     IF (N.LE.0) GO TO 200
                  END IF
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
      WRITE (NOUT,99995) J
*
  240 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      IF (FULL) THEN
         WRITE (NOUT,99993) NC, SNAME(1:6), UPLO, N, RALPHA, INCX, LDA
      ELSE IF (PACKED) THEN
         WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, N, RALPHA, INCX
      END IF
*
  260 CONTINUE
      RETURN
*
*
*     End of CCHK5.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
99994 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',',F4.1,', X,',I2,', AP',
     +       ')                                         .')
99993 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',',F4.1,', X,',I2,', A,',
     +       I3,')                                      .')
99992 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHK6(SNAME,EPS,THRESH,NOUT,NTRA,TRACE,REWI,FATAL,
     +                 NIDIM,IDIM,NALF,ALF,NINC,INC,NMAX,INCMAX,A,AA,AS,
     +                 X,XX,XS,Y,YY,YS,YT,G,Z)
*
*     Tests F06SRF and F06SSF.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, HALF, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),HALF=(0.5D0,0.0D0),
     +                 ONE=(1.0D0,0.0D0))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EPS, THRESH
      INTEGER          INCMAX, NALF, NIDIM, NINC, NMAX, NOUT, NTRA
      LOGICAL          FATAL, REWI, TRACE
      CHARACTER*13     SNAME
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,NMAX), AA(NMAX*NMAX), ALF(NALF),
     +                 AS(NMAX*NMAX), X(NMAX), XS(NMAX*INCMAX),
     +                 XX(NMAX*INCMAX), Y(NMAX), YS(NMAX*INCMAX),
     +                 YT(NMAX), YY(NMAX*INCMAX), Z(NMAX,2)
      DOUBLE PRECISION G(NMAX)
      INTEGER          IDIM(NIDIM), INC(NINC)
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, ALS, TRANSL
      DOUBLE PRECISION ERR, ERRMAX
      INTEGER          I, IA, IC, IN, INCX, INCXS, INCY, INCYS, IX, IY,
     +                 J, JA, JJ, LAA, LDA, LDAS, LJ, LX, LY, N, NARGS,
     +                 NC, NS
      LOGICAL          FULL, NULL, PACKED, RESET, SAME, UPPER
      CHARACTER*1      UPLO, UPLOS
      CHARACTER*2      ICH
*     .. Local Arrays ..
      COMPLEX*16       W(2)
      LOGICAL          ISAME(13)
*     .. External Functions ..
      LOGICAL          LCE, LCERES
      EXTERNAL         LCE, LCERES
*     .. External Subroutines ..
      EXTERNAL         CMAKE, CMVCH, F06SRF, F06SSF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DCONJG, MAX
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Data statements ..
      DATA             ICH/'UL'/
*     .. Executable Statements ..
      FULL = SNAME(10:10) .EQ. 'E'
      PACKED = SNAME(10:10) .EQ. 'P'
*     Define the number of arguments.
      IF (FULL) THEN
         NARGS = 9
      ELSE IF (PACKED) THEN
         NARGS = 8
      END IF
*
      NC = 0
      RESET = .TRUE.
      ERRMAX = RZERO
*
      DO 280 IN = 1, NIDIM
         N = IDIM(IN)
*        Set LDA to 1 more than minimum value if room.
         LDA = N
         IF (LDA.LT.NMAX) LDA = LDA + 1
*        Skip tests if not enough room.
         IF (LDA.GT.NMAX) GO TO 280
         IF (PACKED) THEN
            LAA = (N*(N+1))/2
         ELSE
            LAA = LDA*N
         END IF
*
         DO 260 IC = 1, 2
            UPLO = ICH(IC:IC)
            UPPER = UPLO .EQ. 'U'
*
            DO 240 IX = 1, NINC
               INCX = INC(IX)
               LX = ABS(INCX)*N
*
*              Generate the vector X.
*
               TRANSL = HALF
               CALL CMAKE('GE',' ',' ',1,N,X,1,XX,ABS(INCX),0,N-1,RESET,
     +                    TRANSL)
               IF (N.GT.1) THEN
                  X(N/2) = ZERO
                  XX(1+ABS(INCX)*(N/2-1)) = ZERO
               END IF
*
               DO 220 IY = 1, NINC
                  INCY = INC(IY)
                  LY = ABS(INCY)*N
*
*                 Generate the vector Y.
*
                  TRANSL = ZERO
                  CALL CMAKE('GE',' ',' ',1,N,Y,1,YY,ABS(INCY),0,N-1,
     +                       RESET,TRANSL)
                  IF (N.GT.1) THEN
                     Y(N/2) = ZERO
                     YY(1+ABS(INCY)*(N/2-1)) = ZERO
                  END IF
*
                  DO 200 IA = 1, NALF
                     ALPHA = ALF(IA)
                     NULL = N .LE. 0 .OR. ALPHA .EQ. ZERO
*
*                    Generate the matrix A.
*
                     TRANSL = ZERO
                     CALL CMAKE(SNAME(9:10),UPLO,' ',N,N,A,NMAX,AA,LDA,
     +                          N-1,N-1,RESET,TRANSL)
*
                     NC = NC + 1
*
*                    Save every datum before calling the subroutine.
*
                     UPLOS = UPLO
                     NS = N
                     ALS = ALPHA
                     DO 20 I = 1, LAA
                        AS(I) = AA(I)
   20                CONTINUE
                     LDAS = LDA
                     DO 40 I = 1, LX
                        XS(I) = XX(I)
   40                CONTINUE
                     INCXS = INCX
                     DO 60 I = 1, LY
                        YS(I) = YY(I)
   60                CONTINUE
                     INCYS = INCY
*
*                    Call the subroutine.
*
                     IF (FULL) THEN
                        IF (TRACE) WRITE (NTRA,99993) NC, SNAME(1:6),
     +                      UPLO, N, ALPHA, INCX, INCY, LDA
                        CALL F06SRF(UPLO,N,ALPHA,XX,INCX,YY,INCY,AA,LDA)
                     ELSE IF (PACKED) THEN
                        IF (TRACE) WRITE (NTRA,99994) NC, SNAME(1:6),
     +                      UPLO, N, ALPHA, INCX, INCY
                        CALL F06SSF(UPLO,N,ALPHA,XX,INCX,YY,INCY,AA)
                     END IF
*
*                    Check if error-exit was taken incorrectly.
*
                     IF ( .NOT. OK) THEN
                        WRITE (NOUT,99992)
                        FATAL = .TRUE.
                        GO TO 320
                     END IF
*
*                    See what data changed inside subroutines.
*
                     ISAME(1) = UPLO .EQ. UPLOS
                     ISAME(2) = NS .EQ. N
                     ISAME(3) = ALS .EQ. ALPHA
                     ISAME(4) = LCE(XS,XX,LX)
                     ISAME(5) = INCXS .EQ. INCX
                     ISAME(6) = LCE(YS,YY,LY)
                     ISAME(7) = INCYS .EQ. INCY
                     IF (NULL) THEN
                        ISAME(8) = LCE(AS,AA,LAA)
                     ELSE
                        ISAME(8) = LCERES(SNAME(9:10),UPLO,N,N,AS,AA,
     +                             LDA)
                     END IF
                     IF ( .NOT. PACKED) THEN
                        ISAME(9) = LDAS .EQ. LDA
                     END IF
*
*                    If data was incorrectly changed, report and return.
*
                     SAME = .TRUE.
                     DO 80 I = 1, NARGS
                        SAME = SAME .AND. ISAME(I)
                        IF ( .NOT. ISAME(I)) WRITE (NOUT,99998) I
   80                CONTINUE
                     IF ( .NOT. SAME) THEN
                        FATAL = .TRUE.
                        GO TO 320
                     END IF
*
                     IF ( .NOT. NULL) THEN
*
*                       Check the result column by column.
*
                        IF (INCX.GT.0) THEN
                           DO 100 I = 1, N
                              Z(I,1) = X(I)
  100                      CONTINUE
                        ELSE
                           DO 120 I = 1, N
                              Z(I,1) = X(N-I+1)
  120                      CONTINUE
                        END IF
                        IF (INCY.GT.0) THEN
                           DO 140 I = 1, N
                              Z(I,2) = Y(I)
  140                      CONTINUE
                        ELSE
                           DO 160 I = 1, N
                              Z(I,2) = Y(N-I+1)
  160                      CONTINUE
                        END IF
                        JA = 1
                        DO 180 J = 1, N
                           W(1) = ALPHA*DCONJG(Z(J,2))
                           W(2) = DCONJG(ALPHA)*DCONJG(Z(J,1))
                           IF (UPPER) THEN
                              JJ = 1
                              LJ = J
                           ELSE
                              JJ = J
                              LJ = N - J + 1
                           END IF
                           CALL CMVCH('N',LJ,2,ONE,Z(JJ,1),NMAX,W,1,ONE,
     +                                A(JJ,J),1,YT,G,AA(JA),EPS,ERR,
     +                                FATAL,NOUT,.TRUE.)
                           IF (FULL) THEN
                              IF (UPPER) THEN
                                 JA = JA + LDA
                              ELSE
                                 JA = JA + LDA + 1
                              END IF
                           ELSE
                              JA = JA + LJ
                           END IF
                           ERRMAX = MAX(ERRMAX,ERR)
*                          If got really bad answer, report and return.
                           IF (FATAL) GO TO 300
  180                   CONTINUE
                     ELSE
*                       Avoid repeating tests with N.le.0.
                        IF (N.LE.0) GO TO 280
                     END IF
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
      GO TO 340
*
  300 CONTINUE
      WRITE (NOUT,99995) J
*
  320 CONTINUE
      WRITE (NOUT,99996) SNAME(1:6)
      IF (FULL) THEN
         WRITE (NOUT,99993) NC, SNAME(1:6), UPLO, N, ALPHA, INCX, INCY,
     +     LDA
      ELSE IF (PACKED) THEN
         WRITE (NOUT,99994) NC, SNAME(1:6), UPLO, N, ALPHA, INCX, INCY
      END IF
*
  340 CONTINUE
      RETURN
*
*
*     End of CCHK6.
*
99999 FORMAT (' ',A6,' PASSED THE COMPUTATIONAL TESTS (',I6,' CALLS)')
99998 FORMAT (' ******* FATAL ERROR - PARAMETER NUMBER ',I2,' WAS CHAN',
     +       'GED INCORRECTLY *******')
99997 FORMAT (' ',A6,' COMPLETED THE COMPUTATIONAL TESTS (',I6,
     +       ' CALLS)',/' ******* BUT WITH MAXIMUM TEST RATIO',F8.2,
     +       ' - SUSPECT *******')
99996 FORMAT (' ******* ',A6,' FAILED ON CALL NUMBER:')
99995 FORMAT ('      THESE ARE THE RESULTS FOR COLUMN ',I3)
99994 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',(',F4.1,',',F4.1,
     +       '), X,',I2,', Y,',I2,', AP)                            .')
99993 FORMAT (1X,I6,': ',A6,'(''',A1,''',',I3,',(',F4.1,',',F4.1,
     +       '), X,',I2,', Y,',I2,', A,',I3,')                        ',
     +       ' .')
99992 FORMAT (' ******* FATAL ERROR - ERROR-EXIT TAKEN ON VALID CALL *',
     +       '******')
      END
      SUBROUTINE CCHKE(ISNUM,SRNAMT,NOUT)
*
*     Tests the error exits from the Level 2 Blas.
*     Requires a special version of the error-handling routine F06AAZ.
*     ALPHA, RALPHA, BETA, A, X and Y should not need to be defined.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Scalar Arguments ..
      INTEGER          ISNUM, NOUT
      CHARACTER*13     SRNAMT
*     .. Scalars in Common ..
      INTEGER          INFOT, NOUTC
      LOGICAL          LERR, OK
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, BETA
      DOUBLE PRECISION RALPHA
*     .. Local Arrays ..
      COMPLEX*16       A(1,1), X(1), Y(1)
*     .. External Subroutines ..
      EXTERNAL         CHKXER, F06SAF, F06SBF, F06SCF, F06SDF, F06SEF,
     +                 F06SFF, F06SGF, F06SHF, F06SJF, F06SKF, F06SLF,
     +                 F06SMF, F06SNF, F06SPF, F06SQF, F06SRF, F06SSF
*     .. Common blocks ..
      COMMON           /INFOC/INFOT, NOUTC, OK, LERR
*     .. Executable Statements ..
*     OK is set to .FALSE. by the special version of F06AAZ or by CHKXER
*     if anything is wrong.
      OK = .TRUE.
*     LERR is set to .TRUE. by the special version of F06AAZ each time
*     it is called, and is then tested and re-set by CHKXER.
      LERR = .FALSE.
      GO TO (20,40,60,80,100,120,140,160,180,
     +       200,220,240,260,280,300,320,340) ISNUM
   20 INFOT = 1
      CALL F06SAF('/',0,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SAF('N',-1,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SAF('N',0,-1,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06SAF('N',2,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06SAF('N',0,0,ALPHA,A,1,X,0,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06SAF('N',0,0,ALPHA,A,1,X,1,BETA,Y,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
   40 INFOT = 1
      CALL F06SBF('/',0,0,0,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SBF('N',-1,0,0,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SBF('N',0,-1,0,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SBF('N',0,0,-1,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SBF('N',2,0,0,-1,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06SBF('N',0,0,1,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06SBF('N',0,0,0,0,ALPHA,A,1,X,0,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 13
      CALL F06SBF('N',0,0,0,0,ALPHA,A,1,X,1,BETA,Y,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
   60 INFOT = 1
      CALL F06SCF('/',0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SCF('U',-1,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SCF('U',2,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SCF('U',0,ALPHA,A,1,X,0,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 10
      CALL F06SCF('U',0,ALPHA,A,1,X,1,BETA,Y,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
   80 INFOT = 1
      CALL F06SDF('/',0,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SDF('U',-1,0,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SDF('U',0,-1,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06SDF('U',0,1,ALPHA,A,1,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06SDF('U',0,0,ALPHA,A,1,X,0,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 11
      CALL F06SDF('U',0,0,ALPHA,A,1,X,1,BETA,Y,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  100 INFOT = 1
      CALL F06SEF('/',0,ALPHA,A,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SEF('U',-1,ALPHA,A,X,1,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06SEF('U',0,ALPHA,A,X,0,BETA,Y,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SEF('U',0,ALPHA,A,X,1,BETA,Y,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  120 INFOT = 1
      CALL F06SFF('/','N','N',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SFF('U','/','N',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SFF('U','N','/',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SFF('U','N','N',-1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06SFF('U','N','N',2,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06SFF('U','N','N',0,A,1,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  140 INFOT = 1
      CALL F06SGF('/','N','N',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SGF('U','/','N',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SGF('U','N','/',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SGF('U','N','N',-1,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SGF('U','N','N',0,-1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SGF('U','N','N',0,1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SGF('U','N','N',0,0,A,1,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  160 INFOT = 1
      CALL F06SHF('/','N','N',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SHF('U','/','N',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SHF('U','N','/',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SHF('U','N','N',-1,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SHF('U','N','N',0,A,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  180 INFOT = 1
      CALL F06SJF('/','N','N',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SJF('U','/','N',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SJF('U','N','/',0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SJF('U','N','N',-1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 6
      CALL F06SJF('U','N','N',2,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 8
      CALL F06SJF('U','N','N',0,A,1,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  200 INFOT = 1
      CALL F06SKF('/','N','N',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SKF('U','/','N',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SKF('U','N','/',0,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SKF('U','N','N',-1,0,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SKF('U','N','N',0,-1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SKF('U','N','N',0,1,A,1,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SKF('U','N','N',0,0,A,1,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  220 INFOT = 1
      CALL F06SLF('/','N','N',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SLF('U','/','N',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 3
      CALL F06SLF('U','N','/',0,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 4
      CALL F06SLF('U','N','N',-1,A,X,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SLF('U','N','N',0,A,X,0)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  240 INFOT = 1
      CALL F06SNF(-1,0,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SNF(0,-1,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SNF(0,0,ALPHA,X,0,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SNF(0,0,ALPHA,X,1,Y,0,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SNF(2,0,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  260 INFOT = 1
      CALL F06SMF(-1,0,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SMF(0,-1,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SMF(0,0,ALPHA,X,0,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SMF(0,0,ALPHA,X,1,Y,0,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SMF(2,0,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  280 INFOT = 1
      CALL F06SPF('/',0,RALPHA,X,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SPF('U',-1,RALPHA,X,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SPF('U',0,RALPHA,X,0,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SPF('U',2,RALPHA,X,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  300 INFOT = 1
      CALL F06SQF('/',0,RALPHA,X,1,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SQF('U',-1,RALPHA,X,1,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SQF('U',0,RALPHA,X,0,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  320 INFOT = 1
      CALL F06SRF('/',0,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SRF('U',-1,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SRF('U',0,ALPHA,X,0,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SRF('U',0,ALPHA,X,1,Y,0,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 9
      CALL F06SRF('U',2,ALPHA,X,1,Y,1,A,1)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      GO TO 360
  340 INFOT = 1
      CALL F06SSF('/',0,ALPHA,X,1,Y,1,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 2
      CALL F06SSF('U',-1,ALPHA,X,1,Y,1,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 5
      CALL F06SSF('U',0,ALPHA,X,0,Y,1,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
      INFOT = 7
      CALL F06SSF('U',0,ALPHA,X,1,Y,0,A)
      CALL CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
*
  360 IF (OK) THEN
         WRITE (NOUT,99999) SRNAMT(1:6)
      ELSE
         WRITE (NOUT,99998) SRNAMT(1:6)
      END IF
      RETURN
*
*
*     End of CCHKE.
*
99999 FORMAT (' ',A6,' PASSED THE TESTS OF ERROR-EXITS')
99998 FORMAT (' ******* ',A6,' FAILED THE TESTS OF ERROR-EXITS *******')
      END
      SUBROUTINE CMAKE(TYPE,UPLO,DIAG,M,N,A,NMAX,AA,LDA,KL,KU,RESET,
     +                 TRANSL)
*
*     Generates values for an M by N matrix A within the bandwidth
*     defined by KL and KU.
*     Stores the values in the array AA in the data structure required
*     by the routine, with unwanted elements set to rogue value.
*
*     TYPE is 'GE', 'GB', 'HE', 'HB', 'HP', 'TR', 'TB' OR 'TP'.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO, ONE
      PARAMETER        (ZERO=(0.0D0,0.0D0),ONE=(1.0D0,0.0D0))
      COMPLEX*16       ROGUE
      PARAMETER        (ROGUE=(-1.0D10,1.0D10))
      DOUBLE PRECISION RZERO
      PARAMETER        (RZERO=0.0D0)
      DOUBLE PRECISION RROGUE
      PARAMETER        (RROGUE=-1.0D10)
*     .. Scalar Arguments ..
      COMPLEX*16       TRANSL
      INTEGER          KL, KU, LDA, M, N, NMAX
      LOGICAL          RESET
      CHARACTER*1      DIAG, UPLO
      CHARACTER*2      TYPE
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,*), AA(*)
*     .. Local Scalars ..
      INTEGER          I, I1, I2, I3, IBEG, IEND, IOFF, J, JJ, KK
      LOGICAL          GEN, LOWER, SYM, TRI, UNIT, UPPER
*     .. External Functions ..
      COMPLEX*16       CBEG
      EXTERNAL         CBEG
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX, DCONJG, MAX, MIN, DBLE
*     .. Executable Statements ..
      GEN = TYPE(1:1) .EQ. 'G'
      SYM = TYPE(1:1) .EQ. 'H'
      TRI = TYPE(1:1) .EQ. 'T'
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
               IF ((I.LE.J .AND. J-I.LE.KU)
     +             .OR. (I.GE.J .AND. I-J.LE.KL)) THEN
                  A(I,J) = CBEG(RESET) + TRANSL
               ELSE
                  A(I,J) = ZERO
               END IF
               IF (I.NE.J) THEN
                  IF (SYM) THEN
                     A(J,I) = DCONJG(A(I,J))
                  ELSE IF (TRI) THEN
                     A(J,I) = ZERO
                  END IF
               END IF
            END IF
   20    CONTINUE
         IF (SYM) A(J,J) = DCMPLX(DBLE(A(J,J)),RZERO)
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
      ELSE IF (TYPE.EQ.'GB') THEN
         DO 180 J = 1, N
            DO 120 I1 = 1, KU + 1 - J
               AA(I1+(J-1)*LDA) = ROGUE
  120       CONTINUE
            DO 140 I2 = I1, MIN(KL+KU+1,KU+1+M-J)
               AA(I2+(J-1)*LDA) = A(I2+J-KU-1,J)
  140       CONTINUE
            DO 160 I3 = I2, LDA
               AA(I3+(J-1)*LDA) = ROGUE
  160       CONTINUE
  180    CONTINUE
      ELSE IF (TYPE.EQ.'HE' .OR. TYPE.EQ.'TR') THEN
         DO 260 J = 1, N
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
            DO 200 I = 1, IBEG - 1
               AA(I+(J-1)*LDA) = ROGUE
  200       CONTINUE
            DO 220 I = IBEG, IEND
               AA(I+(J-1)*LDA) = A(I,J)
  220       CONTINUE
            DO 240 I = IEND + 1, LDA
               AA(I+(J-1)*LDA) = ROGUE
  240       CONTINUE
            IF (SYM) THEN
               JJ = J + (J-1)*LDA
               AA(JJ) = DCMPLX(DBLE(AA(JJ)),RROGUE)
            END IF
  260    CONTINUE
      ELSE IF (TYPE.EQ.'HB' .OR. TYPE.EQ.'TB') THEN
         DO 340 J = 1, N
            IF (UPPER) THEN
               KK = KL + 1
               IBEG = MAX(1,KL+2-J)
               IF (UNIT) THEN
                  IEND = KL
               ELSE
                  IEND = KL + 1
               END IF
            ELSE
               KK = 1
               IF (UNIT) THEN
                  IBEG = 2
               ELSE
                  IBEG = 1
               END IF
               IEND = MIN(KL+1,1+M-J)
            END IF
            DO 280 I = 1, IBEG - 1
               AA(I+(J-1)*LDA) = ROGUE
  280       CONTINUE
            DO 300 I = IBEG, IEND
               AA(I+(J-1)*LDA) = A(I+J-KK,J)
  300       CONTINUE
            DO 320 I = IEND + 1, LDA
               AA(I+(J-1)*LDA) = ROGUE
  320       CONTINUE
            IF (SYM) THEN
               JJ = KK + (J-1)*LDA
               AA(JJ) = DCMPLX(DBLE(AA(JJ)),RROGUE)
            END IF
  340    CONTINUE
      ELSE IF (TYPE.EQ.'HP' .OR. TYPE.EQ.'TP') THEN
         IOFF = 0
         DO 380 J = 1, N
            IF (UPPER) THEN
               IBEG = 1
               IEND = J
            ELSE
               IBEG = J
               IEND = N
            END IF
            DO 360 I = IBEG, IEND
               IOFF = IOFF + 1
               AA(IOFF) = A(I,J)
               IF (I.EQ.J) THEN
                  IF (UNIT) AA(IOFF) = ROGUE
                  IF (SYM) AA(IOFF) = DCMPLX(DBLE(AA(IOFF)),RROGUE)
               END IF
  360       CONTINUE
  380    CONTINUE
      END IF
      RETURN
*
*     End of CMAKE.
*
      END
      SUBROUTINE CMVCH(TRANS,M,N,ALPHA,A,NMAX,X,INCX,BETA,Y,INCY,YT,G,
     +                 YY,EPS,ERR,FATAL,NOUT,MV)
*
*     Checks the results of the computational tests.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Parameters ..
      COMPLEX*16       ZERO
      PARAMETER        (ZERO=(0.0D0,0.0D0))
      DOUBLE PRECISION RZERO, RONE
      PARAMETER        (RZERO=0.0D0,RONE=1.0D0)
*     .. Scalar Arguments ..
      COMPLEX*16       ALPHA, BETA
      DOUBLE PRECISION EPS, ERR
      INTEGER          INCX, INCY, M, N, NMAX, NOUT
      LOGICAL          FATAL, MV
      CHARACTER*1      TRANS
*     .. Array Arguments ..
      COMPLEX*16       A(NMAX,*), X(*), Y(*), YT(*), YY(*)
      DOUBLE PRECISION G(*)
*     .. Local Scalars ..
      COMPLEX*16       C
      DOUBLE PRECISION ERRI
      INTEGER          I, INCXL, INCYL, IY, J, JX, KX, KY, ML, NL
      LOGICAL          CTRAN, TRAN
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, DIMAG, DCONJG, MAX, DBLE, SQRT
*     .. Statement Functions ..
      DOUBLE PRECISION ABS1
*     .. Statement Function definitions ..
      ABS1(C) = ABS(DBLE(C)) + ABS(DIMAG(C))
*     .. Executable Statements ..
      TRAN = TRANS .EQ. 'T'
      CTRAN = TRANS .EQ. 'C'
      IF (TRAN .OR. CTRAN) THEN
         ML = N
         NL = M
      ELSE
         ML = M
         NL = N
      END IF
      IF (INCX.LT.0) THEN
         KX = NL
         INCXL = -1
      ELSE
         KX = 1
         INCXL = 1
      END IF
      IF (INCY.LT.0) THEN
         KY = ML
         INCYL = -1
      ELSE
         KY = 1
         INCYL = 1
      END IF
*
*     Compute expected result in YT using data in A, X and Y.
*     Compute gauges in G.
*
      IY = KY
      DO 80 I = 1, ML
         YT(IY) = ZERO
         G(IY) = RZERO
         JX = KX
         IF (TRAN) THEN
            DO 20 J = 1, NL
               YT(IY) = YT(IY) + A(J,I)*X(JX)
               G(IY) = G(IY) + ABS1(A(J,I))*ABS1(X(JX))
               JX = JX + INCXL
   20       CONTINUE
         ELSE IF (CTRAN) THEN
            DO 40 J = 1, NL
               YT(IY) = YT(IY) + DCONJG(A(J,I))*X(JX)
               G(IY) = G(IY) + ABS1(A(J,I))*ABS1(X(JX))
               JX = JX + INCXL
   40       CONTINUE
         ELSE
            DO 60 J = 1, NL
               YT(IY) = YT(IY) + A(I,J)*X(JX)
               G(IY) = G(IY) + ABS1(A(I,J))*ABS1(X(JX))
               JX = JX + INCXL
   60       CONTINUE
         END IF
         YT(IY) = ALPHA*YT(IY) + BETA*Y(IY)
         G(IY) = ABS1(ALPHA)*G(IY) + ABS1(BETA)*ABS1(Y(IY))
         IY = IY + INCYL
   80 CONTINUE
*
*     Compute the error ratio for this result.
*
      ERR = ZERO
      DO 100 I = 1, ML
         ERRI = ABS1(YT(I)-YY(1+(I-1)*ABS(INCY)))/EPS
         IF (G(I).NE.RZERO) ERRI = ERRI/G(I)
         ERR = MAX(ERR,ERRI)
         IF (ERR*SQRT(EPS).GE.RONE) GO TO 120
  100 CONTINUE
*     If the loop completes, all results are at least half accurate.
      GO TO 160
*
*     Report fatal error.
*
  120 FATAL = .TRUE.
      WRITE (NOUT,99999)
      DO 140 I = 1, ML
         IF (MV) THEN
            WRITE (NOUT,99998) I, YT(I), YY(1+(I-1)*ABS(INCY))
         ELSE
            WRITE (NOUT,99998) I, YY(1+(I-1)*ABS(INCY)), YT(I)
         END IF
  140 CONTINUE
*
  160 CONTINUE
      RETURN
*
*
*     End of CMVCH.
*
99999 FORMAT (' ******* FATAL ERROR - COMPUTED RESULT IS LESS THAN HAL',
     +       'F ACCURATE *******',/'                       EXPECTED RE',
     +       'SULT                    COMPUTED RESULT')
99998 FORMAT (1X,I7,2('  (',G15.6,',',G15.6,')'))
      END
      LOGICAL FUNCTION LCE(RI,RJ,LR)
*
*     Tests if two arrays are identical.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Scalar Arguments ..
      INTEGER              LR
*     .. Array Arguments ..
      COMPLEX*16           RI(*), RJ(*)
*     .. Local Scalars ..
      INTEGER              I
*     .. Executable Statements ..
      DO 20 I = 1, LR
         IF (RI(I).NE.RJ(I)) GO TO 40
   20 CONTINUE
      LCE = .TRUE.
      GO TO 60
   40 CONTINUE
      LCE = .FALSE.
   60 RETURN
*
*     End of LCE.
*
      END
      LOGICAL FUNCTION LCERES(TYPE,UPLO,M,N,AA,AS,LDA)
*
*     Tests if selected elements in two arrays are equal.
*
*     TYPE is 'GE', 'HE' or 'HP'.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Scalar Arguments ..
      INTEGER                 LDA, M, N
      CHARACTER*1             UPLO
      CHARACTER*2             TYPE
*     .. Array Arguments ..
      COMPLEX*16              AA(LDA,*), AS(LDA,*)
*     .. Local Scalars ..
      INTEGER                 I, IBEG, IEND, J
      LOGICAL                 UPPER
*     .. Executable Statements ..
      UPPER = UPLO .EQ. 'U'
      IF (TYPE.EQ.'GE') THEN
         DO 40 J = 1, N
            DO 20 I = M + 1, LDA
               IF (AA(I,J).NE.AS(I,J)) GO TO 120
   20       CONTINUE
   40    CONTINUE
      ELSE IF (TYPE.EQ.'HE') THEN
         DO 100 J = 1, N
            IF (UPPER) THEN
               IBEG = 1
               IEND = J
            ELSE
               IBEG = J
               IEND = N
            END IF
            DO 60 I = 1, IBEG - 1
               IF (AA(I,J).NE.AS(I,J)) GO TO 120
   60       CONTINUE
            DO 80 I = IEND + 1, LDA
               IF (AA(I,J).NE.AS(I,J)) GO TO 120
   80       CONTINUE
  100    CONTINUE
      END IF
*
      LCERES = .TRUE.
      GO TO 140
  120 CONTINUE
      LCERES = .FALSE.
  140 RETURN
*
*     End of LCERES.
*
      END
      COMPLEX*16     FUNCTION CBEG(RESET)
*
*     Generates complex numbers as pairs of random numbers uniformly
*     distributed between -0.5 and 0.5.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Scalar Arguments ..
      LOGICAL                      RESET
*     .. Local Scalars ..
      INTEGER                      I, IC, J, MI, MJ
*     .. Intrinsic Functions ..
      INTRINSIC                    DCMPLX
*     .. Save statement ..
      SAVE                         I, IC, J, MI, MJ
*     .. Executable Statements ..
      IF (RESET) THEN
*        Initialize local variables.
         MI = 891
         MJ = 457
         I = 7
         J = 7
         IC = 0
         RESET = .FALSE.
      END IF
*
*     The sequence of values of I or J is bounded between 1 and 999.
*     If initial I or J = 1,2,3,6,7 or 9, the period will be 50.
*     If initial I or J = 4 or 8, the period will be 25.
*     If initial I or J = 5, the period will be 10.
*     IC is used to break up the period by skipping 1 value of I or J
*     in 6.
*
      IC = IC + 1
   20 I = I*MI
      J = J*MJ
      I = I - 1000*(I/1000)
      J = J - 1000*(J/1000)
      IF (IC.GE.5) THEN
         IC = 0
         GO TO 20
      END IF
      CBEG = DCMPLX((I-500)/1001.0D0,(J-500)/1001.0D0)
      RETURN
*
*     End of CBEG.
*
      END
      SUBROUTINE CHKXER(SRNAMT,INFOT,NOUT,LERR,OK)
*
*     Tests whether F06AAZ has detected an error when it should.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*     .. Scalar Arguments ..
      INTEGER           INFOT, NOUT
      LOGICAL           LERR, OK
      CHARACTER*13      SRNAMT
*     .. Executable Statements ..
      IF ( .NOT. LERR) THEN
         WRITE (NOUT,99999) INFOT, SRNAMT(1:6)
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
*     the test program for testing error exits from the Level 2 BLAS
*     routines.
*
*     F06AAZ  is an error handler for the Level 2 BLAS routines.
*
*     It is called by the Level 2 BLAS routines if an input parameter is
*     invalid.
*
*     Auxiliary routine for test program for Level 2 Blas.
*
*     -- Written on 10-August-1987.
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
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
     +       'D OF ',A6,' *******')
99997 FORMAT (' ******* F06AAZ WAS CALLED WITH INFO = ',I6,' *******')
      END
