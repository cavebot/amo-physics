*     F01BSF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NZMAX, LICN, LIRN
      PARAMETER        (NMAX=20,NZMAX=50,LICN=3*NZMAX,LIRN=3*NZMAX/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ETA, RPMIN, U
      INTEGER          I, IFAIL, N, NZ
      LOGICAL          GROW, LBLOCK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LICN), W(NMAX)
      INTEGER          ICN(LICN), IDISP(10), IKEEP(NMAX,5), IRN(LIRN),
     +                 IVECT(NZMAX), IW(NMAX,8), JVECT(NZMAX)
      LOGICAL          ABORT(4)
*     .. External Subroutines ..
      EXTERNAL         F01BRF, F01BSF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BSF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NZ
      CALL X04ABF(1,NOUT)
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX .AND. NZ.GT.0 .AND. NZ.LE.NZMAX) THEN
         READ (NIN,*) (A(I),IRN(I),ICN(I),I=1,NZ)
         U = 0.1D0
         LBLOCK = .TRUE.
         GROW = .TRUE.
         ABORT(1) = .TRUE.
         ABORT(2) = .TRUE.
         ABORT(3) = .FALSE.
         ABORT(4) = .TRUE.
         IFAIL = 110
*
         CALL F01BRF(N,NZ,A,LICN,IRN,LIRN,ICN,U,IKEEP,IW,W,LBLOCK,GROW,
     +               ABORT,IDISP,IFAIL)
*
         IF (GROW) THEN
            WRITE (NOUT,*) 'On exit from F01BRF'
            WRITE (NOUT,99998) 'Value of W(1)  = ', W(1)
         END IF
         READ (NIN,*) (A(I),IVECT(I),JVECT(I),I=1,NZ)
         ETA = 0.1D0
         IFAIL = 110
*
         CALL F01BSF(N,NZ,A,LICN,IVECT,JVECT,ICN,IKEEP,IW,W,GROW,ETA,
     +               RPMIN,ABORT(4),IDISP,IFAIL)
*
         IF (GROW) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'On exit from F01BSF'
            WRITE (NOUT,99998) 'Value of W(1)  = ', W(1)
         END IF
         IF (ETA.LT.1.0D0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Value of RPMIN = ', RPMIN
         END IF
      ELSE
         WRITE (NOUT,*) 'N or NZ is out of range.'
         WRITE (NOUT,99999) 'N = ', N, '  NZ = ', NZ
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F7.4)
      END
