*     F04AXF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NZMAX, LICN, LIRN
      PARAMETER        (NMAX=20,NZMAX=50,LICN=3*NZMAX,LIRN=3*NZMAX/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RESID, U
      INTEGER          I, IFAIL, MTYPE, N, NZ
      LOGICAL          GROW, LBLOCK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LICN), RHS(NMAX), W(NMAX)
      INTEGER          ICN(LICN), IDISP(10), IKEEP(NMAX,5), IRN(LIRN),
     +                 IW(NMAX,8)
      LOGICAL          ABORT(4)
*     .. External Subroutines ..
      EXTERNAL         F01BRF, F04AXF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NZ
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
*        Decomposition of sparse matrix
         CALL F01BRF(N,NZ,A,LICN,IRN,LIRN,ICN,U,IKEEP,IW,W,LBLOCK,GROW,
     +               ABORT,IDISP,IFAIL)
*
         IF (GROW) THEN
            WRITE (NOUT,*) 'On exit from F01BRF'
            WRITE (NOUT,99998) 'Value of W(1) = ', W(1)
         END IF
         READ (NIN,*) (RHS(I),I=1,N)
         MTYPE = 1
*
*        Approximate solution of sparse linear equations
         CALL F04AXF(N,A,LICN,ICN,IKEEP,RHS,W,MTYPE,IDISP,RESID)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'On exit from F04AXF'
         WRITE (NOUT,*) ' Solution'
         WRITE (NOUT,99997) (RHS(I),I=1,N)
      ELSE
         WRITE (NOUT,99999) 'N or NZ is out of range: N = ', N,
     +     '  NZ = ', NZ
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F9.4)
99997 FORMAT (1X,F9.4)
      END
