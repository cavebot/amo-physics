*     F01BRF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NZMAX, LICN, LIRN
      PARAMETER        (NMAX=20,NZMAX=50,LICN=3*NZMAX,LIRN=3*NZMAX/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION U
      INTEGER          I, IFAIL, N, NZ
      LOGICAL          GROW, LBLOCK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LICN), W(NMAX)
      INTEGER          ICN(LICN), IDISP(10), IKEEP(NMAX,5), IRN(LIRN),
     +                 IW(NMAX,8)
      LOGICAL          ABORT(4)
*     .. External Subroutines ..
      EXTERNAL         F01BRF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BRF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NZ
      CALL X04ABF(1,NOUT)
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
         WRITE (NOUT,*)
         WRITE (NOUT,99999)
     +     'Number of non-zeros in decomposition         =', IDISP(2)
         WRITE (NOUT,99999)
     +     'Minimum size of array IRN                    =', IDISP(6)
         WRITE (NOUT,99999)
     +     'Minimum size of arrays A and ICN             =', IDISP(7)
         WRITE (NOUT,99999)
     +     'Number of compresses on IRN (IDISP(3))       =', IDISP(3)
         WRITE (NOUT,99999)
     +     'Number of compresses on A and ICN (IDISP(4)) =', IDISP(4)
         IF (GROW) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Value of W(1) =', W(1)
         END IF
         IF (LBLOCK) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Structural rank                =',
     +        IDISP(8)
            WRITE (NOUT,99999) 'Number of diagonal blocks      =',
     +        IDISP(9)
            WRITE (NOUT,99999) 'Size of largest diagonal block =',
     +        IDISP(10)
         END IF
      ELSE
         WRITE (NOUT,99999) 'N or NZ is out of range:  N = ', N,
     +     '  NZ = ', NZ
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F8.4)
      END
