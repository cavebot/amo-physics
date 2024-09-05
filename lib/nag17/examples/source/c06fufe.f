*     C06FUF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, MNMAX
      PARAMETER        (MMAX=96,NMAX=96,MNMAX=MMAX*NMAX)
*     .. Local Scalars ..
      INTEGER          IFAIL, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION TRIGM(2*MMAX), TRIGN(2*NMAX), WORK(2*MNMAX),
     +                 X(MNMAX), Y(MNMAX)
*     .. External Subroutines ..
      EXTERNAL         C06FUF, C06GCF, READXY, WRITXY
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=40) M, N
      IF (M*N.GE.1 .AND. M*N.LE.MNMAX) THEN
         CALL READXY(NIN,X,Y,M,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data values'
         CALL WRITXY(NOUT,X,Y,M,N)
         IFAIL = 0
*
*        -- Compute transform
         CALL C06FUF(M,N,X,Y,'Initial',TRIGM,TRIGN,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Components of discrete Fourier transform'
         CALL WRITXY(NOUT,X,Y,M,N)
*
*        -- Compute inverse transform
         CALL C06GCF(Y,M*N,IFAIL)
         CALL C06FUF(M,N,X,Y,'Subsequent',TRIGM,TRIGN,WORK,IFAIL)
         CALL C06GCF(Y,M*N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Original sequence as restored by inverse transform'
         CALL WRITXY(NOUT,X,Y,M,N)
         GO TO 20
      ELSE
         WRITE (NOUT,*) ' ** Invalid value of M or N'
      END IF
   40 STOP
      END
*
      SUBROUTINE READXY(NIN,X,Y,N1,N2)
*     Read 2-dimensional complex data
*     .. Scalar Arguments ..
      INTEGER           N1, N2, NIN
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N1,N2), Y(N1,N2)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 20 I = 1, N1
         READ (NIN,*) (X(I,J),J=1,N2)
         READ (NIN,*) (Y(I,J),J=1,N2)
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE WRITXY(NOUT,X,Y,N1,N2)
*     Print 2-dimensional complex data
*     .. Scalar Arguments ..
      INTEGER           N1, N2, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N1,N2), Y(N1,N2)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 20 I = 1, N1
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Real ', (X(I,J),J=1,N2)
         WRITE (NOUT,99999) 'Imag ', (Y(I,J),J=1,N2)
   20 CONTINUE
      RETURN
*
99999 FORMAT (1X,A,7F10.3,/(6X,7F10.3))
      END
