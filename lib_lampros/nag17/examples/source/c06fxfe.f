*     C06FXF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N1MAX, N2MAX, N3MAX, NMAX
      PARAMETER        (N1MAX=16,N2MAX=16,N3MAX=16,
     +                 NMAX=N1MAX*N2MAX*N3MAX)
*     .. Local Scalars ..
      INTEGER          IFAIL, N, N1, N2, N3
*     .. Local Arrays ..
      DOUBLE PRECISION TRIGN1(2*N1MAX), TRIGN2(2*N2MAX),
     +                 TRIGN3(2*N3MAX), WORK(2*NMAX), X(NMAX), Y(NMAX)
*     .. External Subroutines ..
      EXTERNAL         C06FXF, C06GCF, READXY, WRITXY
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06FXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=40) N1, N2, N3
      N = N1*N2*N3
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         CALL READXY(NIN,X,Y,N1,N2,N3)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Original data values'
         CALL WRITXY(NOUT,X,Y,N1,N2,N3)
         IFAIL = 0
*
*        -- Compute transform
         CALL C06FXF(N1,N2,N3,X,Y,'Initial',TRIGN1,TRIGN2,TRIGN3,WORK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Components of discrete Fourier transform'
         CALL WRITXY(NOUT,X,Y,N1,N2,N3)
*
*        -- Compute inverse transform
         CALL C06GCF(Y,N,IFAIL)
         CALL C06FXF(N1,N2,N3,X,Y,'Subsequent',TRIGN1,TRIGN2,TRIGN3,
     +               WORK,IFAIL)
         CALL C06GCF(Y,N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Original sequence as restored by inverse transform'
         CALL WRITXY(NOUT,X,Y,N1,N2,N3)
         GO TO 20
      ELSE
         WRITE (NOUT,*) ' ** Invalid value of n1, n2 or n3'
      END IF
   40 STOP
      END
*
      SUBROUTINE READXY(NIN,X,Y,N1,N2,N3)
*     Read 3-dimensional complex data
*     .. Scalar Arguments ..
      INTEGER           N1, N2, N3, NIN
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N1,N2,N3), Y(N1,N2,N3)
*     .. Local Scalars ..
      INTEGER           I, J, K
*     .. Executable Statements ..
      DO 40 I = 1, N1
         DO 20 J = 1, N2
            READ (NIN,*) (X(I,J,K),K=1,N3)
            READ (NIN,*) (Y(I,J,K),K=1,N3)
   20    CONTINUE
   40 CONTINUE
      RETURN
      END
*
      SUBROUTINE WRITXY(NOUT,X,Y,N1,N2,N3)
*     Print 3-dimensional complex data
*     .. Scalar Arguments ..
      INTEGER           N1, N2, N3, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N1,N2,N3), Y(N1,N2,N3)
*     .. Local Scalars ..
      INTEGER           I, J, K
*     .. Executable Statements ..
      DO 40 I = 1, N1
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'z(i,j,k) for i =', I
         DO 20 J = 1, N2
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Real ', (X(I,J,K),K=1,N3)
            WRITE (NOUT,99999) 'Imag ', (Y(I,J,K),K=1,N3)
   20    CONTINUE
   40 CONTINUE
      RETURN
*
99999 FORMAT (1X,A,7F10.3,/(6X,7F10.3))
99998 FORMAT (1X,A,I6)
      END
