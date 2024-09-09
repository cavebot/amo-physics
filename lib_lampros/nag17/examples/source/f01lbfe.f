*     F01LBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, M1MAX, M2MAX, IL, IA
      PARAMETER        (NMAX=7,M1MAX=6,M2MAX=6,IL=M1MAX,
     +                 IA=M1MAX+M2MAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IV, J, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AL(IL,NMAX)
      INTEGER          IN(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01LBF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01LBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1, M2
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M1.GE.0 .AND. M1.LE.M1MAX .AND.
     +    M2.GE.0 .AND. M2.LE.M2MAX) THEN
         READ (NIN,*) ((A(J,I),J=1,MIN(N,M1+M2+1)),I=1,N)
         IFAIL = 1
*
         CALL F01LBF(N,M1,M2,A,IA,AL,IL,IN,IV,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F01LBF. IFAIL =', IFAIL
            IF (IFAIL.GT.1) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99999) ' IV = ', IV
            END IF
         ELSE
            WRITE (NOUT,*)
     +    'Elements of upper triangular matrix  U  as stored in array A'
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        ' 1st row gives the values 1.0/U(I,I) for I=1(1)N'
            WRITE (NOUT,*)
            DO 20 I = 1, MIN(N,M1+M2+1)
               WRITE (NOUT,99998) (A(I,J),J=1,N-I+1)
   20       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +   'Elements of lower triangular matrix  L  as stored in array AL'
            WRITE (NOUT,*)
            DO 40 I = 1, M1
               WRITE (NOUT,99998) (AL(I,J),J=1,N-I)
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Vector of interchanges'
            WRITE (NOUT,*)
            WRITE (NOUT,99997) (IN(I),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,99999) 'N or M1 or M2 is out of range: N = ', N,
     +     '  M1 = ', M1, '  M2 = ', M2
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5,A,I5)
99998 FORMAT (1X,6F10.4)
99997 FORMAT (1X,6I10)
      END
