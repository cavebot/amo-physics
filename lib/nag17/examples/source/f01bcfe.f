*     F01BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB
      PARAMETER        (NMAX=4,IA=NMAX,IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), D(NMAX), E(NMAX),
     +                 WK1(NMAX), WK2(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),B(I,J),J=1,N),I=1,N)
*
         CALL F01BCF(N,0.0D0,A,IA,B,IB,D,E,WK1,WK2)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Diagonal'
         WRITE (NOUT,99998) (D(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Sub-diagonal'
         WRITE (NOUT,99998) (E(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Arrays A and B'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (' (',A(I,J),',',B(I,J),')',J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,4(A,F7.3,A,F7.3,A))
99998 FORMAT (1X,8F9.4)
      END
