*     M01ZCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=20,NMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION T
      INTEGER          I, IFAIL, II, J, K, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION RM(MMAX,NMAX)
      INTEGER          ICYCL(NMAX), IRANK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DJF, M01ZCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01ZCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, L
      IF (M.GE.1 .AND. M.LE.MMAX .AND. N.GE.1 .AND. N.LE.NMAX .AND.
     +    L.GE.1 .AND. L.LE.M) THEN
         DO 20 I = 1, M
            READ (NIN,*) (RM(I,J),J=1,N)
   20    CONTINUE
         IFAIL = 0
*
         CALL M01DJF(RM,MMAX,L,L,1,N,'Ascending',IRANK,IFAIL)
         CALL M01ZCF(IRANK,1,N,ICYCL,IFAIL)
*
         DO 60 K = 1, N
            I = ICYCL(K)
            IF (I.LT.0) THEN
               J = -I
            ELSE
*              Swap columns I and J
               DO 40 II = 1, M
                  T = RM(II,J)
                  RM(II,J) = RM(II,I)
                  RM(II,I) = T
   40          CONTINUE
            END IF
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Matrix sorted on row', L
         WRITE (NOUT,*)
         DO 80 I = 1, M
            WRITE (NOUT,99998) (RM(I,J),J=1,N)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,12F6.1)
      END
