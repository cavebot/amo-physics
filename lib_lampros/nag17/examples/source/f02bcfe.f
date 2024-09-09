*     F02BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IA, IB, IVI, IVR
      PARAMETER        (NMAX=4,MMAX=4,IA=NMAX,IB=NMAX+2,IVI=NMAX,
     +                 IVR=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RLB, RUB
      INTEGER          I, IFAIL, J, M, MM, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), RI(NMAX), RR(NMAX),
     +                 U(NMAX), V(NMAX), VI(IVI,MMAX), VR(IVR,MMAX)
      INTEGER          ICNT(NMAX), INTGER(NMAX)
      LOGICAL          C(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) 'N or M out of range: N = ', N, '  M = ', M
         STOP
      END IF
      READ (NIN,*) RLB, RUB, ((A(I,J),J=1,N),I=1,N)
      IFAIL = 1
*
      CALL F02BCF(A,IA,N,RLB,RUB,M,MM,RR,RI,VR,IVR,VI,IVI,INTGER,ICNT,C,
     +            B,IB,U,V,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02BCF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (' (',RR(I),',',RI(I),')',I=1,MM)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (' (',VR(I,J),',',VI(I,J),')',J=1,MM)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
