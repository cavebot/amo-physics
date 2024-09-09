*     E04MBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NCLIN, NROWA, N, NCTOTL, LIWORK, LWORK
      PARAMETER        (NCLIN=7,NROWA=NCLIN,N=7,NCTOTL=NCLIN+N,
     +                 LIWORK=2*N,LWORK=2*N*N+6*N+N+4*NCLIN+NROWA)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJLP
      INTEGER          I, IFAIL, ITMAX, J, MSGLVL
      LOGICAL          LINOBJ
*     .. Local Arrays ..
      DOUBLE PRECISION A(NROWA,N), BL(NCTOTL), BU(NCTOTL),
     +                 CLAMDA(NCTOTL), CVEC(N), WORK(LWORK), X(N)
      INTEGER          ISTATE(NCTOTL), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         E04MBF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04MBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
      ITMAX = 20
      MSGLVL = 1
      LINOBJ = .TRUE.
      READ (NIN,*) (CVEC(J),J=1,N)
      READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
      READ (NIN,*) (BL(J),J=1,NCTOTL)
      READ (NIN,*) (BU(J),J=1,NCTOTL)
      READ (NIN,*) (X(J),J=1,N)
      IFAIL = 1
*
      CALL E04MBF(ITMAX,MSGLVL,N,NCLIN,NCTOTL,NROWA,A,BL,BU,CVEC,LINOBJ,
     +            X,ISTATE,OBJLP,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      STOP
      END
