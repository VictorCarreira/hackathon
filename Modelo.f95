PROGRAM Modelo
!gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic Modelo.f95 -o Modelo
     
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !---------------------- Projeto Hackathon ----------------------!
     !--------------------- Modelo do Domo de Sal -------------------!
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!


        !################# Lista de Variáveis ####################!
        !i, j, k: contadores                                      !
        !Nx, Nz: número total de pontos em x e em z               !
        !Zr: conjunto imagem da função de reta interface 1        !
        !Zc: conjunto imagem da função circunferência do domo     !
        !vel: matriz de velocidades                               !
        !#########################################################!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$ DECLARAÇÃO DAS VARIÁVEIS GLOBAIS $$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
 INTEGER(KIND=SP):: i,j,k, Nx, Nz, zr,zc,ze
 REAL(KIND=DP)::x,y,z,a,b,c,e,ema,eme
 REAL(KIND=DP), PARAMETER::pi=3.141592653
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::den, vel


OPEN(10,FILE='velocidades.txt')

!Dados de entrada do modelo
 Nx=602
 Nz=302
 a=10
 b=5

!Elementos calculados da elipse
 ema=2*a !eixo maior
 eme=2*b !eixo menor
 c=SQRT(a**2-b**2) !centroide
 e=c/a !excentricidade 


WRITE(6,FMT=*)'Informações sobre o Domo de sal'  
WRITE(6,FMT=*)'centro',c  
WRITE(6,FMT=*)'excentricidade',e  

ALLOCATE(vel(Nz,Nx),den(Nz,Nx))

vel=0000.0
!Interface 1
DO j=1,Nx
  zr=NINT(-0.0001*j+90)
  DO i=zr,Nz
    vel(i,j)=2000.0
  END DO
END DO


!Interface 2
DO j=1,Nx
  IF(j.ge.245 .and. j.le.345) THEN
    zc=150+NINT(SQRT(2500.0-(j-295.0)**2))!eq. do círculo
  ELSE
    zc=150
  ENDIF
  DO i=zc,Nz
    vel(i,j)=2100.0 
  END DO
END DO

!Interface 3

DO j=1,Nx
  !IF(j.ge.40 .and. j.le.100) THEN
  !   ze=230+NINT(DSQRT(a**2.0*b**2-b**2*j**2/a**2.0)) !eq. reduzida da elipse
  IF(j.ge.45 .and. j.le.460) THEN
     ze=230+SQRT(j**3.0-j+1)! eq da curva elíptica
  ELSE
     ze=230
  END IF
  DO i=ze,Nz
     vel(i,j)=5000.0
  END DO 
END DO 
 
 
 
!Escreve o arquivo de dados no modo texto
DO i=1,Nz
  DO j=1,Nx
    WRITE(10,FMT=*)i,j,vel(i,j)
  END DO
END DO
CLOSE(10) 

!Formatos utilizados
21 FORMAT(15(F4.2,2x)) 



! Salva a matriz de velicidades em um arquivo binário
!OPEN(12,FILE='VELOCIDADES.bin', STATUS='UNKNOWN',ACCESS='DIRECT',&
!FORM='UNFORMATTED',RECL=4*Nx*Nz)
!WRITE(12,REC=1)((vel(i,j),i=1,Nz),j=1,Nx)! Isto é um laço interno
!CLOSE(12)



END PROGRAM Modelo
