SUBROUTINE PHSYN(GPP,RSP,TSP,QPARSNLF,QPARSHLF,VMXSNLF,VMXSHLF,LAISN,LAISH,TMP,SHM,PRS,WND,CO2PPM,OZN,WSTRS,aO3FLX,LAI,HGT,DVS,hDVS,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O,LTCH,ZKCA,ZKCB,ZKOA,ZKOB,GMMA,GMMB,TRES,CRP_NAME)  


  IMPLICIT NONE

!   [PHYSICAL CONSTANT]
  include "./param.inc"

!   [OUTPUT]
  REAL*8    GPP   ! Gross assimilation rate [mol m-2 s-1]
  REAL*8    RSP   ! Respiration rate [mol m-2 s-1]
  REAL*8    TSP   ! Transpiration rate []

!   [INPUT]
  REAL*8    QPARSNLF   !Absorbed PAR in unit leaf area[W/m**2]
  REAL*8    QPARSHLF   !Absorbed PAR in unit leaf area[W/m**2]
  REAL*8    VMXSNLF    !Vmax in unit leaf area
  REAL*8    VMXSHLF    !Vmax in unit leaf area
  REAL*8    LAISN
  REAL*8    LAISH


  REAL*8    TMP         !Air temperature[K]
  REAL*8    SHM         !Specific density of H2O in air space [kg/kg]
  REAL*8    PRS         !surface pressure [Pa]
  REAL*8    WND         !Wind speed at 2m [m s-1]
  REAL*8    OZN         !Ozone concetration [PPB]
  REAL*8    CO2PPM      ! CO2 concentration [PPM]



  REAL*8    LAI    ! Leaf area index  [m2 m-2]
  REAL*8    HGT

  REAL*8    DVS   !OZN
  REAL*8    hDVS  !OZN
  REAL*8    aO3FLX  ! OZN





  INTEGER TRES  !OZN

  REAL*8    WSTRS

  CHARACTER*200 CRP_NAME


!  [VEGETATION PARAMETER]
  REAL*8 RESPCP
  REAL*8 EFFCON    !
  REAL*8 ATHETA
  REAL*8 BTHETA
  REAL*8 MH2O
  REAL*8 BH2O

  REAL*8 LTCH

  REAL*8 ZKCA     ! Kc at 298K [Pa]
  REAL*8 ZKCB     ! Parameter for temperature dependence of Kc [-]
  REAL*8 ZKOA     ! Ko at 298K [Pa]
  REAL*8 ZKOB     ! Parameter for temperature dependence of Ko [-]
  REAL*8 GMMA     ! Parameter for temperature dependence of Gamma* [-]
  REAL*8 GMMB     ! Parameter for temperature dependence of Gamma* [-]





!  [INTERNAL VARIABLE]
  REAL*8 GPPSNLF
  REAL*8 GPPSHLF
  REAL*8 RSPSNLF
  REAL*8 RSPSHLF
  REAL*8 TSPSNLF
  REAL*8 TSPSHLF



  REAL*8 Tl     ! Leaf temperature [K]


  Tl=TMP        ! assuming that air temperature is same as leaf temperature

  GPP = 0.D0
  RSP = 0.D0
  TSP = 0.D0


  IF(LAI > 0.D0)THEN !YM

     ! Sunlit
     IF(LAISN > 0.D0)THEN

        IF(CRP_NAME .ne. "Maize")THEN
           CALL LEAF_PHSYN_C3(GPPSNLF,RSPSNLF,TSPSNLF,QPARSNLF,Tl,WND,SHM,PRS,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,VMXSNLF,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O) 
        ELSE
           CALL LEAF_PHSYN_C4(GPPSNLF,RSPSNLF,TSPSNLF,QPARSNLF,Tl,WND,SHM,PRS,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,VMXSNLF,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O) 



        END IF
     ELSE
        GPPSNLF = 0.D0
        RSPSNLF = 0.D0
        TSPSNLF = 0.D0
     END IF

     ! Shade
     IF(LAISH > 0.D0)THEN
        IF(CRP_NAME .ne. "Maize")THEN
           CALL LEAF_PHSYN_C3(GPPSHLF,RSPSHLF,TSPSHLF,QPARSHLF,Tl,WND,SHM,PRS,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,VMXSHLF,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O) 
        ELSE
           CALL LEAF_PHSYN_C4(GPPSHLF,RSPSHLF,TSPSHLF,QPARSHLF,Tl,WND,SHM,PRS,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,VMXSNLF,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O) 

        END IF

     ELSE
        GPPSHLF = 0.D0
        RSPSHLF = 0.D0
        TSPSHLF = 0.D0
     END IF

!      write(*,'(2F20.10)'),TSPSNLF*1000000.D0,TSPSHLF*1000000.D0

!      write(*,'(6F20.10)'),GPPSNLF*1000000.D0,RSPSNLF*1000000.D0,GPPSHLF*1000000.D0,RSPSHLF*1000000.D0,LAISN,LAISH

     GPP = GPPSNLF * LAISN + GPPSHLF * LAISH
     RSP = RSPSNLF * LAISN + RSPSHLF * LAISH
     TSP = TSPSNLF * LAISN + TSPSHLF * LAISH

!     print *,QPARSNLF,QPARSHLF,VMXSNLF*1000000.D0,VMXSHLF*1000000.D0
!     print *,GPPSNLF*1000000.D0,GPPSHLF*1000000.D0
!     print *,RSPSNLF*1000000.D0,RSPSHLF*1000000.D0
!     print *,(GPPSNLF-RSPSNLF)*1000000.D0,(GPPSHLF-RSPSHLF)*1000000.D0

!    print *,RSPSNLF*1000000.D0,RSPSHLF*1000000.D0
!     print *,"GPP,RSP=",GPP*1000000.D0,RSP*1000000.D0


  ELSE
     GPP = 0.D0
     RSP = 0.D0
     TSP = 0.D0

  END IF


END SUBROUTINE PHSYN




SUBROUTINE LEAF_PHSYN_C3(GPPLF_D,RSPLF_D,TSPLF_D,Qp,Tl,WND,QW,Ps,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,Vm25,RESPCP,EFFCON,ATHETA,BTHETA,G1d,G0) 

    

!
  IMPLICIT NONE

!   [PHYSICAL CONSTANT]
  include "./param.inc"

!   [OUTPUT]
  REAL*8    GPPLF_D   ! Gross assimilation rate [mol(CO2)/m**2/s]
  REAL*8    RSPLF_D   ! Respiration [mol(CO2/m**2/s)]
  REAL*8    TSPLF_D   ! Transpiration [kg/m**2/s]

  REAL*16 GPPLF
  REAL*16 RSPLF
  REAL*16 TSPLF


!   [INPUT]

  REAL*8    Tl      ! Leaf temperature[K]
  REAL*8    WND      ! Wind speed at 2m [m/s]
  REAL*8    QW      ! Specific density of H2O in air space [kg/kg]
  REAL*8    Qp  ! Leaf absorbed PAR [W/m-2]
  REAL*8    Ps    ! Surface pressure [Pa]
  REAL*8    CO2PPM  ! Atmospheric CO2 concentration [ppm]
  REAL*8    WSTRS   ! Water stree [-]

  REAL*8    HGT

  REAL*8    Vm25   ! Maximum Rubisco capacity [mol/m**2/s]


  REAL*8 OZN !OZN
  REAL*8 aO3FLX !OZN

  REAL*8 G1d
  REAL*8 G0


  REAL*8 DVS !OZN
  REAL*8 hDVS !OZN
  REAL*8 O3FLX  !OZN


!  [VEGETATION PARAMETER]
  REAL*8 RESPCP   ! Parameter for Respiration [-]
  REAL*8 EFFCON   ! Quantum efficiency [-]
  REAL*8 ATHETA   ! GPP transition parameter [-]
  REAL*8 BTHETA   ! GPP transition parameter [-]
  REAL*8 MH2O     ! Slope of BB model (for GSH2O)
  REAL*8 BH2O     ! Intercept of BB model [mol(H2O)/m**2/s]



!   [INTERNAL VARIABLE]
  REAL*16    PARM  ! Absorbed PAR [mol/m**2/s]

  REAL*16    ZKC   ! Kc: Michaelis-Mentern coefficient [Pa]
  REAL*16    ZKO   ! Ko: Michaelis-Mentern coefficient [Pa]
  REAL*16    GMMS  ! CO2 compensation point [Pa]
  REAL*16    RRKK  ! Kc * (1.D0 + O2 / Ko)

  REAL*16    Vm    ! Rubisoco capacity [mol(CO2)/m**2/s]
  REAL*16    Je    ! Electron tranport rate considering PAR [mol(CO2)/m**2/s]
  REAL*16    Jm    ! Electron tranport rate at 298K [mol(CO2)/m**2/s]



  REAL*16    GBH2O     ! Leaf boundary conductance for vapor [mol(H2O)/m**2/s]
  REAL*16    GBCO2     ! Leaf boundary conductance for CO2 [mol(CO2)/m**2/s]

  REAL*16    H2OI      ! H2O mol fraction inside
  REAL*16    H2OA      ! H2O mol fraction outside
  REAL*16    RH        ! Relative air humidity [-]

  REAL*16  UF
  

  REAL*16    OZNfact !OZN


  REAL*16    e1,e2,e3
  REAL*16    SQRTIN

  ! [FUNCTION]
  REAL*8    FQSAT

  REAL*16 VMXLFm

  REAL*16    Rb,GB


  REAL*16  R_JV




!   [INTERNAL]

  COMPLEX*32  AnC(6),AnE(6)      ! Photosynthesis rate               [micro mol(CO2) m-2 s-1]
  COMPLEX*32  CsC(6),CsE(6)      ! CO2 concentration at leaf surface [micro mol(CO2) mol-1]
  COMPLEX*32  CiC(6),CiE(6)      ! CO2 concentration in leaf         [micro mol(CO2) mol-1]
  COMPLEX*32  GscC(6),GscE(6)    ! Stomatal conductance for CO2      [mol(CO2) m-2 s-1]

  REAL*16 Cs
  REAL*16 GsCO2
  REAL*16 GsH2O

  REAL*16 H2OS

  REAL*16    aa(2)
  REAL*16    bb(2)
  REAL*16    dd(2)
  REAL*16    ee(2)

  REAL*16    G1cd    ! Slope of BWB model (for Gsc)
  REAL*16    G0c    ! Intercept of BWB model (for Gsc)[mol(CO2)/m**2(l)/s]

  REAL*16    ARFA
  REAL*16    BETA
  REAL*16    GANM
  REAL*16    ZETA
  REAL*16    GZAI
  REAL*16    EATA

  REAL*16 a1,a2,a3,a4
  REAL*16 b1,b2,b3

  REAL*16 A,B,C

  REAL*16 D1,D2
  REAL*16 p,q

  COMPLEX*32 u,v,uu,vv
  COMPLEX*32 w
  COMPLEX*32 w2
  COMPLEX*32 X1,X2,X3


  COMPLEX*32 An(6)
  COMPLEX*32 CO2S(6)
  COMPLEX*32 Gsc(6)
  COMPLEX*32 Ci(6)

  REAL*16 Ca

  INTEGER   I,J,II





  REAL*16    OMC       ! Rubisco-limited GPP [mol(CO2)/m**2/s]
  REAL*16    OME       ! Electron-transport limited GPP [mol(CO2)/m**2/s]
  REAL*16    OMS       ! Sucros-export limited GPP [mol(CO2)/m**2/s]
  REAL*16    OMP       ! GPP limited by OME and OMC [mol(CO2)/m**2/s]


  REAL*16    ASMN
  
  REAL*16 Z0m,Z0h
  REAL*16 D0


  !  [BIOPHYSICAL CONSTAT]
  REAL*16,PARAMETER::Eff=0.425Q0       ! Photo efficiency          [-] !!0.5*0.85=0.5*(1-0.15)=(50% of the energy is absorbed by each Photosystem)*(15%: wavelengths of light which are not used) ref: P316 in Lawlor(2001) and P248 in Bonan (215)
  REAL*16,PARAMETER::Dl=0.04Q0
  REAL*16,PARAMETER::Cv=0.01Q0         ! Turbulent transfer coefficient [m s-0.5]. 0.01 is set according to CLM5.0 (2020)



  PARM = 4.6Q0 *Qp  ! [micro mol m-2]

  ! Bernacchi et al. (2001) and (2003)
  ZKC=qexp(38.05Q0-79430.Q0/(Tl*RVAP*WMH2O))     ! [micro mol mol-1]
  ZKO=qexp(20.30Q0-36380.Q0/(Tl*RVAP*WMH2O))/1000.Q0     ! [ mol mol-1]
  GMMS=qexp(19.02Q0-37830.Q0/(Tl*RVAP*WMH2O))    ! [micro mol mol-1]
  Vm = (Vm25*1000000.Q0)  * qexp(26.35Q0-65330.Q0/(Tl*RVAP*WMH2O)) * WSTRS ! [micro mol mol-1]
  R_JV = 1.67D0 * (0.941D0 + 1.32D0 * 0.0001D0 * CO2PPM) / (0.941D0 + 1.32D0 * 0.0001D0 * 368.87D0)   ! downregulation
  Jm = 1.67Q0 * (Vm25*1000000.Q0) *qexp(17.70Q0-43900.Q0/(Tl*RVAP*WMH2O))  ! [micro mol mol-1]   P250 in Bonan (2015)
  RSPLF = 0.015Q0 * (Vm25*1000000.Q0) * qexp(18.72Q0-46390.Q0/(Tl*RVAP*WMH2O))  ![micro mol mol-1]

  RRKK  = ZKC * (1.Q0 + (PO2A / Ps) / ZKO)

  ! Calculation of aerodynamic resistance

  D0 = HGT * 2.Q0 / 3.Q0 ! Zero plane displacement FAO56  P20
  Z0m = HGT * 0.123Q0    ! Roughness length for momentum FAO56 P20
  Z0h = 0.1Q0 * Z0m      ! Roughness length for heat  FAO56 P20
  Rb = (qlog((2.Q0-D0)/Z0m) * qlog((2.Q0-D0)/Z0h))/(FKARM*FKARM*WND)  !aerodynamic resistance [s m-1]  !FAO56 P20

  GB = 1.Q0/Rb                     !Leaf boundary conductance  [m s-1]
  GBH2O = GB*Ps/(Tl*RVAP*WMH2O)   ! [mol(H2O) m-2(l) s-1]
  GBCO2 = GBH2O / 1.4Q0          ! [mol(CO2) m-2(l) s-1]


  e1 = 0.7Q0
  e2 = -(PARM*Eff+Jm)
  e3 = PARM*Eff*Jm

  SQRTIN = QSQRT(e2*e2-4.Q0*e1*e3)
  Je = (-e2 - SQRTIN)/(2.Q0*e1)


  Ca  = CO2PPM 


  H2OI  = FQSAT(Tl,Ps)/EPSV
  H2OA  = QW/EPSV
!  SPH2OA = 6.11D0 * 10.D0**((7.5D0 * (TC-273.15D0)) / ((TC-273.15D0) + 237.3D0)) * 100.D0

  RH = MIN(1.D0,QW/FQSAT(Tl,Ps))


  OZNfact = 1.D0 - aO3FLX * 0.04D0 !OZN




  ! Rubisco or RuBP-regeneration limited photosynthesis ! Baldocchi, 1994
  aa = (/Vm,Je/)
  bb = (/RRKK,8.Q0*GMMS/)
  dd = (/GMMS,GMMS/)
  ee = (/1.Q0,4.Q0/)
  
  
  G1cd = G1d / 1.6Q0   !!(1.Q0 + G1d/qsqrt(VPD))   ! original: 1.6(1+G1/sqrt(VPD))
  G0c = G0 / 1.6Q0     ! [mol(CO2) m-2 s-1]
  
  
  DO I = 1, 2    ! I=1: ASMNC   I=2: ASMNE
     
     
     ARFA = GBCO2*Ca
     BETA = G1cd*GBCO2*Rh - G0c
     GANM = aa(I)*dd(I) + bb(I)*RSPLF
     ZETA = aa(I) - ee(I)*RSPLF
     EATA = Ca*GBCO2*ZETA - GANM*GBCO2
     GZAI = ee(I)*Ca*GBCO2 + ZETA + bb(I)*GBCO2


     a1=ee(I)*BETA - ee(I)*GBCO2
     a2=ee(I)*G0c*ARFA - BETA*GZAI + ee(I)*GBCO2*ARFA + GBCO2*ZETA
     a3=-G0c*ARFA*GZAI + BETA*EATA - GBCO2*ARFA*ZETA
     a4=G0c*ARFA*EATA
     
     
     A = a2/a1
     B = a3/a1
     C = a4/a1
 
    
     p = B-(A**2.Q0)/3.Q0
     q = (2.Q0/27.Q0)*(A**3.Q0)-A*B/3.Q0+C
     
     D1 = (q**2.Q0)/4.Q0+(p**3.Q0)/27.Q0

     uu = -q*0.5Q0 + CQSQRT(QCMPLX(D1))
     vv = -q*0.5Q0 - CQSQRT(QCMPLX(D1))

     u = (uu)**(1.Q0/3.Q0)
     v = - p / (3.Q0 * u)

     w = (-1.Q0+qsqrt(3.Q0)*Qcmplx(0.Q0,1.Q0))*0.5Q0
     w2 =(-1.Q0-qsqrt(3.Q0)*Qcmplx(0.Q0,1.Q0))*0.5Q0

     X1 = u+v
     X2 = u*w+v*w2
     X3 = u*w2+v*w


     An(1) = X1 - A/3.Q0
     An(2) = X2 - A/3.Q0
     An(3) = X3 - A/3.Q0


     b1 =  ee(I)*G0c + ee(I)*GBCO2
     b2 =  -G0c*GZAI - GBCO2*ZETA
     b3 =  G0c*EATA
     
     D2 = b2**2.Q0 - 4.Q0 * b1 * b3
     
     An(4) = (-b2+CQSQRT(QCMPLX(D2)))/(2.Q0*b1)
     An(5) = (-b2-CQSQRT(QCMPLX(D2)))/(2.Q0*b1)
     An(6) = 0.Q0
     
     
     II=0


     DO J=1,5
        CO2S(J)=Ca - An(J) / GBCO2

        IF(J<=3)THEN
           IF(REAL(CO2S(J),kind=16)  /= 0.Q0)THEN
              Gsc(J)=An(J)*G1cd*Rh/CO2S(J) + G0c
           ELSE
              WRITE(*,'(A2)'),"!!"
              stop
           END IF
        ELSE
           Gsc(J)=G0c
        END IF

        Ci(J)=CO2S(J) - An(J)/Gsc(J)

!        print *,An(J)
!        print *,Gsc(J)
!        print *,Ci(J)


        IF(QIMAG(An(J)) < 0.0000000001Q0)THEN
           IF(J<=3)THEN
              IF(REAL(An(J),kind=16)>0.Q0  .and. REAL(Gsc(J),kind=16)>0.Q0 .and. REAL(Ci(J),kind=16)>0.Q0)THEN
                 IF(I==1)THEN
                    OMC=REAL(An(J),kind=16)+RSPLF
                 ELSE
                    OME=REAL(An(J),kind=16)+RSPLF
                 END IF
                 II=1
                 EXIT
              END IF
           ELSE
              IF(REAL(An(J),kind=16)<0.Q0  .and. REAL(Gsc(J),kind=16)>0.Q0 .and. REAL(Ci(J),kind=16)>0.Q0)THEN
                 IF(I==1)THEN
                    OMC=REAL(An(J),kind=16)+RSPLF
                 ELSE
                    OME=REAL(An(J),kind=16)+RSPLF
                 END IF
                 II=1
                 EXIT
              END IF
           END IF
        END IF

     
     END DO

     IF(II==0)THEN
        print *,"No solution!!"
        stop
     END IF
     

  END DO


  OMS   = Vm * 0.5Q0                                                      ! [micro mol(CO2)/m**2(l)/s]
    
  ! photosynthesis
  SQRTIN = (OME + OMC)**2.Q0 - 4.Q0 * ATHETA * OME * OMC
  OMP    = ((OME + OMC) - QSQRT(SQRTIN)) / (2.Q0 * ATHETA)
  SQRTIN = (OMP + OMS)**2.Q0 - 4.Q0 * BTHETA * OMP * OMS
  GPPLF  = ((OMS + OMP) - SQRT(SQRTIN)) / (2.Q0 * BTHETA)                 ! [micro mol(CO2)/m**2(l)/s]
      
  ASMN   = GPPLF - RSPLF                                                  ! [micro mol(CO2)/m**2(l)/s]
    
    
  ! Stomatal conductance
  Cs  = Ca - ASMN / GBCO2                                             ! [micro mol(CO2)/mol(air)]
  
  IF (ASMN > 0.Q0) THEN
     GsCO2 = G1cd*Rh * ASMN/Cs + G0c                               ! [mol/m**2(l)/s for CO2]
  ELSE
     GsCO2 = G0c                                                      ! [mol/m**2(l)/s for CO2]
  END IF
  
  GsH2O = GsCO2 * 1.6Q0                                                      ! [mol/m**2(l)/s for H2O]
  

  ! H2O mol fraction at leaf boundary
  H2OS = (GbH2O * H2OA + GsH2O * H2OI) / (GbH2O + GsH2O)                        ! [mol(H2O)/mol(air)]
  H2OS = MIN(H2OI, H2OS)                                                  ! [mol(H2O)/mol(air)]

  ! Transpiration
  TSPLF = GsH2O * (H2OI - H2OS) * WMH2O                                      ! [kg(H2O)/m**2(l)/s]


  GPPLF_D = DBLE(GPPLF/1000000.Q0)
  RSPLF_D = DBLE(RSPLF/1000000.Q0)
  TSPLF_D = DBLE(TSPLF)





!  print *,"TSP",TSPLF,Vm,Je

!  IF(DVS > hDVS*0.95D0 .AND. SNSH==1)THEN    !OZN
!     aO3FLX = aO3FLX + O3FLX  *DBLE(TRES) * 1000.D0  !OZN   O3FLX[mol/m2/s]   aO3FLX[mmol/m2]
!  END IF                      !OZN



END SUBROUTINE LEAF_PHSYN_C3


SUBROUTINE LEAF_PHSYN_C4(GPPLF,RSPLF,TSPLF,QPARLF,TC,WND,QW,PSFC,OZN,DVS,hDVS,aO3FLX,CO2PPM,WSTRS,HGT,VMXLF,RESPCP,EFFCON,ATHETA,BTHETA,MH2O,BH2O)

!
  IMPLICIT NONE

!   [PHYSICAL CONSTANT]
  include "./param.inc"


!   [OUTPUT]
  REAL*8    GPPLF   ! Gross assimilation rate [mol(CO2)/m**2/s]
  REAL*8    RSPLF   ! Respiration [mol(CO2/m**2/s)]
  REAL*8    TSPLF   ! Transpiration [kg/m**2/s]

!   [INPUT]

  REAL*8    TC      ! Leaf temperature[K]
  REAL*8    WND      ! Mean Wind speed in canopy [m/s]
  REAL*8    QW      ! Specific density of H2O in air space [kg/kg]
  REAL*8    QPARLF  ! Leaf absorbed PAR [W/m-2]
  REAL*8    PSFC    ! Surface pressure [Pa]
  REAL*8 OZN !OZN
  REAL*8 DVS !OZN
  REAL*8 hDVS !OZN
  REAL*8 aO3FLX !OZN

  REAL*8    CO2PPM  ! Atmospheric CO2 concentration [ppm]
  REAL*8    WSTRS   ! Water stree [-]
  REAL*8 HGT
  REAL*8    VMXLF   ! Maximum Rubisco capacity [mol/m**2/s]








  REAL*8 RESPCP   ! Parameter for Respiration [-]
  REAL*8 EFFCON   ! Quantum efficiency [-]
  REAL*8 ATHETA   ! GPP transition parameter [-]
  REAL*8 BTHETA   ! GPP transition parameter [-]
  REAL*8 MH2O     ! Slope of BB model (for GSH2O)
  REAL*8 BH2O     ! Intercept of BB model [mol(H2O)/m**2/s]



!   [INTERNAL VARIABLE]
  REAL*8    PARM  ! Absorbed PAR [mol/m**2/s]

  REAL*8    ZKC   ! Kc: Michaelis-Mentern coefficient [Pa]
  REAL*8    ZKO   ! Ko: Michaelis-Mentern coefficient [Pa]
  REAL*8    GMMS  ! CO2 compensation point [Pa]
  REAL*8    RRKK  ! Kc * (1.D0 + O2 / Ko)

  REAL*8    Vm    ! Rubisoco capacity [mol(CO2)/m**2/s]
  REAL*8    Je    ! Electron tranport rate considering PAR [mol(CO2)/m**2/s]
  REAL*8    Jm    ! Electron tranport rate at 298K [mol(CO2)/m**2/s]
  REAL*8    Jmx   ! Electron tranport rate [mol(CO2)/m**2/s]


  REAL*8    GBH2O     ! Leaf boundary conductance for vapor [mol(H2O)/m**2/s]
  REAL*8    GBCO2     ! Leaf boundary conductance for CO2 [mol(CO2)/m**2/s]
  REAL*8    CO2A      ! Atmospheric CO2 mol fraction [mol/mol]
  REAL*8    PCO2A     ! Atmospheric CO2 partial pressure [Pa]
  REAL*8    PCO2I     ! Leaf inner CO2 partial pressure [Pa]
  REAL*8    H2OI      ! H2O mol fraction inside
  REAL*8    H2OA      ! H2O mol fraction outside
  REAL*8    RH        ! Relative air humidity [-]
!  REAL*8    SPH2OA    ! Saturated vapor pressure

  REAL*8 O3FLX  !OZN
  REAL*8    OZNfact !OZN


  REAL*8    e1,e2,e3
  REAL*8    SQRTIN

  ! [FUNCTION]
  REAL*8    FQSAT

  REAL*8 VMXLFm


  REAL*8    Cv,Dl,Rb,GB

  REAL*8  R_JV

!!!
  REAL*8 TPEM, kp,G1CO2,G0CO2,fH,fL,OMC,OME,OMP,aa,bb,cc,OMS,ASMNSS,ASMN,CO2S,GSCO2,GSH2O,CO2I,H2OS
  REAL*8 I,J

  REAL*8 ASMNS(3)
  REAL*8 GSCO2S(3)
  REAL*8 CO2IS(3)

  REAL*8 Z0m,Z0h
  REAL*8 D0





  PARM = 4.6D0  * QPARLF * 0.000001D0 ! mol m-2 s-1
!  VMXLFm = VMXLF * 1000000.D0


  ! Calculation of aerodynamic resistance
  D0 = HGT * 2.D0 / 3.D0 ! Zero plane displacement FAO56  P20
  Z0m = HGT * 0.123D0    ! Roughness length for momentum FAO56 P20
  Z0h = 0.1D0 * Z0m      ! Roughness length for heat  FAO56 P20

  Rb = (log((2.D0-D0)/Z0m) * log((2.D0-D0)/Z0h))/(FKARM*FKARM*WND)  !aerodynamic resistance [s m-1]  !FAO56 P20

  GB = 1.Q0/Rb                     !Leaf boundary conductance  [m s-1]
  GBH2O = GB*PSFC/(TC*RVAP*WMH2O)   ! [mol(H2O) m-2(l) s-1]
  GBCO2 = GBH2O / 1.4D0          ! [mol(CO2) m-2(l) s-1]


!  print *, "GBCO2=",GBCO2

  CO2A  = CO2PPM  / 1000000.D0 ![mol/mol]
!  print *, CO2A,PARM,VMXLF

  H2OI  = FQSAT(TC,PSFC)/EPSV
  H2OA  = QW/EPSV

  RH = MIN(1.D0,QW/FQSAT(TC,PSFC))


  RSPLF = 0.015D0 * VMXLF * dexp(18.72D0-46390.D0/(TC*RVAP*WMH2O))  ![mol mol-1]


  TPEM   = 2.D0 ** ((TC-298.15D0)/10)
  kp = 0.7D0 * TPEM ! 0.7 ; Collatz 1992

!!! Masutomi-san

 G1CO2 = MH2O/1.6D0   ! BB eq. conversion from H2O to CO2 / !! MH2O = 4,BH2O=0.04 are for H2O
 G0CO2 = BH2O / 1.6D0


!  Rubisco-limited photosynthesis  !
!  OMC

  fH     = 1.D0 + exp( 0.3D0 * ( TC - 313.15D0 ) )
  fL     = 1.D0 + exp( 0.2D0 * ( 288.15D0 - TC ) )
  Vm     = WSTRS * VMXLF * ( TPEM / (fH * fL) )

  OMC    = Vm ! [mol m-2 s-1]

!  Light-limited photosynthesis  !
!  OME

  OME      = EFFCON * PARM ! [mol m-2 s-1]


!  co-limitation photosynthesis  !
!  OMP
  SQRTIN   = (OME + OMC)**2.D0 - 4.D0 * ATHETA * OME * OMC
  SQRTIN   = MAX(SQRTIN, 0.D0)
  OMP      = ((OME + OMC) - SQRT(SQRTIN)) / (2.D0 * ATHETA) ! [mol m-2 s-1]


!  print *, "OMC,OME,OMP=",OMC*1000000.D0,OME*1000000.D0,OMP*1000000.D0

  !! assume An > 0
  !! quadratic equations

  aa = (GBCO2**2.D0)*G1CO2*Rh - GBCO2*G0CO2 - kp*(G0CO2 - GBCO2*G1CO2*Rh + GBCO2)
  bb = CO2A*(GBCO2**2.D0)*G0CO2 - GBCO2*G0CO2*RSPLF + (GBCO2**2.D0)*G1CO2*Rh*RSPLF - kp*CO2A*((GBCO2**2.D0)*G1CO2*Rh - 2.D0*GBCO2*G0CO2 - GBCO2**2.D0)
  cc = CO2A * (GBCO2**2.D0)*G0CO2 * (RSPLF - kp*CO2A)

  !WRITE(*,'()'),

  SQRTIN    = bb ** 2.D0 - 4.D0 * aa * cc
  SQRTIN    = MAX(SQRTIN, 0.D0)
  ASMNS(1)     = ( - bb - SQRT(SQRTIN) ) / ( 2.D0 * aa )
  ASMNS(2)    = ( - bb + SQRT(SQRTIN) ) / ( 2.D0 * aa )
  
!  print *,ASMNS(1)*1000000.D0,ASMNS(2)*1000000.D0

  !! assume An < 0
  ASMNS(3) = (kp*CO2A - RSPLF)/(1.D0 + kp * (1.D0/GBCO2 + 1.D0/G0CO2))



  DO I = 1,2
    IF(ASMNS(I)>= 0.D0) THEN
      CO2S = CO2A - ASMNS(I)/GBCO2

      GSCO2S(I) = G0CO2 + G1CO2*Rh*ASMNS(I)/CO2S

      CO2IS(I) = (ASMNS(I)+RSPLF)/kp
    ELSE
      GSCO2S(I) = -1.D0
      CO2IS(I) = -1.D0
    END IF

  END DO


  IF(ASMNS(3) < 0.D0) THEN
    CO2S = CO2A - ASMNS(3)/GBCO2

    GSCO2S(3) = G0CO2 + G1CO2*Rh*ASMNS(3)/CO2S
    CO2IS(3) = (ASMNS(3)+RSPLF)/kp
  ELSE
    GSCO2S(3) = -1.D0
    CO2IS(3) = -1.D0

  END IF

  !WRITE(*,'(3F10.5)'),ASMNS(1)*1000000.D0,ASMNS(2)*1000000.D0,ASMNS(3)*1000000.D0
  !WRITE(*,'(3F10.5)'),GSCO2S(1),GSCO2S(2),GSCO2S(3)
  !WRITE(*,'(3F10.5)'),CO2IS(1),CO2IS(2),CO2IS(3)


  DO J = 1,3

  IF(J==1 .or. J==2)THEN
    !print*,"J<3",J
    ! J = 1,2
    IF(ASMNS(J)>=0.D0 .and. GSCO2S(J)>0.D0 .and. CO2IS(J)>0.D0) THEN
      ASMNSS = ASMNS(J)
    END IF

  ELSE IF(J==3) THEN
    ! J=3
    !print*,"J=3",J
    IF(ASMNS(J)<0.D0 .and. GSCO2S(J)>0.D0 .and. CO2IS(J)>0.D0) THEN
      ASMNSS = ASMNS(J)
    END IF

  END IF

  END DO

!  print *,ASMNSS*1000000.D0,RSPLF*1000000.D0
!  print *,BTHETA
   OMS = ASMNSS + RSPLF ! [mol m-2 s-1]


!  GPPLF  !
  SQRTIN = (OMP + OMS)**2.D0 - 4.D0 * BTHETA * OMP * OMS
  SQRTIN = MAX(SQRTIN, 0.D0)
  GPPLF  = ((OMS + OMP) - SQRT(SQRTIN)) / (2.D0 * BTHETA)                 ! [ mol(CO2)/m**2(l)/s]
  GPPLF  = MAX(GPPLF,0.D0)  ! [mol m-2 s-1]

!  print *,"GPPLF=",GPPLF*1000000.D0
!  print *,"RSPLF=",RSPLF*1000000.D0

  !Marin!

  ASMN   = GPPLF - RSPLF  ![mol m-2 s-1]


!  Stomatal conductance
  CO2S  = CO2A - ASMN / GBCO2  ![mol/mol]                                           ! [ mol(CO2)/mol(air)]
!

  IF (ASMN > 0.D0) THEN
     GSCO2 = G1CO2*Rh * ASMN/CO2S + G0CO2                               ! [mol/m**2(l)/s for CO2]
  ELSE
     GSCO2 = G0CO2                                                      ! [mol/m**2(l)/s for CO2]
  END IF
  GSH2O = GSCO2 * 1.6D0
!!!

      ! Intercellular CO2 concentration
  CO2I  = (CO2A - (1.D0 / GBCO2 + 1.D0 / GSCO2) * ASMN)                   ! [mol(CO2)/mol(air)]
  PCO2I = CO2I * PSFC                                                     ! [Pa(CO2)]

  !WRITE(*,'(4F20.5)'), ASMNS*1000000.D0,ASMNS2*1000000.D0,CO2I,GSCO2
  !WRITE(*,'(2F10.5)'), GSH2O,ASMN*RH/CO2S
  !WRITE(*,'(3F10.5)'), ASMNS2*1000000.D0,CO2I,GSCO2
  !WRITE(*,'(9F30.5)'), DVS,TC-273.15,PARM*1000000.D0,OMC*1000000.D0,OME*1000000.D0,OMS*1000000.D0,GPPLF*1000000.D0,PCO2I,ASMN*1000000.D0
  !print *, Tc-273.15


  ! H2O mol fraction at leaf boundary
  H2OS = (GBH2O * H2OA + GSH2O * H2OI) / (GBH2O + GSH2O)                        ! [mol(H2O)/mol(air)]
  H2OS = MIN(H2OI, H2OS)                                                  ! [mol(H2O)/mol(air)]

  ! Transpiration
  TSPLF = GSH2O * (H2OI - H2OS) * WMH2O                                      ! [kg(H2O)/m**2(l)/s]









END SUBROUTINE LEAF_PHSYN_C4






