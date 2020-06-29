	! main program start
	PROGRAM CLMMicrobe
	! testbed for methane component in clm 
	! history
	! xiaofeng xu improved this section by primiarly incorpiating microbial process on methane production and consumption 
	!on June 20 2012 for LDRD project
	!declaration

	implicit none
	integer,parameter 		:: r8 = selected_real_kind(12) ! 8 byte real
	integer 					:: timestep, nr		! time step is on second  
	real(r8), parameter 		:: rgas = 8.3145_r8 ! J/mol.K
	real(r8), parameter 		:: rgasLatm = 0.0821_r8 ! L.atm/mol.K
	real(r8) 					:: f_sat = 0.95 ! volumetric soil water defining top of water table or where production is allowed

	! the input data: driving forces
	real(r8), dimension(:), allocatable :: forc_t
	real(r8), dimension(:), allocatable :: forc_pbot
	real(r8), dimension(:), allocatable :: t_soisno
	real(r8), dimension(:), allocatable :: h2osoi_vol
	! end of driving forces

	! the properties from input
	real(r8) 			:: forc_pco2			! initial state, change along manipulation
	real(r8) 			:: forc_pch4			! initial state, change along manipulation
	real(r8) 			:: forc_po2			! initial state, change along manipulation
	real(r8) 			:: forc_ph2			! initial state, change along manipulation
	real(r8)			:: atmco2
	real(r8)			:: atmch4
	real(r8)			:: atmo2
	real(r8)			:: atmh2
	real(r8)			:: soilvolume			! specific input for lab experiment volume of soil
	real(r8)			:: bottlevolume			! specific input for lab experiment volume of bottles
	real(r8) 			:: watsat			
	real(r8) 			:: pco2	
	real(r8) 			:: pch4
	real(r8) 			:: po2
	real(r8) 			:: ph2
   	  
	! soil properties
	real(r8)			:: sand
	real(r8)			:: clay
	real(r8)			:: silt
	real(r8)			:: psi
	real(r8)			:: psisat
	real(r8)			:: vwc
	real(r8) 			:: vwcsat
	real(r8)			:: organic
	real(r8)			:: smp_l
	real(r8)			:: origionalsoilph
	!end 
	real(r8)			:: initialac
	real(r8)			:: initialace
	real(r8)			:: initialorgacid
	real(r8)			:: initialacemeth
	real(r8)			:: initialh2meth
	real(r8)			:: initialmethanotrophs
	real(r8)			:: initialaommethanotrophs
	real(r8)			:: microberecov
	
	! for initile file
	character(len = 256) :: initialfile
	character(len = 256) :: soilparafile
	character(len = 256) :: microbeparfilename
	
	! for output file
	character(len = 256) :: outputfile

	!   variables changes over time
	real(r8), dimension(:), allocatable :: pH
	real(r8), dimension(:), allocatable :: hr
	! soil c and n pool sizes   
	real(r8), dimension(:), allocatable :: cwdc
	real(r8), dimension(:), allocatable :: cwdn
	real(r8), dimension(:), allocatable :: lit1c	
	real(r8), dimension(:), allocatable :: lit1n
	real(r8), dimension(:), allocatable :: lit2c
	real(r8), dimension(:), allocatable :: lit2n
	real(r8), dimension(:), allocatable :: lit3c
	real(r8), dimension(:), allocatable :: lit3n
	real(r8), dimension(:), allocatable :: som1c
	real(r8), dimension(:), allocatable :: som1n
	real(r8), dimension(:), allocatable :: som2c
	real(r8), dimension(:), allocatable :: som2n
	real(r8), dimension(:), allocatable :: som3c
	real(r8), dimension(:), allocatable :: som3n
	real(r8), dimension(:), allocatable :: som4c
	real(r8), dimension(:), allocatable :: som4n
	! key varibles
	real(r8), dimension(:), allocatable :: conc_ch4
	real(r8), dimension(:), allocatable :: conc_o2
	real(r8), dimension(:), allocatable :: conc_co2
	real(r8), dimension(:), allocatable :: conc_h2

	! microbe parameter
	real(r8), dimension(:), allocatable :: m_dAC
	real(r8), dimension(:), allocatable :: m_dAce
	real(r8), dimension(:), allocatable :: m_dOrgAcid
	real(r8), dimension(:), allocatable :: m_dCH4
	real(r8), dimension(:), allocatable :: m_dO2
	real(r8), dimension(:), allocatable :: m_dCO2
	real(r8), dimension(:), allocatable :: m_dH2
	real(r8), dimension(:), allocatable :: m_dAceMethanogens
	real(r8), dimension(:), allocatable :: m_dH2Methanogens
	real(r8), dimension(:), allocatable :: m_dMethanotrophs
	real(r8), dimension(:), allocatable :: m_dAOMMethanotrophs
	real(r8), dimension(:), allocatable :: soilpH
	real(r8), dimension(:), allocatable :: hco3
	real(r8), dimension(:), allocatable :: ch4_prod
	real(r8), dimension(:), allocatable :: co2_prod
	real(r8), dimension(:), allocatable :: h2_prod
	real(r8), dimension(:), allocatable :: ch4_oxid
	real(r8), dimension(:), allocatable :: o2_cons
	! microbe parameter

	integer 						:: i,n              ! loop indices
	integer 						:: ier              ! error code
  
	integer :: FlagpH
	integer :: FlagO2
	real(r8)	:: soilsample_scalar 
	real(r8)	:: hrtoac
		 
	common	/global/ &
		timestep, &
		forc_pco2, &
		forc_pch4, &
		forc_po2, &
		forc_ph2, &
		atmco2, &
		atmch4, &
		atmo2, &
		atmh2, &
		soilvolume, & 
		bottlevolume, & 
		FlagpH, &
		FlagO2, &
		soilsample_scalar
      
	common /soil/ &
		sand, &
		clay, &
		silt, &
		psi, &
		psisat, &
		vwc, &
		vwcsat, &
		smp_l, &
		origionalsoilph
 
	FlagpH = 0  ! switch for pH dynamic along with decomposition
	FlagO2 = 0  ! switch for O2 inhiibtation on decomposition
	soilsample_scalar = 1._r8   !
  
	write(*,*) "Pleae enter the time step for simulations , the unit is seconds"
	read(*,*) timestep
  
	write(*,*) "Pleae enter the number for total simulation steps"
	read(*,*) nr

	! allocate space for key input data
	allocate(forc_t(1:nr))
	allocate(forc_pbot(1:nr))
	allocate(t_soisno(1:nr))
	allocate(h2osoi_vol(1:nr))
	!allocate(watsat(1:nr))
	allocate(pH(1:nr))
	allocate(hr(1:nr))
   
	allocate(conc_ch4(1:nr))
	allocate(conc_o2(1:nr))
	allocate(conc_co2(1:nr))
	allocate(conc_h2(1:nr))
   
	allocate(cwdc(1:nr))
	allocate(cwdn(1:nr))
	allocate(lit1c(1:nr))
	allocate(lit1n(1:nr))
	allocate(lit2c(1:nr))
	allocate(lit2n(1:nr))
	allocate(lit3c(1:nr))
	allocate(lit3n(1:nr))
	allocate(som1c(1:nr))
	allocate(som1n(1:nr))
	allocate(som2c(1:nr))
	allocate(som2n(1:nr))
	allocate(som3c(1:nr))
	allocate(som3n(1:nr))
	allocate(som4c(1:nr))
	allocate(som4n(1:nr))
	
	! microbe parameter
	allocate(m_dAC(1:nr))
	allocate(m_dAce(1:nr))
	allocate(m_dOrgAcid(1:nr))	
	allocate(m_dCH4(1:nr))
	Allocate(m_dO2(1:nr))
	allocate(m_dCO2(1:nr))
	allocate(m_dH2(1:nr)) 
	allocate(m_dAceMethanogens(1:nr))
	allocate(m_dH2Methanogens(1:nr))
	allocate(m_dMethanotrophs(1:nr)) 
	allocate(m_dAOMMethanotrophs(1:nr)) 
	allocate(ch4_prod(1:nr)) 
	allocate(co2_prod(1:nr)) 
	allocate(h2_prod(1:nr)) 
	allocate(ch4_oxid(1:nr)) 	
	allocate(o2_cons(1:nr)) 
	allocate(soilpH(1:nr))
	allocate(hco3(1:nr))
	! microbe parameter
	! end of the allocation

	write(*,*) "Model inializing! "
	write(*,*) "please enter the name of file for initilizing model"
	read(*,*) initialfile
		
	open(unit = 11, file=initialfile)
		
	read (11,*,iostat=ier) organic
	read (11,*,iostat=ier) cwdc(1), cwdn(1)
	read (11,*,iostat=ier) lit1c(1), lit1n(1)
	read (11,*,iostat=ier) lit2c(1), lit2n(1)
	read (11,*,iostat=ier) lit3c(1), lit3n(1)
	read (11,*,iostat=ier) som1c(1), som1n(1)
	read (11,*,iostat=ier) som2c(1), som2n(1)
	read (11,*,iostat=ier) som3c(1), som3n(1)
	read (11,*,iostat=ier) som4c(1), som4n(1)
	!read (11,*,iostat=ier) initialac, initialace, initialacemeth, initialh2meth, initialmethanotrophs, initialaommethanotrophs
	read (11,*,iostat=ier) initialac, initialace, initialorgacid,initialacemeth, initialh2meth, initialmethanotrophs
	!print *, initialac,  " ", initialace, " ", initialorgacid, " ", initialacemeth, " ",  initialh2meth, " ", initialmethanotrophs
	read (11,*,iostat=ier) vwc, vwcsat
	read (11,*,iostat=ier) pco2, pch4, po2, ph2
	read (11,*,iostat=ier) origionalsoilph
	read (11,*,iostat=ier) hrtoac
	if (ier /= 0) then
	write(*,*)'model inializing failed !'
	else
	write(*,*) "model inialization finished !"
	end if
	close(11)
	
	write(*,*) "please enter the name of file for soil parametersl"
	read(*,*) soilparafile
	
	write(*,*) "please enter the name of file for microbe parametersl"
	read(*,*) microbeparfilename
		
	cwdc(1) = cwdc(1) * soilsample_scalar
	cwdn(1) = cwdn(1) * soilsample_scalar
	lit1c(1) = lit1c(1) * soilsample_scalar
	lit1n(1) = lit1n(1) * soilsample_scalar 
	lit2c(1) = lit2c(1) * soilsample_scalar 
	lit2n(1) = lit2n(1) * soilsample_scalar
	lit3c(1) = lit3c(1) * soilsample_scalar 
	lit3n(1) = lit3n(1) * soilsample_scalar
	som1c(1) = som1c(1) * soilsample_scalar
	som1n(1) = som1n(1) * soilsample_scalar
	som2c(1) = som2c(1) * soilsample_scalar
	som2n(1) = som2n(1) * soilsample_scalar
	som3c(1) = som3c(1) * soilsample_scalar
	som3n(1) = som3n(1) * soilsample_scalar
	som4c(1) = som4c(1) * soilsample_scalar
	som4n(1) = som4n(1) * soilsample_scalar

	forc_pco2 = pco2 * 1e-6 * 1.01e+5 !385.5_r8 * 1e-6  * 1.01e+5  ! ppm co2 concentration in air converted to mol / m3 dry air
	forc_pch4 = pch4 * 1e-6 * 1.01e+5 !1.8_r8 * 1e-6 * 1.01e+5   ! ppm ch4 concentration in air converted to mol / m3 dry air
	forc_po2 = po2 * 1e-6 * 1.01e+5  ! ppm o2 concentration in air converted to mol / m3 dry air
	forc_ph2 = ph2 * 1e-6 * 1.01e+5

	watsat = 0.5_r8	! fake value
	soilvolume = 0.00001 ! m3 volume of the soil samples this will not influnece the reaction, rather little impact on rate
	bottlevolume = 0.00005 ! m3 volume of the soil samples this will not influnece the reaction, rather little impact on rate

	sand = 30
	clay = 5
	silt = 65
	!vwc = 0.4
	!vwcsat = 0.5

	print *, 'read data start'
	call readdata(nr, forc_t, forc_pbot, t_soisno, h2osoi_vol)
	print *, 'read data end'
		
	write(*,*) "please enter the name of file for saving model output!"
	read(*,*) outputfile

	call soilpsi(sand, clay, silt, vwc, vwcsat, organic, psisat, psi, smp_l)

	!print *, psisat, psi

	atmco2 = forc_pco2 / rgas / forc_t(1)
	atmch4 = forc_pch4 / rgas / forc_t(1)
	atmo2 = forc_po2 / rgas / forc_t(1)
	atmh2 = forc_ph2 / rgas / forc_t(1)

	conc_co2(1)=bottlevolume * atmco2		! converting from ppm to mol
	conc_ch4(1)=bottlevolume * atmch4		! converting from ppm to mol
	conc_o2(1)=bottlevolume * atmo2		! converting from ppm to mol
	conc_h2(1)=bottlevolume * atmh2		! converting from ppm to mol
!	print *,"initial state of gas mole: CO2: ",  conc_co2(1), " CH4: ",conc_ch4(1), " O2: ",conc_o2(1)
!	tested the conversion is correct, it is about 0.00047 mol o2 in the 50ml bottle 0.00047->10.5 ml->21% in 50ml bottle
	m_dAC(1) = initialac					! g C 
	m_dAce (1)= initialace				! g C in soil
	m_dOrgAcid(1)=initialorgacid			! g C in soil
	m_dAceMethanogens(1) = initialacemeth	! g C in soil
	m_dH2Methanogens(1) = initialh2meth	! g C in soil
	m_dMethanotrophs(1) = initialmethanotrophs	! g C in soil
	m_dAOMMethanotrophs(1) = initialaommethanotrophs	! g C in soil
	soilpH(1) = origionalsoilph
	hco3(1) = 0.
	microberecov = 1.0

	do n = 1, nr
	!print *, n, " step simulation!  CO2: ", conc_co2(n), " CH4: ",conc_ch4(n), " O2: ",conc_o2(n)
!print *, "Before decomp: ", m_dAC(n) 
	!print *, psisat, " ", psi, " ", hr(n), " ",  t_soisno(n), " ", cwdc(n), " ", cwdn(n)
	!print *, lit1c(n), " ", lit1n(n)," ", lit2c(n), " ", lit2n(n)," ", lit3c(n), " ", lit3n(n)," ", som1c(n), " ", som1n(n)
	!print *, som2c(n), " ", som2n(n)," ", som3c(n), " ", som3n(n)," ", som4c(n), " ", som4n(n), " ", conc_o2(n)
    
call decomp(t_soisno(n), psisat, psi, hr(n), cwdc(n), cwdn(n), &
		lit1c(n), lit1n(n),lit2c(n), lit2n(n),lit3c(n), lit3n(n),som1c(n), som1n(n),&
		som2c(n), som2n(n),som3c(n), som3n(n),som4c(n), som4n(n), conc_o2(n), &
		m_dAC(n), hrtoac, soilparafile)
!print *, "after decomp: ", m_dAC(n)		
    	!print *, psisat, " ", psi, " ", hr(n), " ",  t_soisno(n)  , " ", cwdc(n), " ", cwdn(n)
	!print *, lit1c(n), " ", lit1n(n)," ", lit2c(n), " ", lit2n(n)," ", lit3c(n), " ", lit3n(n)," ", som1c(n), " ", som1n(n)
	!print *, som2c(n), " ", som2n(n)," ", som3c(n), " ", som3n(n)," ", som4c(n), " ", som4n(n), " ", conc_o2(n)
	
	! print *, "hr: ", hr(n)
	! Here is the update with methane production and oxidation by simulaitng microbial process
	if(conc_o2(n) < hr(n) / 12.) then
	hr(n) = conc_o2(n)
	end if
	
	conc_o2(n) = conc_o2(n) - hr(n) / 12.
	!print *, 	conc_o2(n)
	
	 !~ print *, "Before microbe: ", m_dAC(n), " ACE: ", m_dAce(n), " m_dOrgAcid: ", m_dOrgAcid(n), " CH4: ", &
		 !~ m_dCH4(n), " O2: ", m_dO2(n), " CO2: ", m_dCO2(n), " H2: ", m_dH2(n), " Acemehtanogen: ", &
		 !~ m_dAceMethanogens(n), " H2mehtanogen: ", m_dH2Methanogens(n), " Methanetroph: ", m_dMethanotrophs(n)

	m_dAC(n) = m_dAC(n) / soilvolume			! converting from mol to mol/m3 
	m_dAce(n) = m_dAce(n) / soilvolume		! 
	m_dOrgAcid(n) = m_dOrgAcid(n) / soilvolume
	m_dCH4(n)  = conc_ch4(n) / bottlevolume
	m_dO2(n) = conc_o2(n) / bottlevolume
	m_dCO2(n) = (conc_co2(n) + hr(n) / 12) / bottlevolume
	m_dH2(n) = conc_h2(n) / bottlevolume
	
	if(n < 0) then
	microberecov = (n / 480)**0.5
	else
	microberecov = 1.
	end if
call microbe(m_dAC(n), m_dAce(n), m_dOrgAcid(n), m_dCH4(n), m_dO2(n), m_dCO2(n), m_dH2(n), m_dAceMethanogens(n), &
	m_dH2Methanogens(n), m_dMethanotrophs(n), m_dAOMMethanotrophs(n), ch4_prod(n), co2_prod(n), h2_prod(n), &
	ch4_oxid(n), o2_cons(n), (t_soisno - 273.15), soilpH(n), hco3(n), microbeparfilename, microberecov)

	conc_ch4(n) = m_dCH4(n)  * bottlevolume
	conc_o2(n) = m_dO2(n) * bottlevolume
	conc_co2(n) = m_dCO2(n) * bottlevolume
	conc_h2(n) = m_dH2(n) * bottlevolume
	m_dAce(n) = m_dAce(n) * soilvolume
	m_dOrgAcid(n) = m_dOrgAcid(n) * soilvolume
	m_dAC(n) = m_dAC(n) * soilvolume
	
	o2_cons(n) = o2_cons(n) + hr(n) / 12.

!	print *, "After microbe: ", m_dAC(n), " ACE: ", m_dAce(n), " m_dOrgAcid: ", m_dOrgAcid(n), " CH4: ", &
!		m_dCH4(n), " O2: ", m_dO2(n), " CO2: ", m_dCO2(n), " H2: ", m_dH2(n), " Acemehtanogen: ", &
!		m_dAceMethanogens(n), " H2mehtanogen: ", m_dH2Methanogens(n), " Methanetroph: ", m_dMethanotrophs(n)
	
	! end of the simulation of microbe in producing and oxidizing methane

	if(n < nr) then
	conc_o2(n+1)=conc_o2(n)
	conc_co2(n+1)=conc_co2(n)
	conc_ch4(n+1)=conc_ch4(n)
	conc_h2(n+1)=conc_h2(n)
	
	cwdc(n+1)=cwdc(n)
	cwdn(n+1)=cwdn(n)
	lit1c(n+1)=lit1c(n)
	lit1n(n+1)=lit1n(n)
	lit2c(n+1)=lit2c(n)
	lit2n(n+1)=lit2n(n)
	lit3c(n+1)=lit3c(n)
	lit3n(n+1)=lit3n(n)
	som1c(n+1)=som1c(n)
	som1n(n+1)=som1n(n)
	som2c(n+1)=som2c(n)
	som2n(n+1)=som2n(n)
	som3c(n+1)=som3c(n)
	som3n(n+1)=som3n(n)
	som4c(n+1)=som4c(n)
	som4n(n+1)=som4n(n)
	
	! microbial parameter
	m_dAC(n+1) = m_dAC(n)
	m_dAce(n+1) = m_dAce(n)
	m_dOrgAcid(n+1) = m_dOrgAcid(n)

	m_dAceMethanogens(n+1) = m_dAceMethanogens(n)
	m_dH2Methanogens(n+1) = m_dH2Methanogens(n)
	m_dMethanotrophs(n+1) = m_dMethanotrophs(n)
	m_dAOMMethanotrophs(n+1) = m_dAOMMethanotrophs(n)
	
	soilpH(n+1) = soilpH(n)
	hco3(n+1) = hco3(n)
	! microbial parameter
	endif
	print *,n, " :CO2: ", conc_co2(n) * 1000000., " CH4: ",conc_ch4(n) * 1000000. !, " O2: ",conc_o2(n)	
	end do

	ch4_prod = ch4_prod * bottlevolume
	co2_prod = co2_prod * bottlevolume
	h2_prod = h2_prod * bottlevolume
	ch4_oxid = ch4_oxid * bottlevolume
	o2_cons = o2_cons * bottlevolume

call writeoutput(nr, hr, cwdc, cwdn, lit1c, lit1n, lit2c, lit2n, lit3c, lit3n, &
	som1c, som1n, som2c, som2n, som3c, som3n, som4c, som4n, &
	m_dAce, m_dOrgAcid, ch4_prod, co2_prod, h2_prod, &
	ch4_oxid, o2_cons, conc_ch4 * 1000000., &
	conc_o2, conc_co2 * 1000000., m_dH2, &
	m_dAceMethanogens, m_dH2Methanogens, m_dMethanotrophs, &
	m_dAOMMethanotrophs, soilpH, outputfile)

	stop
END PROGRAM CLMMicrobe
	! main program end


	! read data subroutine start
subroutine readdata(nr, forc_t, forc_pbot, t_soisno, h2osoi_vol)
	implicit none
	integer,parameter 		:: r8 = selected_real_kind(12) ! 8 byte real

	integer, intent(in)		:: nr
	real(r8), intent(out)	:: forc_t(1:nr)
	real(r8), intent(out)	:: forc_pbot(1:nr)
	real(r8), intent(out)	:: t_soisno(1:nr)
	real(r8), intent(out)	:: h2osoi_vol(1:nr)

	integer :: n, ier
	character(len=256) 		:: filename
	print *, "please enter the filename for input data:"
	read (*,*) filename
	open (100, FILE=filename)

	do n = 1, nr
	read(100,*,iostat=ier)  &
		forc_t(n), forc_pbot(n), t_soisno(n), h2osoi_vol(n)
	!print *, forc_t(n), " ",forc_pbot(n), " ",t_soisno(n), " ",h2osoi_vol(n)
	!1001 format(f7.2, f7.2, f7.2, f7.2, f7.2, f7.2, f7.2, f7.2)
	if (ier /= 0) then
	write(*,*) 'error in reading input data'
	end if
	end do
	close(100)
	print *, "reading forcing data finished"
	!read
end subroutine readdata
	! read data subroutine end


! hydrological properties start
subroutine soilpsi(sand, clay, silt, vwc, vwcsat, organic3d, psisat, psi, smp_l)
implicit none
	integer,parameter 	:: r8 = selected_real_kind(12) 		! 8 byte real
	real(r8), intent(in) 	:: sand ! percentage
	real(r8), intent(in) 	:: clay ! percentage
	real(r8), intent(in)	:: silt ! percentage
	real(r8), intent(in) 	:: vwc
	real(r8), intent(in) 	:: vwcsat
	real(r8), intent(in)	:: organic3d   ! read-in the organic matter content kg / m3
	real(r8), intent(out) 	:: psisat
	real(r8), intent(out) 	:: psi
	real(r8), intent(out)	:: smp_l ! soil matric potential (mm)
	real(r8)				:: bsw ! Clapp and Hornberger "b"
	real(r8) 				:: bsw2 ! Clapp and Hornberger "b" for CN module
	real(r8)				:: smp ! msoil matrix potential  it seems this is exactly same as smp_l
	real(r8)				:: sucsat  ! minimum soil suction
	real(r8)				:: s_node ! soil wetness
	real(r8)				:: smpmin ! restriction for min of soil potential
	real(r8)				:: om_frac ! organic matter fraction
	real(r8)				:: om_b !   Clapp Hornberger parameter for organic soil (letts, 2000) Line 188 of iniTimeConst.F90
	real(r8) 				:: organic_max  ! orgnaic matter hwere oil is assumed to act like peat
	real(r8)				:: om_sucsat ! saturated suction for organic matter (Lets, 2000)
 
	om_sucsat = 10.3_r8    ! saturated suction for organic matter (Lets, 2000)
	smpmin = -1._r8    ! restriction for min of soil potential line 750 of iniTimeConst.F90
	organic_max = 130._r8
	om_b = 2.7_r8
 
	om_frac = min(organic3d / organic_max, 1._r8)
	sucsat = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand) )
	bsw = (1._r8-om_frac)*(2.91_r8 + 0.159_r8*clay) + om_frac*om_b   
	sucsat = (1._r8-om_frac)*sucsat + om_sucsat*om_frac  
	s_node = min(1.0_r8, max(vwc / vwcsat, 0.01_r8))	    
	smp = max(smpmin, (-sucsat * (s_node ** (-bsw))))
	smp_l = smp
	bsw2 = -(3.1_r8 + 0.157_r8 * clay - 0.003_r8 * sand)
	psisat = -(exp((1.54_r8 - 0.0095_r8*sand + 0.0063_r8*(100.0_r8-sand-clay))*log(10.0_r8))*9.8e-5_r8)
	psi = psisat * ((vwc/vwcsat)**bsw2)
 
	!print *, psisat, " psisat and psi: ", psi
end subroutine soilpsi
	! hydrological properties end


! decomposition subroutine start
subroutine decomp(t_soisno, psisat, soilpsi, hrsum, cwdc, cwdn, litr1c, litr1n, litr2c, litr2n, &
	litr3c, litr3n, soil1c, soil1n, soil2c, soil2n, soil3c, soil3n, soil4c, soil4n, conc_o2, m_dAC, &
	hrtoac, soilparafile)
	implicit none
	integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
	real(r8), intent(in) :: t_soisno    	! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
	real(r8), intent(in) :: psisat      	! soil water potential at saturation for CN code (MPa)
	real(r8), intent(in) :: soilpsi		! soil potential

	real(r8),intent(inout) 	:: cwdc 	! (gC/m2) coarse woody debris C
	real(r8),intent(inout) 	:: litr1c  	! (kgC/m2) litter labile C
	real(r8),intent(inout) 	:: litr2c  	! (kgC/m2) litter cellulose C
	real(r8),intent(inout) 	:: litr3c  	! (kgC/m2) litter lignin C
	real(r8),intent(inout) 	:: soil1c 	! (kgC/m2) soil organic matter C (fast pool)
	real(r8),intent(inout) 	:: soil2c 	! (kgC/m2) soil organic matter C (medium pool)
	real(r8),intent(inout) 	:: soil3c 	! (kgC/m2) soil organic matter C (slow pool)
	real(r8),intent(inout) 	:: soil4c 	! (kgC/m2) soil organic matter C (slowest pool)
	real(r8),intent(inout) 	:: soil1n 	! (kgC/m2) soil organic matter C (fast pool)
	real(r8),intent(inout) 	:: soil2n 	! (kgC/m2) soil organic matter C (medium pool)
	real(r8),intent(inout) 	:: soil3n 	! (kgC/m2) soil organic matter C (slow pool)
	real(r8),intent(inout) 	:: soil4n 	! (kgC/m2) soil organic matter C (slowest pool)
	real(r8),intent(inout) 	:: cwdn        ! (gN/m2) coarse woody debris N
	real(r8),intent(inout) 	:: litr1n        	! (kgN/m2) litter labile N
	real(r8),intent(inout) 	:: litr2n        	! (kgN/m2) litter cellulose N
	real(r8),intent(inout) 	:: litr3n		! (kgN/m2) litter lignin N
	real(r8),intent(in)	:: conc_o2
	real(r8),intent(inout)	:: m_dAC
	real(r8), intent(in)	:: hrtoac
	
	character (len = 256),intent(in)	:: soilparafile
	! local pointers to implicit in/out scalars

	real(r8) :: o2limiting			! added by xiaofeng to control the decomposition posed by o2 limition
	!
	real(r8) :: fpi           			! fraction of potential immobilization (no units)
	real(r8) :: cwdc_to_litr2c
	real(r8) :: cwdc_to_litr3c
	real(r8) :: litr1_hr
	real(r8) :: litr1c_to_soil1c
	real(r8) :: litr2_hr
	real(r8) :: litr2c_to_soil2c
	real(r8) :: litr3_hr
	real(r8) :: litr3c_to_soil3c
	real(r8) :: soil1_hr
	real(r8) :: soil1c_to_soil2c
	real(r8) :: soil2_hr
	real(r8) :: soil2c_to_soil3c
	real(r8) :: soil3_hr
	real(r8) :: soil3c_to_soil4c
	real(r8) :: soil4_hr
	real(r8) :: cwdn_to_litr2n
	real(r8) :: cwdn_to_litr3n
	real(r8) :: potential_immob
	real(r8) :: litr1n_to_soil1n
	real(r8) :: sminn_to_soil1n_l1
	real(r8) :: litr2n_to_soil2n
	real(r8) :: sminn_to_soil2n_l2
	real(r8) :: litr3n_to_soil3n
	real(r8) :: sminn_to_soil3n_l3
	real(r8) :: soil1n_to_soil2n
	real(r8) :: sminn_to_soil2n_s1
	real(r8) :: soil2n_to_soil3n
	real(r8) :: sminn_to_soil3n_s2
	real(r8) :: soil3n_to_soil4n
	real(r8) :: sminn_to_soil4n_s3
	real(r8) :: soil4n_to_sminn
	real(r8) :: sminn_to_denit_l1s1
	real(r8) :: sminn_to_denit_l2s2
	real(r8) :: sminn_to_denit_l3s3
	real(r8) :: sminn_to_denit_s1s2
	real(r8) :: sminn_to_denit_s2s3
	real(r8) :: sminn_to_denit_s3s4
	real(r8) :: sminn_to_denit_s4
	real(r8) :: sminn_to_denit_excess
	real(r8) :: gross_nmin
	real(r8) :: net_nmin
	! For methane code
	!#ifdef LCH4
	real(r8) :: fphr      		! fraction of potential SOM + LITTER heterotrophic respiration	
	!   real(r8) :: finundated    !fractional inundated area in soil column
	!#endif


	! local pointers to implicit out scalars
	!
	! !OTHER LOCAL VARIABLES:

	real(r8):: dt           	!decomp timestep (seconds)
	real(r8):: dtd          	!decomp timestep (days)
	real(r8):: t_scalar     	!soil temperature scalar for decomp
	real(r8):: minpsi, maxpsi    !limits for soil water scalar for decomp
	real(r8):: psi                   !temporary soilpsi for water scalar
	real(r8):: w_scalar     	!soil water scalar for decomp
	real(r8):: rate_scalar  	!combined rate scalar for decomp
	real(r8):: cn_l1       	!C:N for litter 1
	real(r8):: cn_l2       	!C:N for litter 2
	real(r8):: cn_l3        	!C:N for litter 3
	real(r8):: cn_s1        	!C:N for SOM 1
	real(r8):: cn_s2        	!C:N for SOM 2
	real(r8):: cn_s3        	!C:N for SOM 3
	real(r8):: cn_s4        	!C:N for SOM 4
	real(r8):: rf_l1s1      	!respiration fraction litter 1 -> SOM 1
	real(r8):: rf_l2s2      	!respiration fraction litter 2 -> SOM 2
	real(r8):: rf_l3s3      	!respiration fraction litter 3 -> SOM 3
	real(r8):: rf_s1s2      	!respiration fraction SOM 1 -> SOM 2
	real(r8):: rf_s2s3      	!respiration fraction SOM 2 -> SOM 3
	real(r8):: rf_s3s4      	!respiration fraction SOM 3 -> SOM 4
	real(r8):: k_l1         	!decomposition rate constant litter 1
	real(r8):: k_l2         	!decomposition rate constant litter 2
	real(r8):: k_l3         	!decomposition rate constant litter 3
	real(r8):: k_s1         	!decomposition rate constant SOM 1
	real(r8):: k_s2         	!decomposition rate constant SOM 2
	real(r8):: k_s3         	!decomposition rate constant SOM 3
	real(r8):: k_s4         	!decomposition rate constant SOM 3
	real(r8):: k_frag       	!fragmentation rate constant CWD
	real(r8):: ck_l1        	!corrected decomposition rate constant litter 1
	real(r8):: ck_l2        	!corrected decomposition rate constant litter 2
	real(r8):: ck_l3        	!corrected decomposition rate constant litter 3
	real(r8):: ck_s1        	!corrected decomposition rate constant SOM 1
	real(r8):: ck_s2        	!corrected decomposition rate constant SOM 2
	real(r8):: ck_s3        	!corrected decomposition rate constant SOM 3
	real(r8):: ck_s4        	!corrected decomposition rate constant SOM 3
	real(r8):: ck_frag      	!corrected fragmentation rate constant CWD
	real(r8):: cwd_fcel     	!cellulose fraction of coarse woody debris
	real(r8):: cwd_flig     	!lignin fraction of coarse woody debris
	real(r8):: cwdc_loss    	!fragmentation rate for CWD carbon (gC/m2/s)
	real(r8):: cwdn_loss    	!fragmentation rate for CWD nitrogen (gN/m2/s)
	real(r8):: plitr1c_loss 	!potential C loss from litter 1
	real(r8):: plitr2c_loss 	!potential C loss from litter 2
	real(r8):: plitr3c_loss 	!potential C loss from litter 3
	real(r8):: psoil1c_loss 	!potential C loss from SOM 1
	real(r8):: psoil2c_loss 	!potential C loss from SOM 2
	real(r8):: psoil3c_loss 	!potential C loss from SOM 3
	real(r8):: psoil4c_loss 	!potential C loss from SOM 4
	real(r8):: pmnf_l1s1    	!potential mineral N flux, litter 1 -> SOM 1
	real(r8):: pmnf_l2s2    	!potential mineral N flux, litter 2 -> SOM 2
	real(r8):: pmnf_l3s3    	!potential mineral N flux, litter 3 -> SOM 3
	real(r8):: pmnf_s1s2    	!potential mineral N flux, SOM 1 -> SOM 2
	real(r8):: pmnf_s2s3    	!potential mineral N flux, SOM 2 -> SOM 3
	real(r8):: pmnf_s3s4    	!potential mineral N flux, SOM 3 -> SOM 4
	real(r8):: pmnf_s4      	!potential mineral N flux, SOM 4
	real(r8):: immob        	!potential N immobilization
	real(r8):: ratio        	!temporary variable
	real(r8):: dnp         	!denitrification proportion
	real(r8):: o2_scalar		! O2 inhibition on decomposition
	! For methane code
	real(r8):: phr       		!potential HR (gC/m2/s)
	real(r8), intent(out) :: hrsum     !sum of HR (gC/m2/s)
	!EOP
	!-----------------------------------------------------------------------
	integer	:: timestep
	real(r8)	:: forc_pco2
	real(r8)	:: forc_pch4
	real(r8)	:: forc_po2
	real(r8)	:: forc_ph2
	real(r8)	:: atmco2
	real(r8)	:: atmch4
	real(r8)	:: atmo2
	real(r8)	:: atmh2
	real(r8)	:: soilvolume
	real(r8)	:: bottlevolume
	integer	:: FlagpH
	Integer	:: FlagO2
	real(r8)	:: soilsample_scalar
	
	real(r8)	:: dr1
	real(r8)	:: dr2
	real(r8)	:: dr3
	real(r8)	:: dr4
	real(r8)	:: dr5
	real(r8)	:: dr6
	real(r8)	:: dr7
	real(r8)	:: dr8

	common	/global/ &
		timestep, &
		forc_pco2, &
		forc_pch4, &
		forc_po2, &
		forc_ph2, &
		atmco2, &
		atmch4, &
		atmo2, &
		atmh2, &
		soilvolume, &
		bottlevolume, &
		FlagpH, &
		FlagO2, &
		soilsample_scalar
	
	if(FlagO2 == 0) then
	o2_scalar = 1._r8	
	else
	o2_scalar = 2.0 * conc_o2 / (conc_o2 + atmo2 * bottlevolume)
	endif
	
	! set time steps
	dt = real(timestep, r8 )
	dtd = dt/3600.0_r8
	!print *, timestep, " decomposition time step: ",dtd
	! set soil organic matter compartment C:N ratios (from Biome-BGC v4.2.0)
	cn_s1 = 12.0_r8
	cn_s2 = 12.0_r8
	cn_s3 = 10.0_r8
	cn_s4 = 10.0_r8

	! set respiration fractions for fluxes between compartments
	! (from Biome-BGC v4.2.0)
	rf_l1s1 = 0.39_r8
	rf_l2s2 = 0.55_r8
	rf_l3s3 = 0.29_r8
	rf_s1s2 = 0.28_r8
	rf_s2s3 = 0.46_r8
	rf_s3s4 = 0.55

	! set the cellulose and lignin fractions for coarse woody debris
	cwd_fcel = 0.76_r8
	cwd_flig = 0.24_r8

	! set initial base rates for decomposition mass loss (1/day)
	! (from Biome-BGC v4.2.0, using three SOM pools)
	! Value inside log function is the discrete-time values for a
	! daily time step model, and the result of the log function is
	! the corresponding continuous-time decay rate (1/day), following
	! Olson, 1963.
	
	open(unit = 102, file=soilparafile)
	read (102,*) dr1
	read (102,*) dr2
	read (102,*) dr3
	read (102,*) dr4
	read (102,*) dr5
	read (102,*) dr6
	read (102,*) dr7
	read (102,*) dr8
	close(102)
	
	!print *, dr1, " ",  dr2,  " ", dr3, " ",  dr4, " ",  dr5,  " ", dr6,  " ", dr7, " ", dr8
	k_l1 = -log(1.0_r8-dr1)
	k_l2 = -log(1.0_r8-dr2)
	k_l3 = -log(1.0_r8-dr3)
	k_s1 = -log(1.0_r8-dr4)
	k_s2 = -log(1.0_r8-dr5)
	k_s3 = -log(1.0_r8-dr6)
	k_s4 = -log(1.0_r8-dr7)
	k_frag = -log(1.0_r8-dr8)
	
	! xiaofeng use the above code to replace the following, just move parameters into a file
	!k_l1 = -log(1.0_r8-0.7_r8)
	!k_l2 = -log(1.0_r8-0.07_r8)
	!k_l3 = -log(1.0_r8-0.014_r8)
	!k_s1 = -log(1.0_r8-0.07_r8)
	!k_s2 = -log(1.0_r8-0.014_r8)
	!k_s3 = -log(1.0_r8-0.0014_r8)
	!k_s4 = -log(1.0_r8-0.0001_r8)
	!k_frag = -log(1.0_r8-0.001_r8)
	
	! calculate the new discrete-time decay rate for model timestep
	k_l1 = 1.0_r8-exp(-k_l1*dtd)
	k_l2 = 1.0_r8-exp(-k_l2*dtd)
	k_l3 = 1.0_r8-exp(-k_l3*dtd)
	k_s1 = 1.0_r8-exp(-k_s1*dtd)
	k_s2 = 1.0_r8-exp(-k_s2*dtd)
	k_s3 = 1.0_r8-exp(-k_s3*dtd)
	k_s4 = 1.0_r8-exp(-k_s4*dtd)
	k_frag = 1.0_r8-exp(-k_frag*dtd)
	
	! calculate rate constant scalar for soil temperature
	! Replaced the Lloyd and Taylor function with a Q10 formula, with Q10 = 1.5
	! as part of the modifications made to improve the seasonal cycle of 
	! atmospheric CO2 concentration in global simulations. This does not impact
	! the base rates at 25 C, which are calibrated from microcosm studies.
	t_scalar = 0._r8
	t_scalar=t_scalar + (1.5**((t_soisno-(273.15_r8+25._r8))/10._r8))
	
	! calculate the rate constant scalar for soil water content.
	! Uses the log relationship with water potential given in
	! Andren, O., and K. Paustian, 1987. Barley straw decomposition in the field:
	! a comparison of models. Ecology, 68(5):1190-1200.
	! and supported by data in
	! Orchard, V.A., and F.J. Cook, 1983. Relationship between soil respiration
	! and soil moisture. Soil Biol. Biochem., 15(4):447-453.
	
	minpsi = -10.0_r8;
	w_scalar = 0._r8
	maxpsi = psisat
	psi = soilpsi
	psi = min(soilpsi,maxpsi)
	! decomp only if soilpsi is higher than minpsi
	
	!~ if (psi > minpsi) then
	!~ w_scalar = w_scalar + (log(minpsi/psi)/log(minpsi/maxpsi))
	!~ end if
	!xiaofeng replaced above codes with following
	if (psi > minpsi) then
	w_scalar = w_scalar + log(psi/minpsi)*log(psi/maxpsi)/(log(psi/minpsi)*log(psi/maxpsi) - &
		log(psi/(maxpsi*2/3+minpsi/3.))*log(psi/(maxpsi*2/3+minpsi/3.)))
	end if
	w_scalar = w_scalar! ** 0.5
	w_scalar = max(0.1, w_scalar)
!	w_scalar = min(0.8, w_scalar)
	
	!print *,"w_scalar: ",  w_scalar
	! set initial values for potential C and N fluxes
	plitr1c_loss = 0._r8
	plitr2c_loss = 0._r8
	plitr3c_loss = 0._r8
	psoil1c_loss = 0._r8
	psoil2c_loss = 0._r8
	psoil3c_loss = 0._r8
	psoil4c_loss = 0._r8
	pmnf_l1s1 = 0._r8
	pmnf_l2s2 = 0._r8
	pmnf_l3s3 = 0._r8
	pmnf_s1s2 = 0._r8
	pmnf_s2s3 = 0._r8
	pmnf_s3s4 = 0._r8
	pmnf_s4 = 0._r8
	! column loop to calculate potential decomp rates and total immobilization
	! demand.

	! calculate litter compartment C:N ratios
	if (litr1n > 0._r8) then
	cn_l1 = litr1c/litr1n
	else
	cn_l1 = 0.
	end if
	
	if (litr2n > 0._r8) then
	cn_l2 = litr2c/litr2n
	else
	cn_l2 = 0.
	end if
	
	if (cn_l3 > 0._r8) then
	cn_l3 = litr3c/litr3n
	else
	cn_l3 = 0.
	end if
	
	! calculate the final rate scalar as the product of temperature and water
	! rate scalars, and correct the base decomp rates

	rate_scalar = t_scalar * w_scalar * o2_scalar
	ck_l1 = k_l1 * rate_scalar
	ck_l2 = k_l2 * rate_scalar
	ck_l3 = k_l3 * rate_scalar
	ck_s1 = k_s1 * rate_scalar
	ck_s2 = k_s2 * rate_scalar
	ck_s3 = k_s3 * rate_scalar
	ck_s4 = k_s4 * rate_scalar
	ck_frag = k_frag * rate_scalar

	! calculate the non-nitrogen-limited fluxes
	! these fluxes include the  "/ dt" term to put them on a
	! per second basis, since the rate constants have been
	! calculated on a per timestep basis.

	! CWD fragmentation -> litter pools
	cwdc_loss = cwdc * ck_frag / dt
	cwdc_to_litr2c = cwdc_loss * cwd_fcel
	cwdc_to_litr3c = cwdc_loss * cwd_flig
	cwdn_loss = cwdn * ck_frag / dt
	cwdn_to_litr2n = cwdn_loss * cwd_fcel
	cwdn_to_litr3n = cwdn_loss * cwd_flig

	! litter 1 -> SOM 1
	if (litr1c > 0._r8) then
        plitr1c_loss = litr1c * ck_l1 / dt
        ratio = 0._r8
        if (litr1n > 0._r8) ratio = cn_s1/cn_l1
        pmnf_l1s1 = (plitr1c_loss * (1.0_r8 - rf_l1s1 - ratio))/cn_s1
	end if

	! litter 2 -> SOM 2
	if (litr2c > 0._r8) then
        plitr2c_loss = litr2c * ck_l2 / dt
        ratio = 0._r8
        if (litr2n > 0._r8) ratio = cn_s2/cn_l2
        pmnf_l2s2 = (plitr2c_loss * (1.0_r8 - rf_l2s2 - ratio))/cn_s2
	end if

	! litter 3 -> SOM 3
	if (litr3c > 0._r8) then
        plitr3c_loss = litr3c * ck_l3 / dt
        ratio = 0._r8
        if (litr3n > 0._r8) ratio = cn_s3/cn_l3
        pmnf_l3s3 = (plitr3c_loss * (1.0_r8 - rf_l3s3 - ratio))/cn_s3
	end if

	! SOM 1 -> SOM 2
	if (soil1c > 0._r8) then
        psoil1c_loss = soil1c * ck_s1 / dt
        pmnf_s1s2 = (psoil1c_loss * (1.0_r8 - rf_s1s2 - (cn_s2/cn_s1)))/cn_s2
	end if

	! SOM 2 -> SOM 3
	if (soil2c > 0._r8) then
        psoil2c_loss = soil2c * ck_s2 / dt
        pmnf_s2s3 = (psoil2c_loss * (1.0_r8 - rf_s2s3 - (cn_s3/cn_s2)))/cn_s3
	end if

	! SOM 3 -> SOM 4
	if (soil3c > 0._r8) then
        psoil3c_loss = soil3c * ck_s3 / dt
        pmnf_s3s4 = (psoil3c_loss * (1.0_r8 - rf_s3s4 - (cn_s4/cn_s3)))/cn_s4
	end if

	! Loss from SOM 4 is entirely respiration (no downstream pool)
	if (soil4c > 0._r8) then
        psoil4c_loss = soil4c * ck_s4 / dt
        pmnf_s4 = -psoil4c_loss/cn_s4
	end if

	! Sum up all the potential immobilization fluxes (positive pmnf flux)
	! and all the mineralization fluxes (negative pmnf flux)

	immob = 0._r8
	! litter 1 -> SOM 1
	if (pmnf_l1s1 > 0._r8) then
        immob = immob + pmnf_l1s1
	else
        gross_nmin = gross_nmin - pmnf_l1s1
	end if

	! litter 2 -> SOM 2
	if (pmnf_l2s2 > 0._r8) then
        immob = immob + pmnf_l2s2
	else
        gross_nmin = gross_nmin - pmnf_l2s2
	end if

	! litter 3 -> SOM 3
	if (pmnf_l3s3 > 0._r8) then
        immob = immob + pmnf_l3s3
	else
        gross_nmin = gross_nmin - pmnf_l3s3
	end if

	! SOM 1 -> SOM 2
	if (pmnf_s1s2 > 0._r8) then
        immob = immob + pmnf_s1s2
	else
        gross_nmin = gross_nmin - pmnf_s1s2
	end if

	! SOM 2 -> SOM 3
	if (pmnf_s2s3 > 0._r8) then
        immob = immob + pmnf_s2s3
	else
        gross_nmin = gross_nmin - pmnf_s2s3
	end if

	! SOM 3 -> SOM 4
	if (pmnf_s3s4 > 0._r8) then
        immob = immob + pmnf_s3s4
	else
        gross_nmin = gross_nmin - pmnf_s3s4
	end if

	! SOM 4
	gross_nmin = gross_nmin - pmnf_s4
	potential_immob = immob

	! Add up potential hr for methane calculations
	phr = rf_s1s2*psoil1c_loss + rf_s2s3*psoil2c_loss + rf_s3s4*psoil3c_loss &
		+ psoil4c_loss &
		+ rf_l1s1*plitr1c_loss + rf_l2s2*plitr2c_loss + rf_l3s3*plitr3c_loss

	dnp = 0.01_r8

	! upon return from CNAllocation, the fraction of potential immobilization
	! has been set (cps%fpi). now finish the decomp calculations.
	! Only the immobilization steps are limited by fpi (pmnf > 0)
	! Also calculate denitrification losses as a simple proportion
	! of mineralization flux.

	! litter 1 fluxes (labile pool)
	if (litr1c > 0._r8) then
        if (pmnf_l1s1 > 0._r8) then
        plitr1c_loss = plitr1c_loss * fpi
        pmnf_l1s1 = pmnf_l1s1 * fpi
        sminn_to_denit_l1s1 = 0._r8
        else
	sminn_to_denit_l1s1 = -dnp * pmnf_l1s1
        end if
        litr1_hr = rf_l1s1 * plitr1c_loss
         litr1c_to_soil1c = (1._r8 - rf_l1s1) * plitr1c_loss
         if (litr1n > 0._r8) litr1n_to_soil1n = plitr1c_loss / cn_l1
         sminn_to_soil1n_l1 = pmnf_l1s1
         net_nmin = net_nmin - pmnf_l1s1
	end if

	! litter 2 fluxes (cellulose pool)
	if (litr2c > 0._r8) then
         if (pmnf_l2s2 > 0._r8) then
            plitr2c_loss = plitr2c_loss * fpi
            pmnf_l2s2 = pmnf_l2s2 * fpi
            sminn_to_denit_l2s2 = 0._r8
         else
            sminn_to_denit_l2s2 = -dnp * pmnf_l2s2
         end if
         litr2_hr = rf_l2s2 * plitr2c_loss
         litr2c_to_soil2c = (1._r8 - rf_l2s2) * plitr2c_loss
         if (litr2n > 0._r8) litr2n_to_soil2n = plitr2c_loss / cn_l2
         sminn_to_soil2n_l2 = pmnf_l2s2
         net_nmin = net_nmin - pmnf_l2s2
	end if

	! litter 3 fluxes (lignin pool)
	if (litr3c > 0._r8) then
         if (pmnf_l3s3 > 0._r8) then
            plitr3c_loss = plitr3c_loss * fpi
            pmnf_l3s3 = pmnf_l3s3 * fpi
            sminn_to_denit_l3s3 = 0._r8
         else
            sminn_to_denit_l3s3 = -dnp * pmnf_l3s3
         end if
         litr3_hr = rf_l3s3 * plitr3c_loss
         litr3c_to_soil3c = (1._r8 - rf_l3s3) * plitr3c_loss
         if (litr3n > 0._r8) litr3n_to_soil3n = plitr3c_loss / cn_l3
         sminn_to_soil3n_l3 = pmnf_l3s3
         net_nmin = net_nmin - pmnf_l3s3
	end if

	! SOM 1 fluxes (fast rate soil organic matter pool)
	if (soil1c > 0._r8) then
        if (pmnf_s1s2 > 0._r8) then
        psoil1c_loss = psoil1c_loss * fpi
        pmnf_s1s2 = pmnf_s1s2 * fpi
        sminn_to_denit_s1s2 = 0._r8
        else
        sminn_to_denit_s1s2 = -dnp * pmnf_s1s2
        end if
        soil1_hr = rf_s1s2 * psoil1c_loss
        soil1c_to_soil2c = (1._r8 - rf_s1s2) * psoil1c_loss
        soil1n_to_soil2n = psoil1c_loss / cn_s1
        sminn_to_soil2n_s1 = pmnf_s1s2
        net_nmin = net_nmin - pmnf_s1s2
	end if

      ! SOM 2 fluxes (medium rate soil organic matter pool)
	if (soil2c > 0._r8) then
        if (pmnf_s2s3 > 0._r8) then
        psoil2c_loss = psoil2c_loss * fpi
        pmnf_s2s3 = pmnf_s2s3 * fpi
        sminn_to_denit_s2s3 = 0._r8
        else
        sminn_to_denit_s2s3 = -dnp * pmnf_s2s3
        end if
        soil2_hr = rf_s2s3 * psoil2c_loss
        soil2c_to_soil3c = (1._r8 - rf_s2s3) * psoil2c_loss
        soil2n_to_soil3n = psoil2c_loss / cn_s2
        sminn_to_soil3n_s2 = pmnf_s2s3
        net_nmin = net_nmin - pmnf_s2s3
	end if

	! SOM 3 fluxes (slow rate soil organic matter pool)
	if (soil3c > 0._r8) then
	if (pmnf_s3s4 > 0._r8) then
	psoil3c_loss = psoil3c_loss * fpi
	pmnf_s3s4 = pmnf_s3s4 * fpi
	sminn_to_denit_s3s4 = 0._r8
	else
	sminn_to_denit_s3s4 = -dnp * pmnf_s3s4
	end if
	soil3_hr = rf_s3s4 * psoil3c_loss
	soil3c_to_soil4c = (1._r8 - rf_s3s4) * psoil3c_loss
	soil3n_to_soil4n = psoil3c_loss / cn_s3
	sminn_to_soil4n_s3 = pmnf_s3s4
	net_nmin = net_nmin - pmnf_s3s4
	end if

	! SOM 4 fluxes (slowest rate soil organic matter pool)
	if (soil4c > 0._r8) then
	soil4_hr = psoil4c_loss
	soil4n_to_sminn = psoil4c_loss / cn_s4
	sminn_to_denit_s4 = -dnp * pmnf_s4
	net_nmin = net_nmin- pmnf_s4
	end if

	!print *, soil1_hr, " hr ", soil2_hr , " ",  soil3_hr , " ",  soil4_hr , " ",  litr1_hr , " ",  litr2_hr , " ",  litr3_hr
	! Calculate total fraction of potential HR, for methane code
	hrsum = soil1_hr + soil2_hr + soil3_hr + soil4_hr + litr1_hr + litr2_hr + litr3_hr
	hrsum = hrsum / soilsample_scalar ! * real(timestep, r8)
	! Nitrogen limitation / (low)-moisture limitation
      
	!if(hrsum / 12.0 > (0.95 * conc_o2)) then
	!     	o2limiting = 0.95 * conc_o2 / (hrsum / 12.0 )
	!    	hrsum = 0.95 * conc_o2 * 12.0
	!   else
	!  o2limiting = 1.
	! end if
      
	o2limiting = 2.0 * conc_o2 / (conc_o2 + 0.0005)
       
!	print *, hrsum, "hrsum" , m_dAC      
	m_dAC = m_dAC + hrsum * hrtoac
	! for ac pool
	o2limiting = o2limiting + hrtoac
	
	if(o2limiting >= 1.) then
	o2limiting = 1.
	end if

	hrsum = hrsum * o2limiting

	litr1_hr = litr1_hr * o2limiting
	litr2_hr = litr2_hr * o2limiting
	litr3_hr = litr3_hr * o2limiting
	soil1_hr = soil1_hr * o2limiting
	soil2_hr = soil2_hr * o2limiting
	soil3_hr = soil3_hr * o2limiting
	soil4_hr = soil4_hr * o2limiting

	cwdc_loss = cwdc_loss * o2limiting
	cwdn_loss = cwdn_loss * o2limiting
	cwdc_to_litr2c = cwdc_to_litr2c * o2limiting
	cwdc_to_litr3c = cwdc_to_litr3c * o2limiting
	cwdn_to_litr2n = cwdn_to_litr2n * o2limiting
	cwdn_to_litr3n = cwdn_to_litr3n * o2limiting
	litr1c_to_soil1c = litr1c_to_soil1c * o2limiting
	litr1n_to_soil1n = litr1n_to_soil1n * o2limiting
	litr2c_to_soil2c = litr2c_to_soil2c * o2limiting
	litr2n_to_soil2n = litr2n_to_soil2n * o2limiting
	litr3c_to_soil3c = litr3c_to_soil3c * o2limiting
	litr3n_to_soil3n = litr3n_to_soil3n * o2limiting
	soil1c_to_soil2c = soil1c_to_soil2c * o2limiting
	soil1n_to_soil2n = soil1n_to_soil2n * o2limiting
	soil2c_to_soil3c = soil2c_to_soil3c * o2limiting
	soil2n_to_soil3n = soil2n_to_soil3n * o2limiting
	soil3c_to_soil4c = soil3c_to_soil4c * o2limiting
	soil3n_to_soil4n = soil3n_to_soil4n * o2limiting
	sminn_to_denit_s4 = sminn_to_denit_s4 * o2limiting
 !print *, "litr1_hr ", litr1_hr, "litr2_hr ", litr2_hr, "litr3_hr ", litr3_hr, "soil1_hr ", soil1_hr, &
!		"soil2_hr ", soil2_hr, "soil3_hr ", soil3_hr, "soil4_hr ", soil4_hr
	if (phr > 0._r8) then
	fphr = hrsum / phr * w_scalar
	fphr = max(fphr, 0.01_r8) 		! Prevent overflow errors for 0 respiration
	else
	fphr = 1._r8
	end if
 
	litr1c = litr1c - litr1_hr
	litr2c = litr2c - litr2_hr
	litr3c = litr3c - litr3_hr
	soil1c = soil1c - soil1_hr
	soil2c = soil2c - soil2_hr
	soil3c = soil3c - soil3_hr
	soil4c = soil4c - soil4_hr
	
	cwdc = cwdc - cwdc_loss
	cwdn = cwdn - cwdn_loss
	litr2c = litr2c + cwdc_to_litr2c
	litr3c = litr3c + cwdc_to_litr3c
	litr2n = litr2n + cwdn_to_litr2n
	litr3n = litr3n + cwdn_to_litr3n
	
	litr1c = litr1c - litr1c_to_soil1c
	litr1n = litr1n - litr1n_to_soil1n
	soil1c = soil1c + litr1c_to_soil1c
	soil1n = soil1n + litr1n_to_soil1n
	
	litr2c = litr2c - litr2c_to_soil2c
	litr2n = litr2n - litr2n_to_soil2n
	soil2c = soil2c + litr2c_to_soil2c
	soil2n = soil2n + litr2n_to_soil2n
	
	litr3c = litr3c - litr3c_to_soil3c
	litr3n = litr3n - litr3n_to_soil3n
	soil3c = soil3c + litr3c_to_soil3c
	soil3n = soil3n + litr3n_to_soil3n
	
	soil1c = soil1c - soil1c_to_soil2c
	soil1n = soil1n - soil1n_to_soil2n
	soil2c = soil2c + soil1c_to_soil2c
	soil2n = soil2n + soil1n_to_soil2n
	
	soil2c = soil2c - soil2c_to_soil3c
	soil2n = soil2n - soil2n_to_soil3n
	soil3c = soil3c + soil2c_to_soil3c
	soil3n = soil3n + soil2n_to_soil3n
	
	soil3c = soil3c - soil3c_to_soil4c
	soil3n = soil3n - soil3n_to_soil4n
	soil4c = soil4c + soil3c_to_soil4c
	soil4n = soil4n + soil3n_to_soil4n
	
	!soil4c = soil4c - psoil4c_loss
	soil4n = soil4n - sminn_to_denit_s4
end subroutine decomp
	! decomposition subroutine end


subroutine microbe(m_dAC, m_dAce, m_dOrgAcid, m_dCH4, m_dO2, m_dCO2, m_dH2, m_dAceMethanogens, m_dH2Methanogens, &
	m_dMethanotrophs, m_dAOMMethanotrophs, ch4_prod, co2_prod, h2_prod, ch4_oxid, o2_cons, soiltemp, soilpH, &
	hco3, microbeparfilename, microberecov)
	implicit none

	integer, parameter 			:: r8 = selected_real_kind(12) 		! 8 byte real

	real(r8), intent(inout)		:: m_dAC
	real(r8), intent(inout)		:: m_dAce
	real(r8), intent(inout)		:: m_dOrgAcid
	real(r8), intent(inout)		:: m_dCH4
	real(r8), intent(inout)		:: m_dO2
	real(r8), intent(inout)		:: m_dCO2
	real(r8), intent(inout)		:: m_dH2
	real(r8), intent(inout)		:: m_dAceMethanogens
	real(r8), intent(inout)		:: m_dH2Methanogens
	real(r8), intent(inout)		:: m_dMethanotrophs
	real(r8), intent(inout)		:: m_dAOMMethanotrophs
	real(r8), intent(in)			:: soiltemp
	real(r8), intent(inout)		:: soilpH
	real(r8), intent(inout)		:: hco3
	real(r8), intent(out)		:: ch4_prod
	real(r8), intent(out)		:: co2_prod
	real(r8), intent(out)		:: h2_prod
	real(r8), intent(out)		:: ch4_oxid
	real(r8), intent(out)		:: o2_cons
	real(r8), intent(IN)			:: microberecov
	character(len=256), intent(in) :: microbeparfilename
	
	! soil properties
	real(r8)					:: sand
	real(r8)					:: clay
	real(r8)					:: silt
	real(r8)					:: psi
	real(r8)					:: psisat
	real(r8)					:: vwc
	real(r8) 					:: vwcsat
	real(r8)					:: organic
	real(r8)					:: smp_l
	real(r8)					:: origionalsoilph
	!end 
	real(r8) 					:: ACCO2Prod
	real(r8) 					:: ACConcentration
	real(r8) 					:: AceProd
	real(r8) 					:: ACH2Prod
	real(r8) 					:: H2AceProd
	real(r8) 					:: H2CH4Prod
	real(r8) 					:: H2PlantFlux
	real(r8) 					:: H2Cons
	real(r8) 					:: AceCons
	real(r8) 					:: CH4Prod
	real(r8) 					:: CH4Oxid
	real(r8) 					:: AOMCH4Oxid
	real(r8) 					:: CH4PlantFlux
	real(r8) 					:: CH4Ebull
	real(r8) 					:: O2PlantFlux
	real(r8) 					:: PlantO2Cons
	real(r8) 					:: AerO2Cons
	real(r8) 					:: CH4O2Cons
	real(r8) 					:: CO2PlantFlux
	real(r8) 					:: CO2Prod
	real(r8) 					:: H2CO2Cons
	real(r8) 					:: AceMethanogenGrowth
	real(r8) 					:: AceMethanogenDying
	real(r8) 					:: H2MethanogenGrowth
	real(r8) 					:: H2MethanogenDying
	real(r8) 					:: MethanotrophGrowth
	real(r8) 					:: MethanotrophDying
	real(r8) 					:: AOMMethanotrophGrowth
	real(r8) 					:: AOMMethanotrophDying
	real(r8) 					:: dTempH2
	real(r8) 					:: dTempratio

	real(r8) 					:: m_dKAce
	real(r8) 					:: m_dAceProdACmax
	real(r8) 					:: m_dKAceProdO2
	real(r8) 					:: m_dH2ProdAcemax
	real(r8) 					:: m_dKH2ProdAce
	real(r8) 					:: m_dKCO2ProdAce
	real(r8) 					:: m_dGrowRH2Methanogens
	real(r8) 					:: m_dDeadRH2Methanogens
	real(r8) 					:: m_dYH2Methanogens
	real(r8) 					:: m_dGrowRAceMethanogens
	real(r8) 					:: m_dDeadRAceMethanogens
	real(r8) 					:: m_dYAceMethanogens
	real(r8) 					:: m_dGrowRMethanotrophs
	real(r8) 					:: m_dDeadRMethanotrophs
	real(r8) 					:: m_dYMethanotrophs
	
	real(r8) 					:: m_dGrowRAOMMethanotrophs
	real(r8) 					:: m_dDeadRAOMMethanotrophs
	real(r8) 					:: m_dYAOMMethanotrophs
	
	real(r8) 					:: m_dAceProdQ10
	real(r8) 					:: m_dACProdQ10
	real(r8)					:: m_dACMinQ10
	real(r8) 					:: m_dAceH2min
	real(r8) 					:: m_dCH4H2min
	real(r8) 					:: m_dKH2ProdCH4
	real(r8) 					:: m_dKCO2ProdCH4
	real(r8) 					:: m_dH2CH4ProdQ10
	real(r8) 					:: m_dH2AceProdQ10
	real(r8) 					:: m_dKCH4ProdAce
	real(r8) 					:: m_dKCH4ProdO2
	real(r8) 					:: m_dCH4ProdQ10
	real(r8) 					:: m_drCH4Prod
	real(r8) 					:: m_dKCH4OxidCH4	
	real(r8) 					:: m_dKAOMCH4OxidCH4
	
	real(r8) 					:: m_dKCH4OxidO2
	
	real(r8) 					:: m_dCH4OxidQ10
	real(r8) 					:: m_dAOMCH4OxidQ10
	
	real(r8)					:: m_drAer
	real(r8)					:: m_dKAerO2
	real(r8)					:: m_dAerDecomQ10
	real(r8)					:: m_drCH4Oxid
	real(r8)					:: m_dKe
	real(r8)					:: m_dCH4min
	real(r8)					:: m_dAirCH4
	real(r8)					:: m_dAirH2
	real(r8)					:: m_dAirO2
	real(r8)					:: m_dAirCO2
	integer					:: IsH2Production
	real(r8)					:: H2maxCH4
	real(r8)					:: AcemaxCH4
	real(r8)					:: aom
	
	!real(r8) :: AOM
	real(r8) 					:: HCH4Prod

	character(len=40)			:: microbeparname
	
	real(r8) 					:: pHeffect
	real(r8) 					:: pHmax
	real(r8)					:: pHmin
	real(r8)					:: pHopt

	integer					:: timestep
	real(r8)					:: forc_pco2
	real(r8)					:: forc_pch4
	real(r8)					:: forc_po2
	real(r8)					:: forc_ph2
	real(r8)					:: atmco2
	real(r8)					:: atmch4
	real(r8)					:: atmo2
	real(r8)					:: atmh2
	real(r8)					:: soilvolume
	real(r8)					:: bottlevolume
	integer					:: FlagpH
	Integer					:: FlagO2
	real(r8)					:: soilsample_scalar

	common	/global/ &
		timestep, &
		forc_pco2, &
		forc_pch4, &
		forc_po2, &
		forc_ph2, &
		atmco2, &
		atmch4, &
		atmo2, &
		atmh2, &
		soilvolume, &
		bottlevolume, &
		FlagpH, &
		FlagO2, &
		soilsample_scalar	
		
	common /soil/ &
		sand, &
		clay, &
		silt, &
		psi, &
		psisat, &
		vwc, &
		vwcsat, &
		smp_l, &
		origionalsoilph

	m_dKAce = 15 ! //3000	//36*1000/12/50/4    /50 for the microbe c in soc, // mMol C m-3 : convert from 36 gC m-3

!	m_dAceProdACmax = 33.56;	 // *24 mMol m-3 h-1: convert from 1.44 m MolL-1 h-1
	m_dAceProdACmax = 0.005 !;//14.4; //14.4E3; // mMol/m3/d 0.999995; // 1-pow(0.6, 23)  0.4h-1  // Grant R.F, 1998
	m_dKAceProdO2 = 10 !;  // mMol/m3
	m_dH2ProdAcemax = 1.35e-5 !;  // mMol/m3   1.35; // 0.05625 * 24mMol / mMol Acetate-C h-1 : convert from 6.75e-3 mMol acetate g-1 h-1
	m_dKH2ProdAce = 1.65e-3 !;	 // mMol/m3   //16.5e-3;
	m_dKCO2ProdAce = 8.25e-3 !;		// mMol/m3   8.25e-3;
	m_dGrowRH2Methanogens = 0.023 !;		// d-1  0.023*24=0.552, (1-0.023)^24 = 0.5589
	m_dDeadRH2Methanogens = 0.007 !;		//0.0735 d-1   0.016*24  //0.384;  0.007 * 24 = 0.168; 0.002 - 0.012 h-1
	m_dYH2Methanogens = 0.20 !;
	m_dGrowRAceMethanogens = 0.02 !;		// d-1
	m_dDeadRAceMethanogens = 0.001 !;		 //0.01 d-1
	m_dYAceMethanogens = 0.04 !;   // 0.04; molC/(molAcetate -C) : convert from 0.08 mol C (mol acetate)-1
	m_dGrowRMethanotrophs = 0.024 !;		// d-1
	m_dDeadRMethanotrophs = 0.002 !;		//d-1
	m_dYMethanotrophs = 0.40 !;	// molC/(mol CH4 -C) : convert from 0.4 mol C (mol CH4)-1

	m_dGrowRAOMMethanotrophs = 0.024 !;		// d-1
	m_dDeadRAOMMethanotrophs = 0.002 !;		//d-1
	m_dYAOMMethanotrophs = 0.40 !;	// molC/(mol CH4 -C) : convert from 0.4 mol C (mol CH4)-1

	m_dAceProdQ10 = 2.0 !;
	m_dACProdQ10 = 1.5 !;
	m_dACMinQ10 = 1.5 !;
	m_dAceH2min = 0.48e-3 !;		 //0.48e-3;   mMol/m3
	m_dCH4H2min = 0.27e-4 !;	 // 0.27e-4;   mMol/m3
	m_dKH2ProdCH4 = 7.75e-3 !;	// mMol/m3
	m_dKCO2ProdCH4 = 1.98e-3 !;	 // mMol/m3
	m_dH2CH4ProdQ10 = 3.5 !;
	m_dH2AceProdQ10 = 3.5 !;
	m_dKCH4ProdAce = 5 !; //0.005 !;		 // mMol/m3
	m_dKCH4ProdO2 = 10 !; // 0.01;	// mMol/m3
	m_dCH4ProdQ10 = 4 !;
	m_drCH4Prod = 0.5 !;		// MolCH4(Mol acetate -C)-1 : convert from 2 MolCH4(Mol acetate)-1
	m_dKCH4OxidCH4 = 5 !; //0.005;		// mMol/m3
	m_dKAOMCH4OxidCH4 = 2
	
	m_dKCH4OxidO2 = 10 !; // 0.01;		// mMol/m3
	m_dCH4OxidQ10 = 2.0 !;
	m_dAOMCH4OxidQ10 = 2.0 !
	
	m_drAer = 2 !;					// MolO2(mol C)-1
	m_dKAerO2 = 10 !;				 //0.01 mMol/m3
	m_dAerDecomQ10 = 2.0 !;
	m_drCH4Oxid = 2 !;			// MolO2(molCH4)-1
	m_dKe = 0.03 !;			 // 0.05 h-1
	m_dCH4min = 0.5 !;			 //0.5 mMol/m3

	m_dAirCH4 = 0.0893 !;  // mM/m3
	m_dAirH2 = 0.0257 !; // mM/m3
	m_dAirO2 = 9.375e3 !; // mM/m3
	m_dAirCO2 = 16.295 !; // mM/m3

	pHmax = 11.
	pHmin = 3.
	pHopt = 7.

	open(unit = 101, file=microbeparfilename)
	read (101,*) microbeparname, m_dKAce
	read (101,*) microbeparname, m_dAceProdACmax
	read (101,*) microbeparname, m_dKAceProdO2
	read (101,*) microbeparname, m_dH2ProdAcemax
	read (101,*) microbeparname, m_dKH2ProdAce
	read (101,*) microbeparname, m_dKCO2ProdAce
	read (101,*) microbeparname, m_dGrowRH2Methanogens
	read (101,*) microbeparname, m_dDeadRH2Methanogens
	read (101,*) microbeparname, m_dYH2Methanogens
	read (101,*) microbeparname, m_dGrowRAceMethanogens
	read (101,*) microbeparname, m_dDeadRAceMethanogens
	read (101,*) microbeparname, m_dYAceMethanogens
	read (101,*) microbeparname, m_dGrowRMethanotrophs
	read (101,*) microbeparname, m_dDeadRMethanotrophs
	read (101,*) microbeparname, m_dYMethanotrophs
	
	read (101,*) microbeparname, m_dGrowRAOMMethanotrophs
	read (101,*) microbeparname, m_dDeadRAOMMethanotrophs
	read (101,*) microbeparname, m_dYAOMMethanotrophs
	
	read (101,*) microbeparname, m_dAceProdQ10
	read (101,*) microbeparname, m_dACProdQ10
	read (101,*) microbeparname, m_dACMinQ10
	read (101,*) microbeparname, m_dAceH2min
	read (101,*) microbeparname, m_dCH4H2min
	read (101,*) microbeparname, m_dKH2ProdCH4
	read (101,*) microbeparname, m_dKCO2ProdCH4
	read (101,*) microbeparname, m_dH2CH4ProdQ10
	read (101,*) microbeparname, m_dH2AceProdQ10
	read (101,*) microbeparname, m_dKCH4ProdAce
	read (101,*) microbeparname, m_dKCH4ProdO2
	read (101,*) microbeparname, m_dCH4ProdQ10
	read (101,*) microbeparname, m_drCH4Prod
	read (101,*) microbeparname, m_dKCH4OxidCH4
	read (101,*) microbeparname, m_dKAOMCH4OxidCH4
	read (101,*) microbeparname, m_dKCH4OxidO2
	read (101,*) microbeparname, m_dCH4OxidQ10
	read (101,*) microbeparname, m_dAOMCH4OxidQ10
	read (101,*) microbeparname, m_drAer
	read (101,*) microbeparname, m_dKAerO2
	read (101,*) microbeparname, m_dAerDecomQ10
	read (101,*) microbeparname, m_drCH4Oxid
	read (101,*) microbeparname, m_dKe
	read (101,*) microbeparname, m_dCH4min
	read (101,*) microbeparname, m_dAirCH4
	read (101,*) microbeparname, m_dAirH2
	read (101,*) microbeparname, m_dAirO2
	read (101,*) microbeparname, m_dAirCO2
	read (101,*) microbeparname, aom
!	read (101,*) microbeparname, AcemaxCH4
	close(101)

	m_dGrowRH2Methanogens = microberecov * m_dGrowRH2Methanogens
	m_dDeadRH2Methanogens= microberecov * m_dDeadRH2Methanogens
	
	!m_dGrowRH2Methanogens = m_dGrowRH2Methanogens * min(1.0, max(0.0, (1.0 - m_dCH4 / H2maxCH4)))
		
	m_dGrowRAceMethanogens = microberecov * m_dGrowRAceMethanogens
	m_dDeadRAceMethanogens = microberecov * m_dDeadRAceMethanogens
	
	!m_dGrowRAceMethanogens = m_dGrowRAceMethanogens * min(1.0, max(0.0, (1.0 - m_dCH4 / AcemaxCH4)))
	
	m_dGrowRMethanotrophs = microberecov * m_dGrowRMethanotrophs
	m_dDeadRMethanotrophs = microberecov * m_dDeadRMethanotrophs
		
	!if(m_dAceMethanogens > )
	!m_dGrowRAOMMethanotrophs = microberecov * m_dGrowRAOMMethanotrophs
	!m_dDeadRAOMMethanotrophs = microberecov * m_dDeadRAOMMethanotrophs	
	
	if(soilpH < 4) then
		pHeffect = 0.0
	else
		if(soilpH > 10) then
		pHeffect = 0.0
		else
		pHeffect = (soilpH - pHmin) * (soilpH - pHmax) / ((soilpH - pHmin) * (soilpH - pHmax) - (soilpH - pHopt) * (soilpH - pHopt))
		end if
	end if
	!print *, "H2: ", m_dH2
	
	if(pHeffect >= 1.0) then
	pHeffect = 1.0
	end if
	
	ACConcentration = 0.0

	ACConcentration = ACConcentration + m_dAC / 12.
	!print *, "m_dO2: ", m_dO2
	!print *, "m_dO2: ", m_dO2, "m_dAceProdACmax: ", m_dAceProdACmax, "ACConcentration: ", ACConcentration,  m_dKAce, m_dKAceProdO2
!print *, "AC", ACConcentration, "ACE: ", m_dAce,"co2", m_dCO2, "CH4", m_dCH4 

	if(m_dO2 <= 1e-3) then
	!AceProd = m_dAceProdACmax * (ACConcentration / (ACConcentration + m_dKAce)) &
	!	* (1 - m_dO2 / (m_dO2 + m_dKAceProdO2)) &
	!	* (m_dACMinQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect! * (1 - ACConcentration/ (ACConcentration + 0.01))

	AceProd = m_dAceProdACmax * (ACConcentration / (ACConcentration + m_dKAce)) &
		* (1 - m_dO2 / (m_dO2 + m_dKAceProdO2)) &
		* (m_dACMinQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
		
	!ACH2Prod = AceProd / 6.0
	!ACH2Prod = AceProd
	!ACCO2Prod = 0.5 * AceProd
	IsH2Production = 1
	else
	AceProd = m_dAceProdACmax * m_dO2 / (m_dO2 + m_dKAceProdO2) &
		* (ACConcentration / (ACConcentration + m_dKAce)) &
		* (m_dAceProdQ10 ** (soiltemp - 13.5) / 10.) * pHeffect
	!ACCO2Prod = AceProd
	ACH2Prod = 0
	IsH2Production = 0
	end if
	
	if(AceProd < 0) then
	AceProd = 0
	end if

	if(ACConcentration > (AceProd * 1.5)) then
	ACConcentration = ACConcentration - AceProd * 1.5
	else	
	AceProd = ACConcentration / 1.5
	ACConcentration = ACConcentration - AceProd * 1.5
	end if
		
	if(IsH2Production == 1.0)  then
	!ACH2Prod = (AceProd / 6.0)
	ACH2Prod = AceProd * 1.5				! based on C mole, not real acetate
	ACCO2Prod = 0.5 * AceProd
	else
	ACCO2Prod = AceProd
	ACH2Prod = 0.0
	end if
	!print *, "AceProd: ", AceProd," ACCO2Prod: ",ACCO2Prod
	m_dH2 = m_dH2 + ACH2Prod ! / volume * 1000.0
	 		
	if(ACConcentration < 0.0) then
	ACConcentration = 0.0
	end if
 

	!print *, "m_dO2: ", m_dO2, "ACConcentration: ", ACConcentration,  m_dKAce, m_dKAceProdO2
	!print *,m_dAce, " Ace and Prod ", AceProd, "H2: ", m_dH2, " ch4h2min: ", m_dCH4H2min, "ACConcentration: ", ACConcentration

! use a more smoothing way to represent the following threshold
	!~ if(m_dO2 > 1e-3) then
	!~ HCH4Prod = 0.
	!~ H2AceProd = 0.	
	!~ H2CH4Prod = 0.
	!~ else
	!~ if(m_dH2 <= m_dCH4H2min) then
	!~ ! Xiaofeng replaced the following two lines code with new mechanism of CH4 production from CO2
	!~ !H2AceProd = 0
	!~ !H2CH4Prod = 0
	!~ ! end
	!~ HCH4Prod = m_dGrowRH2Methanogens / m_dYH2Methanogens * m_dH2Methanogens * m_dH2 / ( m_dH2 + m_dKH2ProdCH4) &
		!~ * m_dCO2 / (m_dCO2 + m_dKCO2ProdCH4) * (m_dH2CH4ProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
	!~ H2AceProd = 0	
	!~ H2CH4Prod = 0
	!~ !HCH4Prod = 0
	!~ !print *, "HCH4Prod: ", HCH4Prod	
	!~ else
	!~ if(m_dH2 <= m_dAceH2min) then
	!~ H2CH4Prod = m_dGrowRH2Methanogens / m_dYH2Methanogens * m_dH2Methanogens * m_dH2 / ( m_dH2 + m_dKH2ProdCH4) &
		!~ * m_dCO2 / (m_dCO2 + m_dKCO2ProdCH4) * (m_dH2CH4ProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
	!~ H2AceProd = 0.
	!~ !print *, "H2CH4Prod: ", H2CH4Prod
	!~ else
!~ !	if(m_dH2 >= m_dAceH2min) then
	!~ H2AceProd = m_dH2ProdAcemax * m_dH2 / (m_dH2 + m_dKH2ProdAce) * m_dCO2 / (m_dCO2 + m_dKCO2ProdAce) &
		!~ * (m_dH2AceProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
	!~ H2CH4Prod = 0
	!~ !
	!~ end if
	!~ HCH4Prod = 0.
	!~ end if
	!~ end if
!print *, "AC", ACConcentration, "ACE: ", m_dAce,"co2", m_dCO2, "CH4", m_dCH4, "H2 ", m_dH2

	if(m_dO2 > 1e-3) then
	!HCH4Prod = 0.
	H2AceProd = 0.	
	H2CH4Prod = 0.
	else
	H2CH4Prod = m_dGrowRH2Methanogens / m_dYH2Methanogens * m_dH2Methanogens * m_dH2 / ( m_dH2 + m_dKH2ProdCH4) &
		* m_dCO2 / (m_dCO2 + m_dKCO2ProdCH4) * (m_dH2CH4ProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect !* (1 - m_dCH4 / (m_dCH4 + 10))
	if(m_dH2 < (m_dCH4H2min + m_dAceH2min) / 2) then
	H2CH4Prod = H2CH4Prod * 1 / (1 + exp(-5000000*(m_dH2 - m_dCH4H2min)))
	else
	H2CH4Prod = H2CH4Prod * 1 / (1 + exp(5000000*(m_dH2 - m_dAceH2min)))
	end if
	
	if(m_dH2 > m_dAceH2min) then
	H2AceProd = m_dH2ProdAcemax * m_dH2 / (m_dH2 + m_dKH2ProdAce) * m_dCO2 / (m_dCO2 + m_dKCO2ProdAce) &
		* (m_dH2AceProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
	H2AceProd = H2AceProd * 1 / (1 + exp(-5000000*(m_dH2 - m_dAceH2min)))
	end if
	end if
	
!print *, "m_dH2: ", m_dH2, "m_dAceH2min: ", m_dAceH2min, "m_dCH4H2min: ", m_dCH4H2min
!~ print *, "m_dH2ProdAcemax: ", m_dH2ProdAcemax, "m_dKH2ProdAce: ", m_dKH2ProdAce, "m_dKCO2ProdAce: ", m_dKCO2ProdAce
!print *, "H2AceProd: ", H2AceProd
!print *, "H2CH4Prod: ", H2CH4Prod

	H2Cons = 2. * H2AceProd + 4. * H2CH4Prod
			
	if((m_dH2 * 0.9) >= H2Cons) then
	H2Cons = H2Cons
	m_dH2 = m_dH2 - H2Cons
	Else
	dTempratio = (0.9 * m_dH2) / H2Cons
	!dTempH2 = m_dH2	
	H2AceProd = H2AceProd * dTempratio
	H2CH4Prod = H2CH4Prod * dTempratio
	H2Cons = m_dH2 * 0.9
	m_dH2 = m_dH2 - H2Cons
	end if

!	print *, "H2cons: ", H2Cons
!	
!	print *, "m_dH2ProdAcemax: ", m_dH2ProdAcemax, "m_dH2: ", m_dH2, "m_dCO2: ", m_dCO2
	
!	print *, "H2CH4Prod: ", H2CH4Prod, " HCH4Prod: ", HCH4Prod, "H2AceProd: ", H2AceProd, "m_dH2Methanogens: ", m_dH2Methanogens

	if(m_dH2 < 0) then
	m_dH2 = 0
	end if
	!print *, "H2cons: ", m_dH2
	!// For H2 dyndamics
	!if(m_dH2>g_dMaxH2inWater) then
	!H2PlantFlux = m_dPlantTrans * pow(RootDistribution, 0.5) * RootFactor * (m_dH2 - g_dMaxH2inWater)		
	!if((m_dH2 - H2PlantFlux)>0) then
	!H2PlantFlux = H2PlantFlux
	!else
	!H2PlantFlux = (m_dH2 - g_dMaxH2inWater)
	!end if
	!else
	
	H2PlantFlux = 0
	!end if
	
	m_dH2 = m_dH2 - H2PlantFlux
	
	if(m_dH2 < 0) then 
	m_dH2 = 0
	end if
	!	m_dDiffusion(DH2, m_dH2, 24);
	
	!	// For acetate dyndamics
	!	AceCons[z] = m_dGrowRAceMethanogens / m_dYAceMethanogens * m_dAceMethanogens[z] * m_dAce[z] * m_dAce[z] / (m_dAce[z] + m_dKCH4ProdAce)
	!	*(1 - m_dO2[z] / (m_dO2[z] + m_dKCH4ProdO2)) * Tfunction(m_dCH4ProdQ10, SoilTemp[i][z]);
	!print *, "CO2: ", m_dCO2
	if(m_dO2 < 1e-5) then
	AceCons = m_dGrowRAceMethanogens / m_dYAceMethanogens * m_dAceMethanogens * m_dAce / (m_dAce + m_dKCH4ProdAce) &
		* (m_dCH4ProdQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect * (1 - m_dCH4 / (m_dCH4 + 0.01))  * (1 - m_dCO2 / (m_dCO2 + 0.01))
	else
	AceCons = 0.
	endif
	
	if(m_dAce > AceCons * 2) then
	m_dAce = m_dAce - AceCons * 2
	else
	AceCons = 0.45 * m_dAce
	m_dAce = m_dAce - AceCons * 2
	end if

	m_dAce = m_dAce + (AceProd + H2AceProd)
!print *, "m_dAce : ",m_dAce, "AceProd: ",AceProd, "H2AceProd : ",H2AceProd
	if(m_dAce < 0) then
	m_dAce = 0
	end if

	!// For CH4 dyndamics
	!print *, "m_drCH4Prod * (1 - m_dYAceMethanogens): ", m_drCH4Prod * (1 - m_dYAceMethanogens) * AceCons, "H2CH4Prod: ", H2CH4Prod
	CH4Prod = AceCons + H2CH4Prod !+ HCH4Prod
!print *, "ch4 add: ",AceCons, H2CH4Prod	
	m_dCH4 = m_dCH4 + CH4Prod

	CH4Oxid = m_dGrowRMethanotrophs / m_dYMethanotrophs * m_dMethanotrophs * m_dCH4 / (m_dCH4 + m_dKCH4OxidCH4) &
		* m_dO2 / (m_dO2 + m_dKCH4OxidO2) * (m_dCH4OxidQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
		
	if(CH4Oxid > 0) then
	CH4Oxid = CH4Oxid
	else
	CH4Oxid = 0
	end if

	if(m_dCH4 > CH4Oxid .and. m_dO2>(2.0*CH4Oxid)) then
	CH4Oxid = CH4Oxid
		else
		if(m_dCH4<=0 .or. m_dO2<=0) then
		CH4Oxid = 0
		else
		CH4Oxid = min(0.8 * m_dCH4, m_dO2 / 2.0)
		end if
	end if

	if(CH4Oxid < 0) then
	CH4Oxid = 0
	end if

	m_dCH4 = m_dCH4 - CH4Oxid

	if(m_dCH4 < 0) then
	m_dCH4 = 0
	end if
	
	CH4PlantFlux = 0.
	CH4Ebull = 0.
	m_dCH4 = m_dCH4 - CH4Ebull - CH4PlantFlux
		
	!	// For O2 dyndamics
	AerO2Cons = m_drAer * ACCO2Prod
	CH4O2Cons = m_drCH4Oxid * CH4Oxid
	m_dO2 = m_dO2 - (AerO2Cons + CH4O2Cons)
	
	if(m_dO2 < 0) then
	m_dO2 = 0
	end if

	O2PlantFlux = 0
	
	m_dO2 = m_dO2 + O2PlantFlux

	PlantO2Cons = 0.
	
	if(m_dO2 >= PlantO2Cons) then
	PlantO2Cons = PlantO2Cons
	else
	PlantO2Cons = 0.9 * m_dO2
	end if
		
	m_dO2 = m_dO2 - PlantO2Cons
		
	if(m_dO2 < 0) then
	m_dO2 = 0
	end if
	
	!// For CO2 dyndamics
	!print *, "ACCO2Prod: ", ACCO2Prod, "CH4Oxid: ", CH4Oxid, "PlantO2Cons: ", PlantO2Cons, "AceCons: ", AceCons
	CO2Prod = ACCO2Prod +  CH4Oxid + PlantO2Cons + AceCons ! - m_drCH4Prod * (1 - m_dYAceMethanogens) * AceCons
	H2CO2Cons = H2AceProd + H2CH4Prod !+ HCH4Prod
	!print *, "H2AceProd: ", H2AceProd, "H2CH4Prod: ", H2CH4Prod
	
	m_dCO2 = m_dCO2 + CO2Prod - H2CO2Cons
	
	CO2PlantFlux = 0
		
	m_dCO2 = m_dCO2 - CO2PlantFlux

	if(m_dCO2 < 0) then
	m_dCO2 = 0
	end if
	
	!print *, "h2: ", m_dH2,  "O2: ", m_dO2,  "CO2: ",m_dCO2
	!if((m_dH2 * 0.5e-7) < 0.001 .and. m_dO2 < 0.00001 .and. m_dCO2 < 0.001) then
	!AOM = m_dCH4 * 0.001 * (hco3 + 1.) / (hco3 + 0.1)
	!AOM = 0.085 * m_dCH4 / (m_dCH4 + 37.)
	!m_dCH4 = m_dCH4 - AOM
	!hco3 = hco3 + AOM
	!print *, "hco3: ", hco3
	!end if
	
	! xiaofeng block AOM
	!if(m_dO2 < 0.0001) then
	!AOMCH4Oxid = m_dGrowRAOMMethanotrophs / m_dYAOMMethanotrophs * m_dAOMMethanotrophs * &
	!	m_dCH4 / (m_dCH4 + m_dKAOMCH4OxidCH4) * (m_dAOMCH4OxidQ10 ** ((soiltemp - 13.5) / 10.)) * pHeffect
	!endif
	
	!if(AOMCH4Oxid < m_dCH4) then
	!AOMCH4Oxid = AOMCH4Oxid
	!else
	!AOMCH4Oxid = m_dCH4
	!end if	
	! xiaofeng block AOM
	!AOMCH4Oxid = 0.85 * CH4Prod
	if(m_dO2 < 1e10) then
	AOMCH4Oxid = aom * CH4Prod
	m_dCH4 = m_dCH4 - AOMCH4Oxid
	end if

!	// For Microbe dyndamics
	AceMethanogenGrowth = m_dYAceMethanogens * AceCons
	AceMethanogenDying = m_dDeadRAceMethanogens * m_dAceMethanogens * (m_dAceMethanogens / (m_dAceMethanogens + 0.01))
	!print *, AceMethanogenGrowth," ",AceMethanogenDying
	H2MethanogenGrowth = m_dYH2Methanogens * H2CH4Prod
	H2MethanogenDying = m_dDeadRH2Methanogens * m_dH2Methanogens * (m_dH2Methanogens / (m_dH2Methanogens + 0.01))
	!print *, H2MethanogenGrowth," H2Methanogen ",H2MethanogenDying
	MethanotrophGrowth = m_dYMethanotrophs * CH4Oxid
	MethanotrophDying = m_dDeadRMethanotrophs * m_dMethanotrophs * (m_dMethanotrophs / (m_dMethanotrophs + 0.01))

	!AOMMethanotrophGrowth = m_dYAOMMethanotrophs * AOMCH4Oxid * pHeffect
	!AOMMethanotrophDying = m_dDeadRAOMMethanotrophs * m_dAOMMethanotrophs * pHeffect
	! xiaofeng block AOM

	m_dAceMethanogens = m_dAceMethanogens + (AceMethanogenGrowth - AceMethanogenDying)
	m_dH2Methanogens = m_dH2Methanogens + (H2MethanogenGrowth - H2MethanogenDying)
	m_dMethanotrophs = m_dMethanotrophs + (MethanotrophGrowth - MethanotrophDying)
	!m_dAOMMethanotrophs = m_dAOMMethanotrophs + (AOMMethanotrophGrowth - AOMMethanotrophDying)
	! xiaofeng block AOM

	!m_dCO2 = m_dCO2 + (AceMethanogenDying + H2MethanogenDying + MethanotrophDying) / 2.
	!m_dAC = m_dAC + (AceMethanogenDying + H2MethanogenDying + MethanotrophDying)
	
	if(m_dAceMethanogens <= 0.) then
	m_dAceMethanogens = 0.
	endif
	
	if(m_dH2Methanogens <= 0.) then
	m_dH2Methanogens = 0.
	endif
	
	if(m_dMethanotrophs <= 0.) then
	m_dMethanotrophs = 0.
	endif
	
	!if(m_dO2 < 1.) then
	!ACConcentration = ACConcentration + 0.5 * m_dMethanotrophs
	!m_dMethanotrophs = 0.5 * m_dMethanotrophs
	!end if
!print *, "AC", ACConcentration, "ACE: ", m_dAce,"co2", m_dCO2, "CH4", m_dCH4, "H2 ", m_dH2		
	m_dAC = ACConcentration * 12.
	!m_dAC = m_dAC + m_dAce * 0.2
	!m_dAce = m_dAce * 0.8
		
!	print *, "m_dAC: ", m_dAC, "m_dAce: ", m_dAce
	! pKa for acetate acid is 4.75
	! [AC-] * [H+] / [AC] = 1.78 * 10e-5
	! 0.42% AC will be dissociated in the water
	if(soilpH > 5.5) then
	soilpH = -1 * log10((10 ** (-origionalsoilph) + 0.0042 * 0.001 * m_dAce))
	if(soilpH < 5.5) then
		soilpH = 5.5
		end if
	end if
	
	!print *, " pH2: ", soilpH, " acetate acid: ", m_dAce
	!print *, "H2cons2 : ", m_dH2
	ch4_prod = CH4Prod
	co2_prod = CO2Prod
	h2_prod = ACH2Prod
	!ch4_oxid = CH4Oxid + AOMCH4Oxid
	! xiaofeng replaced the code in above line with the following line
	! xiaofeng block AOM
	ch4_oxid = CH4Oxid
	o2_cons = CH4O2Cons + PlantO2Cons
	
	!print *, CH4O2Cons, " ",PlantO2COns
end subroutine microbe

subroutine writeoutput(nr, hr, cwdc, cwdn, lit1c, lit1n, lit2c, lit2n, lit3c, lit3n, &
	som1c, som1n, som2c, som2n, som3c, som3n, som4c, som4n, &
	ace, orgacid, ch4_prod, co2_prod, h2_prod, &
	ch4_oxid, o2_cons, conc_ch4, conc_o2, conc_co2, h2con, &
	m_dAceMethanogens, m_dH2Methanogens, m_dMethanotrophs, &
	m_dAOMMethanotrophs, soilpH, outputfile)
	
	implicit none
	integer,parameter 		:: r8 = selected_real_kind(12) ! 8 byte real
	real(r8), intent(in)		:: ch4_prod(1:nr)
	real(r8), intent(in)		:: co2_prod(1:nr)
	real(r8), intent(in)		:: h2_prod(1:nr)
	real(r8), intent(in)		:: ch4_oxid(1:nr)
	real(r8), intent(in)		:: o2_cons(1:nr)
	real(r8), intent(in)		:: ace(1:nr)
	real(r8), intent(in)		:: orgacid(1:nr)
	real(r8), intent(in)		:: h2con(1:nr)
	integer, intent(in)		:: nr
	real(r8), intent(in)		:: hr(1:nr)
	real(r8), intent(in)		:: cwdc(1:nr)
	real(r8), intent(in)		:: cwdn(1:nr)
	real(r8), intent(in)		:: lit1c(1:nr)
	real(r8), intent(in)		:: lit1n(1:nr)
	real(r8), intent(in)		:: lit2c(1:nr)
	real(r8), intent(in)		:: lit2n(1:nr)
	real(r8), intent(in)		:: lit3c(1:nr)
	real(r8), intent(in)		:: lit3n(1:nr)
	real(r8), intent(in)		:: som1c(1:nr)
	real(r8), intent(in)		:: som1n(1:nr)
	real(r8), intent(in)		:: som2c(1:nr)
	real(r8), intent(in)		:: som2n(1:nr)
	real(r8), intent(in)		:: som3c(1:nr)
	real(r8), intent(in)		:: som3n(1:nr)
	real(r8), intent(in)		:: som4c(1:nr)
	real(r8), intent(in)		:: som4n(1:nr)
	real(r8), intent(in)		:: conc_ch4(1:nr)
	real(r8), intent(in)		:: conc_o2(1:nr)
	real(r8), intent(in)		:: conc_co2(1:nr)
	real(r8), intent(in)		:: m_dAceMethanogens(1:nr)
	real(r8), intent(in)		:: m_dH2Methanogens(1:nr)
	real(r8), intent(in)		:: m_dMethanotrophs(1:nr)
	real(r8), intent(in)		:: m_dAOMMethanotrophs(1:nr)
	real(r8), intent(in)		:: soilpH(1:nr)
	
	character(len = 256), intent(in)	:: outputfile
	
	real(r8)				:: carbon(1:nr)
	real(r8)				:: nitrogen(1:nr)
	integer 				:: n, ier
	
	open (1000, FILE=outputfile)
	carbon = cwdc + lit1c + lit2c + lit3c + som1c + som2c + som3c + som4c
	nitrogen = cwdn + lit1n + lit2n + lit3n + som1n + som2n + som3n + som4n

	!write(1000,*,iostat=ier) " carbon ", " nitrogen ", " ace ", " orgacid ", " hr " , " ch4_prod " ,  " co2_prod " , &
	!	 " h2_prod " ,  " ch4_oxid " ,  " o2_cons " ,  " conc_co2 " ,  " conc_ch4 " ,  " conc_o2 " ,  " h2con " , &
	!	 " m_dAceMethanogens " , " m_dH2Methanogens ", " m_dMethanotrophs " , & 
	!	 " m_dAOMMethanotrophs(n) " ,  " soilpH(n) " 
		 
	write(1000,*,iostat=ier) " ace ", " ch4_prod " ,  " co2_prod " , &
		 " h2_prod " ,  " ch4_oxid " ,  " o2_cons " ,  " conc_co2 " ,  " conc_ch4 " ,  " conc_o2 " ,  " h2con " , &
		 " m_dAceMethanogens " , " m_dH2Methanogens ", " m_dMethanotrophs " , & 
		 " m_dAOMMethanotrophs(n) " ,  " soilpH(n) " 
		
	do n = 1, nr
	!write(1000,*,iostat=ier)  carbon(n), nitrogen(n), ace(n), orgacid(n), hr(n), ch4_prod(n), co2_prod(n), &
	!	h2_prod(n), ch4_oxid(n), o2_cons(n), conc_co2(n), conc_ch4(n), conc_o2(n), h2con(n), &
	!	m_dAceMethanogens(n),m_dH2Methanogens(n),m_dMethanotrophs(n), & 
	!	m_dAOMMethanotrophs(n), soilpH(n)

	write(1000,*,iostat=ier)  ace(n), ch4_prod(n), co2_prod(n), &
		h2_prod(n), ch4_oxid(n), o2_cons(n), conc_co2(n), conc_ch4(n), conc_o2(n), h2con(n), &
		m_dAceMethanogens(n),m_dH2Methanogens(n),m_dMethanotrophs(n), & 
		m_dAOMMethanotrophs(n), soilpH(n)

	!1001 format(f7.2, f7.2, f7.2, f7.2, f7.2, f7.2, f7.2, f7.2)
	if (ier /= 0) then
	write(*,*) 'error in writing output'
	end if
	end do
	close(1000)
end subroutine writeoutput
	! write output subroutine end
