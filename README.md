# Quasi-particle-scattering-at-the-surface-of-Bismuth-telluride
This code is in the language IDL.
Results in making a 2D map of pixels of intensity 0-1 representing the equivalent probability of quasiparticle scaterring at that specific energy frequence in K and Q-space for topological insulator Bi2Ti3.


    PRO layerobj::circle2

    WIDGET_CONTROL, /hourglass

    side = 100
    r = side/6
    center = side/2 - 1.

  Creating the 7 layered Array
 
    object1 = MAKE_ARRAY(side,side,7, Value = .0)
    matrix1 = object1[*,*,0]
    matrix2 = object1[*,*,1]
    matrix3 = object1[*,*,2]
    matrix4 = object1[*,*,3]
    matrix5 = object1[*,*,4]
    matrix6 = object1[*,*,5]
    matrix7 = object1[*,*,6]

Determining the location of key point of the plane
  
    circle0 = [center,center]
    circle1 = [center,center-r*sqrt(3)]
    circle2 = [center+(cos(!pi/6.)*sqrt(3)*r),center-(sin(!pi/6.)*sqrt(3)*r)]
    circle3 = [center+(cos(!pi*11./6)*sqrt(3)*r),center-(sin(!pi*11./6)*sqrt(3)*r)]
    circle4 = [center,center+r*sqrt(3)]
    circle5 = [center+cos(!pi*7./6.)*sqrt(3)*r,center-(sin(!pi*7./6.)*sqrt(3)*r)]
    circle6 = [center+cos(!pi*5./6.)*sqrt(3)*r,center-(sin(!pi*5./6.)*sqrt(3)*r)]
    circle7 = [center-cos(!pi/3.)*2*r,center-r*sqrt(3)]
    circle8 = [center+cos(!pi/3.)*2*r,center-r*sqrt(3)]
    circle9 = [center-cos(!pi/3.)*2*r,center+r*sqrt(3)]
    circle10 = [center+cos(!pi/3.)*2*r,center+r*sqrt(3)]
    circle11 = [center-2*r,center]
    circle12 = [center+2*r,center]

  
    FOR x = 0., (side - 1.) DO BEGIN
    FOR y = 0., (side - 1.) DO BEGIN
      
      distance0 = sqrt((x - circle0[0])^2 + (y - circle0[1])^2)
      distance1 = sqrt((x - circle1[0])^2 + (y - circle1[1])^2)+.5
      distance2 = sqrt((x - circle2[0])^2 + (y - circle2[1])^2)+.5
      distance3 = sqrt((x - circle3[0])^2 + (y - circle3[1])^2)+.5
      distance4 = sqrt((x - circle4[0])^2 + (y - circle4[1])^2)+.5
      distance5 = sqrt((x - circle5[0])^2 + (y - circle5[1])^2)+.5
      distance6 = sqrt((x - circle6[0])^2 + (y - circle6[1])^2)+.5
      

 Here we are giving spin information to the conduction band 

      matrix1[x,y]=[1]
      if y-circle0[0] LE 0 then matrix2[x,y] = ACOS((x-circle0[0])/distance0) + !pi/2.
      if y-circle0[0] GT 0 then matrix2[x,y] = (5.*!pi)/2. - ACOS((x-circle0[0])/distance0)
      matrix6[x,y] = [1.]
      if y-circle0[0] LE 0 then matrix7[x,y] = ACOS((x-circle0[0])/distance0) + !pi*3./2.
      if y-circle0[0] GT 0 then matrix7[x,y] = (7.*!pi)/2. - ACOS((x-circle0[0])/distance0)
      
      
 Finishing edits for hexagonal CB
 
      If distance1 LT ((SQRT(3)-0.25)*r) then begin 
      matrix1[x,y]= 0.
      matrix2[x,y]= 0. 
      matrix6[x,y]= 0.  
      matrix7[x,y]= 0. 
      endif
      If distance2 LT ((SQRT(3)-0.25)*r) then begin 
      matrix1[x,y]= 0.
      matrix2[x,y]= 0. 
      matrix6[x,y]= 0.  
      matrix7[x,y]= 0. 
      endif
      If distance3 LT ((SQRT(3)-0.25)*r) then begin 
      matrix1[x,y]= 0.
      matrix2[x,y]= 0. 
      matrix6[x,y]= 0.  
     matrix7[x,y]= 0. 
      endif
      If distance4 LT ((SQRT(3)-0.25)*r) then begin 
      matrix1[x,y]= 0.
      matrix2[x,y]= 0. 
      matrix6[x,y]= 0.  
     matrix7[x,y]= 0. 
      endif
      If distance5 LT ((SQRT(3)-0.25)*r) then begin 
      matrix1[x,y]= 0.  
      matrix2[x,y]= 0. 
      matrix6[x,y]= 0.  
      matrix7[x,y]= 0. 
     endif
      If distance6 LT ((SQRT(3)-0.25)*r) then begin 
     matrix1[x,y]= 0. 
      matrix2[x,y]= 0.
      matrix6[x,y]= 0. 
      matrix7[x,y]= 0.
      endif
      

Creation of 6 circles for the Hexagon
       
       IF distance1 GT (r-0.5) and distance1 LT (r+0.5) THEN BEGIN
         x1 = x - circle1[0]
         y1 = y - circle1[1] 
         matrix1[x,y] = 1 
         matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF
        
      
       IF distance2 GT (r-0.5) and distance2 LT (r+0.5) THEN BEGIN
         x1 = x - circle2[0]
         y1 = y - circle2[1] 
         matrix1[x,y] = 1 
         matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF


       IF distance3 GT (r-0.5) and distance3 LT (r+0.5) THEN BEGIN
         x1 = x - circle3[0]
         y1 = y - circle3[1] 
         matrix1[x,y] = 1 
         matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF


       IF distance4 GT (r-0.5) and distance4 LT (r+0.5) THEN BEGIN
         x1 = x - circle4[0]
         y1 = y - circle4[1] 
         matrix1[x,y] = 1 
         matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF
       
       
       IF distance5 GT (r-0.5) and distance5 LT (r+0.5) THEN BEGIN
         x1 = x - circle5[0]
         y1 = y - circle5[1] 
         matrix1[x,y] = 1 
         matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF
       
       
       IF distance6 GT (r-0.5) and distance6 LT (r+0.5) THEN BEGIN
         x1 = x - circle6[0]
         y1 = y - circle6[1] 
         matrix1[x,y] = 1 
         ;matrix1[x,y]=(-1)*1.866*distance0/r+2.366
         ;matrix1[x,y] = 1.866*sqrt((x-center)^2+(y-center)^2)/r - 0.866
         if y-circle0[0] LE 0 then matrix2[x,y] = ACOS(x1/(r)) + !pi/2
         if y-circle0[0] GT 0 then matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(r))
       ENDIF
       
  This is the creation of a circular CB
       
       IF distance0 LT (side/25.) and distance0 GT (side/50.0) THEN BEGIN
            x1 = x - circle0[0]
            y1 = y - circle0[1] 
         
        IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/distance0) + !pi/2.
            matrix1[x,y] = [1.]
            matrix6[x,y] = [1.]
            matrix7[x,y] = ACOS(x1/distance0) + !pi
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [1.]
            matrix6[x,y] = [1.]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
         ENDIF
         
         
   

 Here is where I reset most pixels so that the 6 circles only make out a hexagon
        
          IF x LT (center-r-1) OR x GT (center+r+1) or y LT (center-round(r*sqrt(3)/2)-1) or y GT (center+round((r*sqrt(3)/2))+1) THEN BEGIN
            matrix1[x,y]= 0.
            matrix2[x,y]= 0. 
            matrix6[x,y]= 0.  
            matrix7[x,y]= 0. 
          ENDIF
        

    
Creating the star states

    x1 = x - circle0[0]
    y1 = y - circle0[1]
    

    If y EQ center then begin
      If x GE (center-2*r) AND x LE (center-r/4) Then begin
            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
      endif
      If x LE (center+2*r) AND x GE (center+r/4.) Then begin
            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
      endif
      endif
      
      IF y LE (sqrt(3)*x-35.87+1.) AND y GE (sqrt(3)*x-35.87-1.) Then begin
      If x GE (center-r) AND x LE (center-r/8.) Then begin

            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
      endif
      If x LE (center+r) AND x GE (center+r/8.) Then begin

            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
      endif
      endif
      
      IF y LE ((-1)*sqrt(3)*x+133.87+1.) AND y GE ((-1)*sqrt(3)*x+133.87-1.) Then begin
        If x GE (center-r) AND x LE (center-r/8.) Then begin

            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
        endif
        If x LE (center+r) AND x GE (center+r/8.) Then begin

            matrix2[x,y] = (5.*!pi)/2. - ACOS(x1/distance0)
            matrix1[x,y] = [0.25]
            matrix6[x,y] = [0.25]
            matrix7[x,y] = (7.*!pi)/2. - ACOS(x1/distance0)
         endif
      endif
     endfor
    endfor

 Making the center point 0

    matrix1[center,center]= 0.
    matrix2[center,center]= 0.
    matrix6[center,center]= 0.
    matrix7[center,center]= 0.
 

Creating circular states in the K-direction

     FOR x = 0., (side - 1.) DO BEGIN
     FOR y = 0., (side - 1.) DO BEGIN
    
      distance7 = sqrt((x - circle7[0])^2 + (y - circle7[1])^2)
      distance8 = sqrt((x - circle8[0])^2 + (y - circle8[1])^2)
      distance9 = sqrt((x - circle9[0])^2 + (y - circle9[1])^2)
      distance10 = sqrt((x - circle10[0])^2 + (y - circle10[1])^2)
      distance11 = sqrt((x - circle11[0])^2 + (y - circle11[1])^2)
      distance12 = sqrt((x - circle12[0])^2 + (y - circle12[1])^2)
   
        
        IF distance7 GT (1) and distance7 LT (2) THEN BEGIN
         x1 = x - circle7[0]
         y1 = y - circle7[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/(1.5)) + !pi/2
            matrix1[x,y] = 0.5
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(1.5))
            matrix1[x,y] = 0.5   
         ENDIF
       ENDIF
      
      
   IF distance8 GT (1) and distance8 LT (2) THEN BEGIN
       
         x1 = x - circle8[0]
         y1 = y - circle8[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/(1.5)) + !pi/2
            matrix1[x,y] = 0.5
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(1.5))
            matrix1[x,y] = 0.5  
         ENDIF
       ENDIF


  IF distance9 GT (1) and distance9 LT (2) THEN BEGIN

         x1 = x - circle9[0]
         y1 = y - circle9[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/1.5) + !pi/2
            matrix1[x,y] = 0.5
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/1.5)
            matrix1[x,y] = 0.5 
         ENDIF
       ENDIF

IF distance10 GT (1) and distance10 LT (2) THEN BEGIN

         x1 = x - circle10[0]
         y1 = y - circle10[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/1.5) + !pi/2
            matrix1[x,y] = 0.5
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(1.5))
            matrix1[x,y] = 0.5   
         ENDIF
       ENDIF
       
   IF distance11 GT (1) and distance11 LT (2) THEN BEGIN
         
         x1 = x - circle11[0]
         y1 = y - circle11[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/1.5) + !pi/2
            matrix1[x,y] = 0.5
            ;matrix1[x,y] = 1
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/(1.5))
            matrix1[x,y] = 0.5
            ;matrix1[x,y] = 1   
         ENDIF
       ENDIF
       
   IF distance12 GT (1) and distance12 LT (2) THEN BEGIN
    
         x1 = x - circle12[0]
         y1 = y - circle12[1] 
         
         IF y1 LE 0.0 THEN BEGIN
            matrix2[x,y] = ACOS(x1/1.5) + !pi/2
            matrix1[x,y] = 0.5
         ENDIF
         
         IF y1 GT 0.0 THEN BEGIN
            matrix2[x,y] = (5*!pi)/2 - ACOS(x1/1.5)
            matrix1[x,y] = 0.5
         ENDIF
       ENDIF
     
       
     ENDFOR
    ENDFOR

Autocorrelation process, Creation of matrices 3,4
   
    FOR centerx = ((1-side)/(2)), ((side-1)/2) DO BEGIN
       FOR centery = ((1-side)/2), ((side - 1)/2) DO BEGIN
           matrix3 = SHIFT(matrix1,centerx,centery)
           matrix4 = SHIFT(matrix2,centerx,centery)

           addition1 = TOTAL(((COS(matrix2)*COS(matrix4)+SIN(matrix2)*SIN(matrix4))+1.)/2.*matrix1*matrix3)
           addition2 = TOTAL(((COS(matrix7)*COS(matrix4)+SIN(matrix7)*SIN(matrix4))+1.)/2.*matrix6*matrix3)
           matrix5[(center+centerx),(center+centery)] = addition2 + addition1

         ENDFOR
     ENDFOR
    
       
Normalization process and turning the picture white-black

       normalization1 = MAX(matrix1)
       matrix1 = 1. - matrix1/normalization1
       
       normalization5 = MAX(matrix5)
       matrix5 = 1. - matrix5/normalization5

    result = obj_new('layerobj', fileType='AC', FwdData=matrix1, $ ; 

    change: FwdData=matrix# to get your relevant map
    group_leader=(*statePtr).tlb, stateptr=stateptr, $
    header=ptr_copy(header), revOrder=revOrder, $
    windowTitle= 'Matrix1 side=100', $


    xyUnits = xyunits, $
    micperpixel = kUnitsPerPix, $
    energies=*energies, fileName=fileName, $
    fPath=fPath, title=title, note=note, curveType=curveType, $
    xsize=side, ysize=side, ptsPerCurve=ptsPerCurve, $
    eUnits=eUnits, valueUnits=valueUnits, valLimits=valLimits, $
    xyformat='F6.2', zformat='G10.4',labelType=0)

    Util_AddLayerObjectToStatePtrList, result, stateptr

    result->display
   
    END
