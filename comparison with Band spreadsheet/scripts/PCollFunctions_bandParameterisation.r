############ Calculate collision probability at any given point along the rotor blade

pcoll = function (r,phi, updown) {
 
 cell_val = ifelse (r > 1, 21 -> cell_val, which(coverC$rad == (round(ceiling(r*100)/5) * 5)/100) -> cell_val)
 upper = coverC[cell_val,1]
 lower = coverC[cell_val - 1, 1]

 p = (r - lower) / (upper-lower)
 c = coverC[cell_val-1, 2] + p * (coverC[cell_val,2] - coverC[cell_val-1,2])


 radius = sampledRotorRadius[i] * r
 chord = sampledBladeWidth[i] * c

# Masden code uses the following - uses a wind-speed-pitch function 
# omega = sampledRotorSpeed[i] * 2 * pi /60
# pitch = sampledRotorPitch[i] * pi /180
 
# For full comparability with the Band spreadsheet
# the rotor speed and pitch have to be fixed
# Masden's code uses a functional relationship
# between pitch and wind-speed
# 
# So use following to compare with Band:

  omega = TurbineData$RotationSpeed * 2 * pi /60
  pitch = TurbineData$Pitch * pi /180
 
 
 
 phi = phi * pi /180
 
 ifelse(updown == "up", 1 -> direction, -1 -> direction)
 
 ifelse(species.dat$Flight == "Flapping", 
 sampledWingSpan-> Wingspan2, 
 sampledWingSpan * abs(cos(phi))->  Wingspan2)
 
 
 multiplier = TurbineData$Blades[t] * omega /(2 * pi * sampledbirdSpeed[i])
 
 alpha = sampledbirdSpeed[i]/(radius * omega)
 
 CollideLength_1 = abs(direction * chord * sin(pitch) + alpha * chord * cos(pitch))
 
 CollideLength2 = ifelse(sampledbirdLength[i] > Wingspan2 * alpha,
 
 sampledbirdLength[i] -> CollideLength2,
 
 Wingspan2 * alpha -> CollideLength2)
 
 ifelse(radius == 0, 1,  (multiplier * (CollideLength_1 + CollideLength2)))
 
 }





###### Function 3


pcollxy = function(x,y,updown) {

			r = (x * x + y * y) ^ 0.5

				phi = ifelse ( y == 0,

				ifelse( x >= 0, pi /2 -> phi, -pi/2 -> phi),
			
				atan (x/y) -> phi)

			phi2 = ifelse ( y < 0, phi + pi -> phi2, phi -> phi2)

			phi2 = phi2 * 180 / pi

			pcoll(r,phi2,updown)


}

#### Function 3


xrisksum2 = function (y, xinc,updown) {

		xmax = (1- y * y) ^ 0.5
		imax = as.integer (xmax/xinc)

		risk = (pcollxy (imax * xinc, y, updown) / 2 + pcollxy(xmax, y, updown)/2) * (xmax - imax * xinc)

		risk2 = risk + (pcollxy(0,y,updown) / 2 + pcollxy(imax * xinc, y, updown)/2) * xinc
	
		for (i in 1: (imax - 1)) {
		
			risk2 = risk2 + pcollxy(i * xinc, y, updown) * xinc

					}


	ifelse(imax > 0, 2 * risk2, 2*risk)


	}


