H(x, y) = 0.07 * (x**2 + y**2) - 1.017 * (sqrt(x**2 + 15.103) + sqrt(y**2 + 15.103)) - 0.00656*x*y + 12.065
set xrange [-15:15]
set yrange [-15:15]
set size ratio -1
set samples 512
set isosamples 512
set contour base
set cntrparam levels incremental 6,0.01,20
unset key
unset xtics
unset ytics
unset border
unset surface
set table 'curve1.dat'
splot H(x,y)
unset table
plot 'curve1.dat' w l lw 1.5 lc rgb "red"
