(run "let p = proc (x) proc (y)
if zero? (-(-(x, y),y))
	then zero? (0)
	else if zero? (-(-(y, x),x))
		then zero? (0)
		else zero? (1)
in ((p X) Y)")