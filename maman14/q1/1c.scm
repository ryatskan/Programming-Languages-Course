(run 
 "let p = proc(x)
                 if zero? (x) then 
                    0
                    else if zero?
                            (-(x,1)) then 1
                            else  -( (p -(x,1)), -( 0, (p -(x,2))))
             in (p REPLACE_ME)")