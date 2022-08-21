(run "let makea = proc (maker)
       proc (x)
        proc (y)
         proc (z)
          if zero? (x) then -1
          else if zero? (y) then -(0,z)
               else (
                     (((maker maker) x) -(y,1))
                     -(z, ( ( ( (maker maker) -(x,1)) -(x,1)) 0))
                    )
        in let f = proc(k) -(0, ((((makea makea) k) k) 0))
in (f X)")