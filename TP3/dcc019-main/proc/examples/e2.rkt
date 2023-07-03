#lang dcc019/proc

   let makerec = proc (f)
                   let d = proc (x)
                     proc (z) ((f (x x)) z)
               in proc (n) ((f (d d)) n)
   in let maketimes4 = proc (f)
                         proc (x)
               if zero?(x) then 0 else -((f -(x,1)), -(0,4))
      in let times4 = (makerec maketimes4)
         in (times4 3)

  let makesum = proc (f)
                 let d = proc (x)
                   proc (z) ((f (x x)) z)
             in proc (n) if zero?(n) then 0 else +((f (d d)), +(n, (f (d d)) -(n, 1)))
in let sum = (makesum makesum)
   in (sum 3)