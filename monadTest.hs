test1 m = (return 13 >>= m) == (m 13)
test2 m = (m 13 >>= return) == (m 13)
test3 m = ((target >>= p1) >>= m2) == (target >>= (\x -> (p1 x) >>= m2))
  where target = m 13
        p1 = (\x -> return (x+1))
        m2 = (\x -> return (x-2))