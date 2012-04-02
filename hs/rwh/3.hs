import Data.Ord (comparing)
import Data.List (sortBy)

-- Compute convex hull using Graham's algorithm
-- FIXME: rewrite with arrays
ch ps = scan $ sortBy (comparing tan) ps'
  where l@(lx, ly) : ps' = sortBy (comparing fst) ps
        tan (x, y) = (y - ly) / (x - lx)
        scan (p : ps) = scan' [p, l, last ps] $ init ps
          where scan' done@(p2 : done'@(p1 : ps)) todo@(p3 : qs) =
                    if ccw p1 p2 p3 > 0 then scan' (p3 : done) qs
                                        else scan' done' todo
                scan' done _ = done

ccw (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
