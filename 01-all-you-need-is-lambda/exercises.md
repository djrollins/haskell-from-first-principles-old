# 1) Combinators
## Combinators
    \x.xxx
    \xyz.xy(zx)
    \xyz.xy(zxy)
## Not
    \xy.zx
    \xy.xy(zxy)


# 2) Normal or diverged?

## Normal
    \x.xxx
    (\x.xxx)z -> zzz
## Diverged
    (\z.zz)(\y.yy)


# 4) Beta reduce

1. (\abc.cba)zz(\wv.w)

[curry         ] : (\a.\b.\c.cba)zz(\w.\v.w)
[a := z        ] : (\b.\c.cbz)z(\w.\v.w)
[b := z        ] : (\c.czz)(\w.\v.w)
[c := (\w.\v.w)] : (\w.\v.w)zz
[w := z        ] : (\v.z)z
[v := z        ] : z

2. (\x.\y.xyy)(\a.a)b

[x := (\a.a)] : (\y.(\a.a)yy)b
[y := b]      : (\a.a)bb
[a := b]      : bb

3. (\y.y)(\x.xx)(\z.zq)

[y := (\x.xx)] : (\x.xx)(\z.zq)
[x := (\z.zq)] : (\z.zq)(\z.zq)
[z := (\z.zq)] : (\z.zq)q
[z := q]       : qq

4. (\z.z)(\z.zz)(\z.zy)

[rename]       : (\x.x)(\a.aa)(\z.zy)
[x := (\a.aa)] : (\a.aa)(\z.zy)
[y := (\z.zy)] : (\z.zy)(\z.zy)
[z := (\z.zy)] : (\z.zy)y
[z := y      ] : yy

5. (\x.\y.xyy)(\y.y)y

[x := (\y'.y')] : (\y.(\y'.y')xx)y
[y := y       ] : (\y'.y')yy
[y':= y       ] : yy

6. (\a.aa)(\b.ba)c

[a := (\b.ba)] : (\b.ba)(\b.ba)c
[b := (\b.ba)] : ((\b.ba)a)c
[b := a      ] : aac

7. (\xyz'.xz'(yz'))(\x.z)(\x.a)

[x := (\x.z)] : (\yz.(\x.z)z'(yz'))(\x.a)
[y := (\x.a)] : \z'.(\x.z)z'((\x.a)z')
[x := z'    ] : \z'.z((\x.a)z')
[x := z'    ] : \z'.za
