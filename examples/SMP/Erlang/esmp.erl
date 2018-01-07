-module(esmp).
-compile(export_all).

start() ->
    eman:init(#{id=> m1, prefs => [w1,w2,w3,w4], partner => -1}),
    eman:init(#{id=> m2, prefs => [w4,w3,w2,w1], partner => -1}),
    eman:init(#{id=> m3, prefs => [w3,w4,w1,w2], partner => -1}),
    eman:init(#{id=> m4, prefs => [w1,w2,w3,w4], partner => -1}),

    ewoman:init(#{id => w1, prefs => [m4,m3,m2,m1], partner => -1, ex => -1}),
    ewoman:init(#{id => w2, prefs => [m1,m2,m3,m4], partner => -1, ex => -1}),
    ewoman:init(#{id => w3, prefs => [m1,m2,m3,m4], partner => -1, ex => -1}),
    ewoman:init(#{id => w4, prefs => [m2,m1,m4,m3], partner => -1, ex => -1}).
