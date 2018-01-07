-module(asmp).
-compile(export_all).

start() ->
    aerl:start(broadcast),
    aman:init(#{id=> m1, prefs => [w1,w2,w3,w4], partner => -1}),
    aman:init(#{id=> m2, prefs => [w4,w3,w2,w1], partner => -1}),
    aman:init(#{id=> m3, prefs => [w3,w4,w1,w2], partner => -1}),
    aman:init(#{id=> m4, prefs => [w1,w2,w3,w4], partner => -1}),

    awoman:init(#{id => w1, prefs => [m4,m3,m2,m1], partner => -1}),
    awoman:init(#{id => w2, prefs => [m1,m2,m3,m4], partner => -1}),
    awoman:init(#{id => w3, prefs => [m1,m2,m3,m4], partner => -1}),
    awoman:init(#{id => w4, prefs => [m2,m1,m4,m3], partner => -1}).
