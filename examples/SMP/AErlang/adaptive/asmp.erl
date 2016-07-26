-module(asmp).
-compile(export_all).

start() ->
    aerl:start(broadcast),

    awoman:init(#{id => w1, sex => woman, eyes => blue, hair => red, prefs => #{wealth => poor, body => weak}, partner => -1, wpartner => -1, ex => -1}),
    awoman:init(#{id => w2, sex => woman, eyes => blue, hair => dark, prefs => #{wealth => rich, body => strong}, partner => -1, param => -3,  wpartner => -1, params => {-1, -1}, ex => -1}),
    awoman:init(#{id => w3, sex => woman, eyes => green, hair => red, prefs => #{wealth => rich, body => strong}, partner => -1, param => -3, wpartner => -1, params => {-1, -1}, ex => -1}),
    awoman:init(#{id => w4, sex => woman, eyes => green, hair => dark, prefs => #{wealth => rich, body => weak}, partner => -1, param => -3, wpartner => -1, params => {-1, -1}, ex => -1}),

    aman:init(#{id=> m2, sex => man, wealth => rich, body => weak, predicate => ["eyes = this.feyes and hair = this.fhair", "eyes = this.feyes", "hair = this.fhair", "sex = woman"], feyes => blue, fhair => red, refusal => [] , partner => -1}),
    aman:init(#{id=> m3, sex => man, wealth => poor, body => strong, predicate => ["eyes = this.feyes and hair = this.fhair", "eyes = this.feyes", "hair = this.fhair", "sex = woman"], feyes => blue, fhair => red, refusal => [], partner => -1}),
    aman:init(#{id=> m4, sex => man, wealth => poor, body => weak, feyes => blue, fhair => red, predicate => ["eyes = this.feyes and hair = this.fhair", "eyes = this.feyes", "hair = this.fhair", "sex = woman"], refusal => [], partner => -1}),

    aman:init(#{id=> m1, sex => man, wealth => rich, body => strong, predicate => ["eyes = this.feyes and hair = this.fhair", "eyes = this.feyes", "hair = this.fhair", "sex = woman"],feyes => blue, fhair => red, refusal => [], partner => -1}).
