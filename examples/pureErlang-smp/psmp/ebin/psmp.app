{application,psmp,
             [{description,"SMTI in Pure Erlang."},
              {vsn,"0.1"},
              {modules,[foo,pman,psmp,psmp_app,pwoman,read,stable]},
              {registered,[psmp_app]},
              {applications,[kernel,stdlib]},
              {env,[{type,noties},{size,1000},{run,10}]},
              {mod,{psmp_app,[]}}]}.
