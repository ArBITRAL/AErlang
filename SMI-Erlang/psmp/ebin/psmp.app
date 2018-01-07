{application,psmp,
             [{description,"SMTI in Pure Erlang."},
              {vsn,"0.1"},
              {modules,[myring,pman,psmp,psmp_app,pwoman,read,stable]},
              {registered,[psmp_app]},
              {applications,[kernel,stdlib]},
              {env,[{size,1000},{run,50}]},
              {mod,{psmp_app,[]}}]}.
