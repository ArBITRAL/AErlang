#!/bin/bash
dirs=(asmp/priv/size-500/*/)
for size in `seq  500 100 500`; do
    echo "msg" "size" "stability" "time" > ../SMP-Pushing/log$size
    mkdir -p ../SMP-Pushing/asmp/priv/size-$size
    for dir in "${dirs[@]}"
    do
	name=`basename "$dir"`
	sed -i '.bal' "s/.*{env.*/{env,[{type,${name}},{size,$size},{run,1}]},/" ../SMP-Pushing/asmp/src/asmp.app.src
	./measure.sh "$name" $size
    done
done
