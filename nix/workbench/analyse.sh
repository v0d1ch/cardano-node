usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    block-propagation RUN-NAME
                          Block propagation analysis for the entire cluster.

    machine-timeline RUN-NAME MACH-NAME
                          Produce a general performance timeline for MACH-NAME

    Options of 'analyse' command:

       --reanalyse        Skip the preparatory steps and launch 'locli' directly
       --time             Time the 'locli' executable runs
EOF
}

analyse() {
local skip_preparation= time=
while test $# -gt 0
do case "$1" in
       --reanalyse | --re ) skip_preparation='true';;
       --time )             time='eval time';;
       * ) break;; esac; shift; done

local op=${1:-$(usage_analyse)}; shift

case "$op" in
    block-propagation | bp )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"

        local name=${1:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"
        cat >>"$keyfile" <<EOF
TraceForgedBlock
AddedToCurrentChain
TraceChainSyncServerReadBlocked.AddBlock
TraceChainSyncServerRead.AddBlock
TraceBlockFetchServerSendBlock
TraceDownloadedHeader
CompletedBlockFetch
EOF

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logdirs=("$dir"/node-*/)

        if test -z "$skip_preparation" -a -n "$(ls "$adir"/logs-node-*.flt.json)"
        then
            msg "filtering logs in: $dir/node-* "
            local jq_args=(
                --sort-keys
                --compact-output
                $(wb backend lostream-fixup-jqargs "$dir")
                ' delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])
                '"$(wb backend lostream-fixup-jqexpr)"
            )
            for d in "${logdirs[@]}"
            do ## TODO: supervisor-specific logfile layout
                grep -hFf "$keyfile" $(ls "$d"/stdout* | tac) | jq "${jq_args[@]}" > \
                "$adir"/logs-$(basename "$d").flt.json &
            done
            wait
        fi

        msg "log sizes:  (files: $(ls "$adir"/*.flt.json | wc -l), lines: $(cat "$adir"/*.flt.json | wc -l))"

        msg "analysing.."
        local locli_args=(
            --genesis         "$dir"/genesis.json
            --run-metafile    "$dir"/meta.json
            ## ->
            # --logobjects-json "$adir"/logs-cluster.logobjects.json
            --analysis-json   "$adir"/block-event-stream.json
        )

        ${time} locli 'analyse' 'block-propagation' \
            "${locli_args[@]}" "$adir"/*.flt.json;;

    machine-timeline | machine | mt )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logs=("$dir"/$mach/stdout) consolidated="$adir"/logs-$mach.json
        if test -z "$skip_preparation" -a -n "$(ls "$adir"/logs-$mach.json)"
        then
            grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"
        fi

        msg "analysing logs of:  $mach  (lines: $(wc -l "$consolidated"))"
        local locli_args=(
            --genesis         "$dir"/genesis.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --logobjects-json "$adir"/logs-$mach.logobjects.json
            --slotstats-json  "$adir"/logs-$mach.slotstats.json
            --timeline-pretty "$adir"/logs-$mach.timeline.txt
            --stats-csv       "$adir"/logs-$mach.stats.csv
            --analysis-json   "$adir"/logs-$mach.analysis.json
            # --timeline-csv            "$adir"/logs-$mach.timeline.csv
            # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
            # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
            # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
        )

        ${time} locli 'analyse' 'machine-timeline' \
            "${locli_args[@]}" "$consolidated";;

    * ) usage_analyse;; esac
}
