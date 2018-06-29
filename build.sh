set -xe

${coreutils}/bin/mkdir -p ${out}/static
${coreutils}/bin/cp ${frontend}/bin/slownews-frontend.jsexe/* ${out}/static/
${coreutils}/bin/cp ${backend}/bin/slownews-backend ${out}/
${coreutils}/bin/cp ${slownewsConfig}/* ${out}/
