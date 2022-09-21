cardano-node run \
  --config $PWD/node/config.json \
  --database-path $PWD/node/db/ \
  --socket-path $PWD/node/node.socket \
  --port 3001 \
  --topology $PWD/node/topology.json
