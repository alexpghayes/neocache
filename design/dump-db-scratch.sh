docker run --interactive --tty --rm \
    --publish=7474:7474 --publish=7687:7687 \
    --volume=$HOME/neo4j/data:/data \  # neo4j data volume
    --volume=$HOME/neo4j/backups:/backups \  # neo4j backup volume
    --user="$(id -u):$(id -g)" \
    neo4j:3.5.21 \
    neo4j-admin dump --database=neo4j --to=/backups/test.dump