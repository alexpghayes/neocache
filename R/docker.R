# return TRUE for success, FALSE for failure

success <- function(sys_code) {
  sys_code == 0
}

find_docker <- function() {
  code <- system("docker system info", ignore.stdout = TRUE, ignore.stderr = TRUE)
  success(code)
}

start_neocache_docker <- function() {
  code <- system("docker start neocache_docker", ignore.stdout = TRUE, ignore.stderr = TRUE)
  success(code)
}

create_neocache_docker <- function() {
  code <- system(
    "docker run --name neocache_docker -p7474:7474 -p7687:7687 -d -e NEO4J_AUTH=neo4j/pass -e NEO4J_apoc_export_file_enabled=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4JLABS_PLUGINS=[\\\"apoc\\\"] neo4j:3.5.21",
    ignore.stdout = TRUE
  )
  success(code)
}

stop_neocache_docker <- function() {
  code <- system("docker stop neocache_docker", ignore.stdout = TRUE)
  success(code)
}

copy_csv_to_docker <- function(path) {
  code <- system(glue("docker cp {path} neocache_docker:/var/lib/neo4j/import/data.csv"))
  success(code)
}

pull_friend_data_from_docker <- function(path) {
  code <- system(glue("docker cp neocache_docker:/var/lib/neo4j/import/get_friends.csv {tmp}"))
  success(code)
}
