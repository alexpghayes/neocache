# return TRUE for success, FALSE for failure

# update: these should all use the stevedore package
# https://github.com/richfitz/stevedore

success <- function(sys_code) {
  sys_code == 0
}

remove_docker_container <- function(container_name) {
  code <- system(glue("docker container rm {container_name}"), ignore.stdout = TRUE)
  success(code)
}

docker_container_exists <- function(container_name) {
  code <- system("docker container ls --all", intern = TRUE)
  any(endsWith(code, container_name))
}

is_docker_container_running <- function(container_name) {
  code <- system("docker container ls", intern = TRUE)
  any(endsWith(code, container_name))
}

find_docker <- function() {
  code <- system("docker system info", ignore.stdout = TRUE, ignore.stderr = TRUE)
  success(code)
}

start_docker <- function(container_name) {
  code <- system(glue("docker start {container_name}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  success(code)
}

create_docker_container <- function(container_name, neo4j_pass, http_port, bolt_port) {
  code <- system(glue(
    "docker create --name {container_name} -p{http_port}:{http_port} -p{bolt_port}:{bolt_port} -e NEO4J_AUTH=neo4j/{neo4j_pass} -e NEO4J_apoc_export_file_enabled=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4JLABS_PLUGINS=[\\\"apoc\\\"] -e NEO4J_dbms_connector_http_advertised__address=:{http_port} -e NEO4J_dbms_connector_http_listen__address=:{http_port} -e NEO4J_dbms_connector_bolt_advertised__address=:{bolt_port} -e NEO4J_dbms_connector_bolt_listen__address=:{bolt_port} neo4j:3.5.21"
  ),
  ignore.stdout = TRUE
  )
  success(code)
}

stop_docker <- function(container_name) {
  code <- system(glue("docker stop {container_name}"), ignore.stdout = TRUE)
  success(code)
}

copy_csv_to_docker <- function(local_path, output_name, container_name) {
  code <- system(glue("docker cp {local_path} {container_name}:/var/lib/neo4j/import/{output_name}"))
  success(code)
}

copy_csv_from_docker <- function(file_name, local_path, container_name) {
  code <- system(glue("docker cp {container_name}:/var/lib/neo4j/import/{file_name} {local_path}"))
  success(code)
}

remove_file_in_docker_container <- function(file_name, container_name) {
  code <- system(glue("docker exec {container_name} rm -rf /var/lib/neo4j/import/{file_name}"))
  success(code)
}

