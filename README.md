# COMICS

Download app on [Docker Hub](https://hub.docker.com/r/ypopov/comics/)

Remove all old images:
sudo docker rmi -f $(docker images | grep "<none>" | awk '{print $3}')
