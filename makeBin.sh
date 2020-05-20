lein uberjar
echo "#! /usr/bin/java -jar" > almas
cat target/uberjar/chess-0.1.0-SNAPSHOT-standalone.jar >> almas
chmod +x almas 
