FROM openjdk:17-slim

# Set the working directory in the container
WORKDIR /app

# Copy the JAR file to the container
COPY scala-compare.jar /app/scala-compare.jar

# Run the application
ENTRYPOINT ["java", "-jar", "/app/scala-compare.jar"]
