# Use an official Python runtime as a parent image
FROM python:3.8-slim

# Set the working directory in the container
WORKDIR /llm

# Copy the current directory contents into the container at /app
COPY . /llm

# Install any needed packages specified in requirements.txt
RUN pip install --trusted-host pypi.python.org -r requirements.txt

# Make port 5000 available to the world outside this container
EXPOSE 5000

# Define environment variable
ENV NAME LLM
# Define HF Token
ENV HUGGINGFACE_CO_TOKEN=''


# Run app.py when the container launches
CMD ["python", "qa.py"]
