# Python + Quarto for GitHub Codespaces
FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    git \
    vim \
    build-essential \
    ca-certificates \
    gdebi-core \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto
ARG QUARTO_VERSION=1.8.24

RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && gdebi -n quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm quarto-${QUARTO_VERSION}-linux-amd64.deb

# Set working directory
WORKDIR /workspace

# Upgrade pip
RUN python -m pip install --upgrade pip

# Install common Python packages for Quarto
RUN pip install \
    jupyter \
    ipykernel \
    notebook \
    matplotlib \
    pandas \
    numpy

# Install project dependencies if present
COPY requirements.txt* ./
RUN if [ -f requirements.txt ]; then pip install -r requirements.txt; fi

# Copy project
COPY . .

# Verify Quarto installation
RUN quarto check

CMD ["bash"]