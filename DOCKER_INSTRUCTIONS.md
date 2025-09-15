# Docker Instructions

This document provides instructions for building and publishing the Docker image used to export Org mode files to HTML.

## Building the Docker Image

To build the Docker image, navigate to the `docker` directory and run the following command:

```bash
docker build -t org-exporter .
```

This will build the image and tag it as `org-exporter`.

## Using the Docker Image

To export an Org mode file to HTML, you can use the following command:

```bash
docker run --rm -v "$(pwd)":/workspace org-exporter your-file.org
```

Replace `your-file.org` with the path to your Org mode file. The exported HTML file will be created in the same directory.

### With Custom CSS

You can also specify a custom CSS file to be embedded in the exported HTML. The CSS file must be present in the `docker/` directory of the image. The `style.css` file is included by default. To use it, you can run:

```bash
docker run --rm -v "$(pwd)":/workspace org-exporter your-file.org --css style
```

## Publishing the Docker Image

You can publish the Docker image to a container registry like Docker Hub or GitHub Container Registry.

### Docker Hub

1.  **Log in to Docker Hub:**
    ```bash
    docker login
    ```

2.  **Tag the image:**
    ```bash
    docker tag org-exporter <your-dockerhub-username>/org-exporter:latest
    ```
    Replace `<your-dockerhub-username>` with your Docker Hub username.

3.  **Push the image:**
    ```bash
    docker push <your-dockerhub-username>/org-exporter:latest
    ```

### GitHub Container Registry

1.  **Log in to GitHub Container Registry:**
    ```bash
    echo $CR_PAT | docker login ghcr.io -u <your-github-username> --password-stdin
    ```
    Replace `<your-github-username>` with your GitHub username. You will need to create a Personal Access Token (PAT) with the `write:packages` scope and set it as the `CR_PAT` environment variable.

2.  **Tag the image:**
    ```bash
    docker tag org-exporter ghcr.io/<your-github-username>/org-exporter:latest
    ```
    Replace `<your-github-username>` with your GitHub username.

3.  **Push the image:**
    ```bash
    docker push ghcr.io/<your-github-username>/org-exporter:latest
    ```

### GitLab Pages

This repository includes a `.gitlab-ci.yml` file that configures GitLab CI/CD to automatically publish the `documentation.org` file to GitLab Pages.

To use this:

1.  **Push your repository to GitLab.**
2.  **Ensure that GitLab CI/CD is enabled for your project.** You can check this in your project's `Settings > CI/CD`.
3.  **Push to the `main` branch.** The pipeline is configured to run on every push to the `main` branch.

The pipeline will then automatically:
- Build the `org-exporter` Docker image.
- Run the image to convert `documentation.org` to `documentation.html`.
- Move the resulting file to `public/index.html`.
- Deploy the `public` directory to GitLab Pages.

Your documentation will be available at `https://<your-gitlab-username>.gitlab.io/<your-project-name>`.
