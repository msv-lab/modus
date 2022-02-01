# Executing Integration Tests

To run integration tests, use the following command:

    cd test
    python -m unittest discover
    
Use the environment variable `MODUS_EXECUTABLE` to specify the modus executable (`modus` by default), and `MODUS_BUILDKIT_FRONTEND` to specify custom BuildKit frontend (`None` by default).
