FROM rust
WORKDIR /usr/src/app
COPY Cargo.toml Cargo.lock ./
RUN mkdir src && echo "fn main () {}" > src/main.rs && echo "fn main () {}" > src/buildkit_frontend.rs
RUN cargo build --release
COPY . .
RUN cargo build --release
ENTRYPOINT [ "sh", "-c", "target/release/buildkit-frontend" ]
