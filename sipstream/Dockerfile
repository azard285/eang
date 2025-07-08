FROM erlang:21

WORKDIR /app

# Установка ffmpeg и других зависимостей
RUN apt-get update && apt-get install -y \
    ffmpeg \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY rebar.config .
COPY rebar.lock .
RUN rebar3 get-deps

COPY . .
RUN rebar3 compile

CMD ["rebar3", "shell"]