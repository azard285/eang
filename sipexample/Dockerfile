# Build stage 0
FROM erlang:21

# Обновляем источники пакетов для старого Debian
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's|security.debian.org|archive.debian.org/|g' /etc/apt/sources.list && \
    sed -i '/buster-updates/d' /etc/apt/sources.list

# Установка зависимостей
RUN apt-get update && apt-get install -y \
    ffmpeg \
    net-tools \
    netcat-openbsd \
    wget

# Установка rebar3 (версия 3.14.0 для совместимости с OTP 21)
RUN wget https://github.com/erlang/rebar3/releases/download/3.14.0/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Установка рабочей директории
WORKDIR /buildroot

# Копирование приложения
COPY sipstream sipstream

# Компиляция
WORKDIR sipstream
#RUN rebar3 compile

# Открытие портов
EXPOSE 5061/udp 4000-4010/udp

CMD ["/bin/bash"]