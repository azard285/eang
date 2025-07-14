# Build stage 0
FROM erlang:21

# Обновляем источники пакетов для старого Debian
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's|security.debian.org|archive.debian.org/|g' /etc/apt/sources.list && \
    sed -i '/buster-updates/d' /etc/apt/sources.list

RUN apt-get update && apt-get install -y openssh-server \
    ffmpeg \
    net-tools
RUN mkdir /var/run/sshd
RUN useradd -m -s /bin/bash sipuser
RUN echo "sipuser:password" | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN echo "Port 22" >> /etc/ssh/sshd_config
EXPOSE 22

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy Erlang application
COPY sipstream sipstream

# Build
WORKDIR sipstream
#RUN make pbx

EXPOSE 5061/udp
EXPOSE 4000-4010/udp

CMD ["/bin/bash"]
